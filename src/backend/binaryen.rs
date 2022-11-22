//! Binaryen bindings.

use crate::entity::EntityRef;
use crate::ir;
use crate::{Ieee32, Ieee64};
use anyhow::{bail, Result};
use lazy_static::lazy_static;
use libc::{c_char, c_void};
use std::ffi::{CStr, CString};

#[derive(Debug)]
pub struct Module(BinaryenModule);
#[derive(Clone, Copy, Debug)]
pub struct Function(BinaryenModule, BinaryenFunction);
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Expression(BinaryenModule, BinaryenExpression);
#[derive(Clone, Copy, Debug)]
pub struct Export(BinaryenModule, BinaryenExport);

pub type BinaryenIndex = u32;
pub type BinaryenType = usize;

impl Module {
    pub fn read(data: &[u8]) -> Result<Module> {
        let ptr = unsafe { BinaryenModuleRead(data.as_ptr(), data.len()) };
        if ptr.is_null() {
            bail!("Failed to parse module");
        }
        Ok(Module(ptr))
    }

    pub fn write(&self) -> Result<Vec<u8>> {
        let result = unsafe { BinaryenModuleAllocateAndWrite(self.0, std::ptr::null()) };
        if result.binary.is_null() {
            bail!("Failed to serialize module");
        }
        let slice = unsafe {
            std::slice::from_raw_parts(
                result.binary as *const c_void as *const u8,
                result.binary_bytes as usize,
            )
        };
        Ok(slice.to_vec())
    }

    pub fn new() -> Result<Module> {
        let ptr = unsafe { BinaryenModuleCreate() };
        if ptr.is_null() {
            bail!("Failed to allocate module");
        }
        Ok(Module(ptr))
    }

    pub fn num_funcs(&self) -> usize {
        unsafe { BinaryenGetNumFunctions(self.0) as usize }
    }

    pub fn func(&self, index: usize) -> Function {
        assert!(index < self.num_funcs());
        let ptr = unsafe { BinaryenGetFunctionByIndex(self.0, index as u32) };
        assert!(!ptr.is_null());
        Function(self.0, ptr)
    }

    pub fn func_by_name(&self, name: &str) -> Option<Function> {
        let c_str = CString::new(name).unwrap();
        let ptr = unsafe { BinaryenGetFunction(self.0, c_str.as_ptr()) };
        if !ptr.is_null() {
            Some(Function(self.0, ptr))
        } else {
            None
        }
    }

    pub fn num_exports(&self) -> usize {
        unsafe { BinaryenGetNumExports(self.0) as usize }
    }

    pub fn export_by_name(&self, name: &str) -> Option<Export> {
        let c_str = CString::new(name).unwrap();
        let ptr = unsafe { BinaryenGetExport(self.0, c_str.as_ptr()) };
        if !ptr.is_null() {
            Some(Export(self.0, ptr))
        } else {
            None
        }
    }

    pub fn export(&self, index: usize) -> Export {
        assert!(index < self.num_exports());
        let ptr = unsafe { BinaryenGetExportByIndex(self.0, index as u32) };
        assert!(!ptr.is_null());
        Export(self.0, ptr)
    }

    pub fn module(&self) -> BinaryenModule {
        self.0
    }

    pub fn add_global(&self, ty: ir::Type, mutable: bool, value: Option<u64>) -> ir::Global {
        let b_ty = Type::from(ty).to_binaryen();
        let value = value.unwrap_or(0);
        let init = match ty {
            ir::Type::I32 => Expression::const_i32(self, value as i32),
            ir::Type::I64 => Expression::const_i64(self, value as i64),
            ir::Type::F32 => Expression::const_f32(self, Ieee32::from_bits(value as u32)),
            ir::Type::F64 => Expression::const_f64(self, Ieee64::from_bits(value)),
            _ => panic!("Unsupported type"),
        };

        let num = unsafe { BinaryenGetNumGlobals(self.0) };
        let global = unsafe { BinaryenAddGlobal(self.0, std::ptr::null(), b_ty, mutable, init.1) };
        assert!(!global.is_null());
        ir::Global::from(num)
    }

    pub fn add_table(&self, ty: ir::Type, init: usize, max: Option<u32>) -> ir::Table {
        let ty = Type::from(ty).to_binaryen();
        let num = unsafe { BinaryenGetNumTables(self.0) };
        let max = max.unwrap_or(0);
        let table = unsafe {
            BinaryenAddTable(
                self.0,
                std::ptr::null(),
                init as BinaryenIndex,
                max as BinaryenIndex,
                ty,
            )
        };
        assert!(!table.is_null());
        ir::Table::from(num)
    }

    pub fn add_table_elem(&self, table: ir::Table, index: usize, elt: ir::Func) {
        let table_name = unsafe {
            BinaryenTableGetName(BinaryenGetTableByIndex(
                self.0,
                table.index() as BinaryenIndex,
            ))
        };
        let func_name = unsafe {
            BinaryenFunctionGetName(BinaryenGetFunctionByIndex(
                self.0,
                elt.index() as BinaryenIndex,
            ))
        };
        let offset = Expression::const_i32(self, index as i32);
        let seg = unsafe {
            BinaryenAddActiveElementSegment(
                self.0,
                table_name,
                std::ptr::null(),
                &func_name as *const *const c_char,
                1,
                offset.1,
            )
        };
        assert!(!seg.is_null());
    }

    pub fn add_mem(
        &self,
        init_pages: usize,
        max_pages: Option<usize>,
        segments: &[ir::MemorySegment],
    ) -> ir::Memory {
        let seg_passive = vec![false; segments.len()];
        let seg_offset = segments
            .iter()
            .map(|seg| Expression::const_i32(self, seg.offset as i32).1)
            .collect::<Vec<_>>();
        let seg_data = segments
            .iter()
            .map(|seg| seg.data.as_ptr() as *const c_char)
            .collect::<Vec<_>>();
        let seg_size = segments
            .iter()
            .map(|seg| seg.data.len() as BinaryenIndex)
            .collect::<Vec<_>>();

        // Binaryen does not support multi-memory.
        unsafe {
            BinaryenSetMemory(
                self.0,
                init_pages as BinaryenIndex,
                max_pages.unwrap_or(0) as BinaryenIndex,
                std::ptr::null(),
                seg_data.as_ptr(),
                seg_passive.as_ptr(),
                seg_offset.as_ptr(),
                seg_size.as_ptr(),
                segments.len() as BinaryenIndex,
                false,
            );
        }
        ir::Memory::from(0)
    }

    pub fn add_table_import(&self, table: ir::Table, module: &str, name: &str) {
        let table_name = unsafe {
            BinaryenTableGetName(BinaryenGetTableByIndex(
                self.0,
                table.index() as BinaryenIndex,
            ))
        };
        let c_module = std::ffi::CString::new(module).unwrap();
        let c_name = std::ffi::CString::new(name).unwrap();
        unsafe {
            BinaryenAddTableImport(self.0, table_name, c_module.as_ptr(), c_name.as_ptr());
        }
    }

    pub fn add_func_import(
        &self,
        func: ir::Func,
        module: &str,
        name: &str,
        params: &[ir::Type],
        results: &[ir::Type],
    ) {
        let func_name = unsafe {
            BinaryenFunctionGetName(BinaryenGetFunctionByIndex(
                self.0,
                func.index() as BinaryenIndex,
            ))
        };
        let c_module = std::ffi::CString::new(module).unwrap();
        let c_name = std::ffi::CString::new(name).unwrap();
        let params = tys_to_binaryen(params.iter().copied());
        let results = tys_to_binaryen(results.iter().copied());
        unsafe {
            BinaryenAddFunctionImport(
                self.0,
                func_name,
                c_module.as_ptr(),
                c_name.as_ptr(),
                params,
                results,
            );
        }
    }

    pub fn add_global_import(
        &self,
        global: ir::Global,
        module: &str,
        name: &str,
        ty: ir::Type,
        mutable: bool,
    ) {
        let global_name = unsafe {
            BinaryenGlobalGetName(BinaryenGetGlobalByIndex(
                self.0,
                global.index() as BinaryenIndex,
            ))
        };
        let c_module = std::ffi::CString::new(module).unwrap();
        let c_name = std::ffi::CString::new(name).unwrap();
        let ty = Type::from(ty).to_binaryen();
        unsafe {
            BinaryenAddGlobalImport(
                self.0,
                global_name,
                c_module.as_ptr(),
                c_name.as_ptr(),
                ty,
                mutable,
            );
        }
    }
}

impl Drop for Module {
    fn drop(&mut self) {
        unsafe {
            BinaryenModuleDispose(self.0);
        }
    }
}

impl Function {
    pub fn body(&self) -> Option<Expression> {
        let body = unsafe { BinaryenFunctionGetBody(self.1) };
        if body.is_null() {
            None
        } else {
            Some(Expression(self.0, body))
        }
    }

    pub fn set_body(&mut self, body: Expression) {
        unsafe {
            BinaryenFunctionSetBody(self.0, body.1);
        }
    }

    pub fn name(&self) -> &str {
        let s = unsafe { CStr::from_ptr(BinaryenFunctionGetName(self.1)) };
        s.to_str().unwrap()
    }

    pub fn create(
        module: &mut Module,
        params: impl Iterator<Item = ir::Type>,
        results: impl Iterator<Item = ir::Type>,
        locals: impl Iterator<Item = ir::Type>,
        body: Expression,
    ) -> Function {
        let params = tys_to_binaryen(params);
        let results = tys_to_binaryen(results);
        let locals: Vec<BinaryenType> = locals.map(|ty| Type::from(ty).to_binaryen()).collect();
        let ptr = unsafe {
            BinaryenAddFunc(
                module.0,
                /* name = */ std::ptr::null(),
                params,
                results,
                locals.as_ptr(),
                locals.len() as BinaryenIndex,
                body.1,
            )
        };
        Function(module.0, ptr)
    }

    pub fn add_local(&mut self, ty: ir::Type) -> usize {
        (unsafe { BinaryenFunctionAddVar(self.1, Type::from(ty).to_binaryen()) }) as usize
    }
}

impl Export {
    pub fn name(&self) -> &str {
        let s = unsafe { CStr::from_ptr(BinaryenExportGetName(self.1)) };
        s.to_str().unwrap()
    }

    pub fn value(&self) -> &str {
        let s = unsafe { CStr::from_ptr(BinaryenExportGetValue(self.1)) };
        s.to_str().unwrap()
    }

    pub fn into_function(&self, module: &Module) -> Option<Function> {
        let kind = unsafe { BinaryenExportGetKind(self.1) };
        if kind == unsafe { BinaryenExternalFunction() } {
            let name = self.value();
            module.func_by_name(name)
        } else {
            None
        }
    }
}

struct TypeIds {
    none_t: BinaryenType,
    i32_t: BinaryenType,
    i64_t: BinaryenType,
    f32_t: BinaryenType,
    f64_t: BinaryenType,
    v128_t: BinaryenType,
}

impl TypeIds {
    fn get() -> Self {
        TypeIds {
            none_t: unsafe { BinaryenTypeNone() },
            i32_t: unsafe { BinaryenTypeInt32() },
            i64_t: unsafe { BinaryenTypeInt64() },
            f32_t: unsafe { BinaryenTypeFloat32() },
            f64_t: unsafe { BinaryenTypeFloat64() },
            v128_t: unsafe { BinaryenTypeVec128() },
        }
    }
}

lazy_static! {
    static ref TYPE_IDS: TypeIds = TypeIds::get();
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    None,
    I32,
    I64,
    F32,
    F64,
    V128,
}

impl Type {
    fn from_binaryen(kind: BinaryenType) -> Option<Type> {
        let tys = &*TYPE_IDS;
        if kind == tys.none_t {
            Some(Type::None)
        } else if kind == tys.i32_t {
            Some(Type::I32)
        } else if kind == tys.i64_t {
            Some(Type::I64)
        } else if kind == tys.f32_t {
            Some(Type::F32)
        } else if kind == tys.f64_t {
            Some(Type::F64)
        } else {
            None
        }
    }

    pub(crate) fn to_binaryen(&self) -> BinaryenType {
        let tys = &*TYPE_IDS;
        match self {
            &Type::None => tys.none_t,
            &Type::I32 => tys.i32_t,
            &Type::I64 => tys.i64_t,
            &Type::F32 => tys.f32_t,
            &Type::F64 => tys.f64_t,
            &Type::V128 => tys.v128_t,
        }
    }
}

impl From<ir::Type> for Type {
    fn from(ty: ir::Type) -> Self {
        match ty {
            ir::Type::I32 => Type::I32,
            ir::Type::I64 => Type::I64,
            ir::Type::F32 => Type::F32,
            ir::Type::F64 => Type::F64,
            ir::Type::V128 => Type::V128,
            _ => unimplemented!(),
        }
    }
}

pub fn tys_to_binaryen(tys: impl Iterator<Item = ir::Type>) -> BinaryenType {
    let tys: Vec<BinaryenType> = tys.map(|ty| Type::from(ty).to_binaryen()).collect();
    unsafe { BinaryenTypeCreate(tys.as_ptr(), tys.len() as BinaryenIndex) }
}

fn name_to_string(name: *const c_char) -> Option<String> {
    if name.is_null() {
        None
    } else {
        Some(unsafe { CStr::from_ptr(name).to_str().unwrap().to_string() })
    }
}

impl Expression {
    pub fn module(&self) -> BinaryenModule {
        self.0
    }

    pub fn block(module: &Module, exprs: &[Expression]) -> Expression {
        let children = exprs.iter().map(|expr| expr.1).collect::<Vec<_>>();
        Expression(module.0, unsafe {
            BinaryenBlock(
                module.0,
                /* name = */ std::ptr::null(),
                children.as_ptr(),
                children.len() as BinaryenIndex,
                BinaryenUndefined(),
            )
        })
    }

    pub fn block_append_child(&mut self, child: Expression) {
        unsafe {
            BinaryenBlockAppendChild(self.1, child.1);
        }
    }

    pub fn nop(module: &Module) -> Expression {
        Expression(module.0, unsafe { BinaryenNop(module.0) })
    }
    pub fn unreachable(module: &Module) -> Expression {
        Expression(module.0, unsafe { BinaryenUnreachable(module.0) })
    }
    pub fn call(
        module: &Module,
        func: ir::Func,
        args: &[Expression],
        tys: &[ir::Type],
    ) -> Expression {
        // Look up the function's name.
        let func_name = unsafe {
            BinaryenFunctionGetName(BinaryenGetFunctionByIndex(
                module.0,
                func.index() as BinaryenIndex,
            ))
        };
        // Create the appropriate type for return.
        let ret_tuple_ty = tys_to_binaryen(tys.iter().copied());
        let args = args.iter().map(|expr| expr.1).collect::<Vec<_>>();
        let expr = unsafe {
            BinaryenCall(
                module.0,
                func_name,
                args.as_ptr(),
                args.len() as BinaryenIndex,
                ret_tuple_ty,
            )
        };
        Expression(module.0, expr)
    }
    pub fn call_indirect(
        module: &Module,
        table: ir::Table,
        sig: &ir::SignatureData,
        target: Expression,
        args: &[Expression],
    ) -> Expression {
        let param_tuple_ty = tys_to_binaryen(sig.params.iter().copied());
        let ret_tuple_ty = tys_to_binaryen(sig.returns.iter().copied());
        let args = args.iter().map(|expr| expr.1).collect::<Vec<_>>();
        let table_name = unsafe {
            BinaryenTableGetName(BinaryenGetTableByIndex(
                module.0,
                table.index() as BinaryenIndex,
            ))
        };
        let expr = unsafe {
            BinaryenCallIndirect(
                module.0,
                table_name,
                target.1,
                args.as_ptr(),
                args.len() as BinaryenIndex,
                param_tuple_ty,
                ret_tuple_ty,
            )
        };
        Expression(module.0, expr)
    }

    pub fn local_get(module: &Module, local: ir::Local, ty: ir::Type) -> Expression {
        let local = local.index() as BinaryenIndex;
        let ty = Type::from(ty).to_binaryen();
        let expr = unsafe { BinaryenLocalGet(module.0, local, ty) };
        Expression(module.0, expr)
    }

    pub fn local_set(module: &Module, local: ir::Local, value: Expression) -> Expression {
        let local = local.index() as BinaryenIndex;
        let expr = unsafe { BinaryenLocalSet(module.0, local, value.1) };
        Expression(module.0, expr)
    }

    pub fn local_tee(
        module: &Module,
        local: ir::Local,
        value: Expression,
        ty: ir::Type,
    ) -> Expression {
        let local = local.index() as BinaryenIndex;
        let ty = Type::from(ty).to_binaryen();
        let expr = unsafe { BinaryenLocalTee(module.0, local, value.1, ty) };
        Expression(module.0, expr)
    }

    pub fn global_get(module: &Module, global: ir::Global, ty: ir::Type) -> Expression {
        let global = global.index() as BinaryenIndex;
        let ty = Type::from(ty).to_binaryen();
        let expr = unsafe { BinaryenGlobalGet(module.0, global, ty) };
        Expression(module.0, expr)
    }

    pub fn global_set(module: &Module, global: ir::Global, value: Expression) -> Expression {
        let global = global.index() as BinaryenIndex;
        let expr = unsafe { BinaryenGlobalSet(module.0, global, value.1) };
        Expression(module.0, expr)
    }

    pub fn select(
        module: &Module,
        cond: Expression,
        if_true: Expression,
        if_false: Expression,
        ty: ir::Type,
    ) -> Expression {
        let ty = Type::from(ty).to_binaryen();
        let expr = unsafe { BinaryenSelect(module.0, cond.1, if_true.1, if_false.1, ty) };
        Expression(module.0, expr)
    }

    pub fn expr_drop(module: &Module, value: Expression) -> Expression {
        Expression(module.0, unsafe { BinaryenDrop(module.0, value.1) })
    }

    pub fn ret(module: &Module, values: &[Expression]) -> Expression {
        let expr = if values.len() == 0 {
            unsafe { BinaryenReturn(module.0, std::ptr::null()) }
        } else if values.len() == 1 {
            unsafe { BinaryenReturn(module.0, values[0].1) }
        } else {
            let exprs = values.iter().map(|e| e.1).collect::<Vec<_>>();
            let tuple = unsafe {
                BinaryenTupleMake(module.0, exprs.as_ptr(), exprs.len() as BinaryenIndex)
            };
            unsafe { BinaryenReturn(module.0, tuple) }
        };
        Expression(module.0, expr)
    }

    pub fn load(
        module: &Module,
        bytes: u8,
        signed: bool,
        offset: u32,
        align: u32,
        ty: ir::Type,
        ptr: Expression,
        mem: ir::Memory,
    ) -> Expression {
        assert_eq!(mem.index(), 0);
        let ty = Type::from(ty).to_binaryen();
        let expr =
            unsafe { BinaryenLoad(module.0, bytes as u32, signed, offset, align, ty, ptr.1) };
        Expression(module.0, expr)
    }

    pub fn store(
        module: &Module,
        bytes: u8,
        offset: u32,
        align: u32,
        ty: ir::Type,
        ptr: Expression,
        value: Expression,
        mem: ir::Memory,
    ) -> Expression {
        assert_eq!(mem.index(), 0);
        let ty = Type::from(ty).to_binaryen();
        let expr =
            unsafe { BinaryenStore(module.0, bytes as u32, offset, align, ptr.1, value.1, ty) };
        Expression(module.0, expr)
    }

    pub fn const_i32(module: &Module, value: i32) -> Expression {
        let expr = unsafe { BinaryenConst(module.0, BinaryenLiteralInt32(value)) };
        Expression(module.0, expr)
    }
    pub fn const_i64(module: &Module, value: i64) -> Expression {
        let expr = unsafe { BinaryenConst(module.0, BinaryenLiteralInt64(value)) };
        Expression(module.0, expr)
    }
    pub fn const_f32(module: &Module, value: Ieee32) -> Expression {
        let expr =
            unsafe { BinaryenConst(module.0, BinaryenLiteralFloat32Bits(value.bits() as i32)) };
        Expression(module.0, expr)
    }
    pub fn const_f64(module: &Module, value: Ieee64) -> Expression {
        let expr =
            unsafe { BinaryenConst(module.0, BinaryenLiteralFloat64Bits(value.bits() as i64)) };
        Expression(module.0, expr)
    }

    pub fn table_get(
        module: &Module,
        table: ir::Table,
        index: Expression,
        ty: ir::Type,
    ) -> Expression {
        let table_name = unsafe {
            BinaryenTableGetName(BinaryenGetTableByIndex(module.0, table.index() as u32))
        };
        let ty = Type::from(ty).to_binaryen();
        let expr = unsafe { BinaryenTableGet(module.0, table_name, index.1, ty) };
        Expression(module.0, expr)
    }

    pub fn table_set(
        module: &Module,
        table: ir::Table,
        index: Expression,
        value: Expression,
    ) -> Expression {
        let table_name = unsafe {
            BinaryenTableGetName(BinaryenGetTableByIndex(module.0, table.index() as u32))
        };
        let expr = unsafe { BinaryenTableSet(module.0, table_name, index.1, value.1) };
        Expression(module.0, expr)
    }

    pub fn table_grow(
        module: &Module,
        table: ir::Table,
        delta: Expression,
        value: Expression,
    ) -> Expression {
        let table_name = unsafe {
            BinaryenTableGetName(BinaryenGetTableByIndex(module.0, table.index() as u32))
        };
        let expr = unsafe { BinaryenTableGrow(module.0, table_name, value.1, delta.1) };
        Expression(module.0, expr)
    }

    pub fn table_size(module: &Module, table: ir::Table) -> Expression {
        let table_name = unsafe {
            BinaryenTableGetName(BinaryenGetTableByIndex(module.0, table.index() as u32))
        };
        let expr = unsafe { BinaryenTableSize(module.0, table_name) };
        Expression(module.0, expr)
    }

    pub fn memory_size(module: &Module, mem: ir::Memory) -> Expression {
        assert_eq!(mem.index(), 0);
        Expression(module.0, unsafe { BinaryenMemorySize(module.0) })
    }

    pub fn memory_grow(module: &Module, mem: ir::Memory, delta: Expression) -> Expression {
        assert_eq!(mem.index(), 0);
        Expression(module.0, unsafe { BinaryenMemoryGrow(module.0, delta.1) })
    }
}

macro_rules! operator {
    (unary $name:tt, $bin_name:tt) => {
        impl Expression {
            pub fn $name(module: &Module, arg: Expression) -> Expression {
                Expression(module.0, unsafe {
                    BinaryenUnary(module.0, $bin_name(), arg.1)
                })
            }
        }
    };
    (binary $name:tt, $bin_name:tt) => {
        impl Expression {
            pub fn $name(module: &Module, arg0: Expression, arg1: Expression) -> Expression {
                Expression(module.0, unsafe {
                    BinaryenBinary(module.0, $bin_name(), arg0.1, arg1.1)
                })
            }
        }
    };
}

operator!(unary i32_eqz, BinaryenEqZInt32);
operator!(binary i32_eq, BinaryenEqInt32);
operator!(binary i32_ne, BinaryenNeInt32);
operator!(binary i32_lt_s, BinaryenLtSInt32);
operator!(binary i32_lt_u, BinaryenLtUInt32);
operator!(binary i32_gt_s, BinaryenGtSInt32);
operator!(binary i32_gt_u, BinaryenGtUInt32);
operator!(binary i32_le_s, BinaryenLeSInt32);
operator!(binary i32_le_u, BinaryenLeUInt32);
operator!(binary i32_ge_s, BinaryenGeSInt32);
operator!(binary i32_ge_u, BinaryenGeUInt32);

operator!(unary i64_eqz, BinaryenEqZInt64);
operator!(binary i64_eq, BinaryenEqInt64);
operator!(binary i64_ne, BinaryenNeInt64);
operator!(binary i64_lt_s, BinaryenLtSInt64);
operator!(binary i64_lt_u, BinaryenLtUInt64);
operator!(binary i64_gt_s, BinaryenGtSInt64);
operator!(binary i64_gt_u, BinaryenGtUInt64);
operator!(binary i64_le_s, BinaryenLeSInt64);
operator!(binary i64_le_u, BinaryenLeUInt64);
operator!(binary i64_ge_s, BinaryenGeSInt64);
operator!(binary i64_ge_u, BinaryenGeUInt64);

operator!(binary f32_eq, BinaryenEqFloat32);
operator!(binary f32_ne, BinaryenNeFloat32);
operator!(binary f32_lt, BinaryenLtFloat32);
operator!(binary f32_gt, BinaryenGtFloat32);
operator!(binary f32_le, BinaryenLeFloat32);
operator!(binary f32_ge, BinaryenGeFloat32);

operator!(binary f64_eq, BinaryenEqFloat64);
operator!(binary f64_ne, BinaryenNeFloat64);
operator!(binary f64_lt, BinaryenLtFloat64);
operator!(binary f64_gt, BinaryenGtFloat64);
operator!(binary f64_le, BinaryenLeFloat64);
operator!(binary f64_ge, BinaryenGeFloat64);

operator!(unary i32_clz, BinaryenClzInt32);
operator!(unary i32_ctz, BinaryenCtzInt32);
operator!(unary i32_popcnt, BinaryenPopcntInt32);

operator!(binary i32_add, BinaryenAddInt32);
operator!(binary i32_sub, BinaryenSubInt32);
operator!(binary i32_mul, BinaryenMulInt32);
operator!(binary i32_div_s, BinaryenDivSInt32);
operator!(binary i32_div_u, BinaryenDivUInt32);
operator!(binary i32_rem_s, BinaryenRemSInt32);
operator!(binary i32_rem_u, BinaryenRemUInt32);
operator!(binary i32_and, BinaryenAndInt32);
operator!(binary i32_or, BinaryenOrInt32);
operator!(binary i32_xor, BinaryenXorInt32);
operator!(binary i32_shl, BinaryenShlInt32);
operator!(binary i32_shr_s, BinaryenShrSInt32);
operator!(binary i32_shr_u, BinaryenShrUInt32);
operator!(binary i32_rotl, BinaryenRotLInt32);
operator!(binary i32_rotr, BinaryenRotRInt32);

operator!(unary i64_clz, BinaryenClzInt64);
operator!(unary i64_ctz, BinaryenCtzInt64);
operator!(unary i64_popcnt, BinaryenPopcntInt64);

operator!(binary i64_add, BinaryenAddInt64);
operator!(binary i64_sub, BinaryenSubInt64);
operator!(binary i64_mul, BinaryenMulInt64);
operator!(binary i64_div_s, BinaryenDivSInt64);
operator!(binary i64_div_u, BinaryenDivUInt64);
operator!(binary i64_rem_s, BinaryenRemSInt64);
operator!(binary i64_rem_u, BinaryenRemUInt64);
operator!(binary i64_and, BinaryenAndInt64);
operator!(binary i64_or, BinaryenOrInt64);
operator!(binary i64_xor, BinaryenXorInt64);
operator!(binary i64_shl, BinaryenShlInt64);
operator!(binary i64_shr_s, BinaryenShrSInt64);
operator!(binary i64_shr_u, BinaryenShrUInt64);
operator!(binary i64_rotl, BinaryenRotLInt64);
operator!(binary i64_rotr, BinaryenRotRInt64);

operator!(unary f32_abs, BinaryenAbsFloat32);
operator!(unary f32_neg, BinaryenNegFloat32);
operator!(unary f32_ceil, BinaryenCeilFloat32);
operator!(unary f32_floor, BinaryenFloorFloat32);
operator!(unary f32_trunc, BinaryenTruncFloat32);
operator!(unary f32_nearest, BinaryenNearestFloat32);
operator!(unary f32_sqrt, BinaryenSqrtFloat32);

operator!(binary f32_add, BinaryenAddFloat32);
operator!(binary f32_sub, BinaryenSubFloat32);
operator!(binary f32_mul, BinaryenMulFloat32);
operator!(binary f32_div, BinaryenDivFloat32);
operator!(binary f32_min, BinaryenMinFloat32);
operator!(binary f32_max, BinaryenMaxFloat32);
operator!(binary f32_copysign, BinaryenCopySignFloat32);

operator!(unary f64_abs, BinaryenAbsFloat64);
operator!(unary f64_neg, BinaryenNegFloat64);
operator!(unary f64_ceil, BinaryenCeilFloat64);
operator!(unary f64_floor, BinaryenFloorFloat64);
operator!(unary f64_trunc, BinaryenTruncFloat64);
operator!(unary f64_nearest, BinaryenNearestFloat64);
operator!(unary f64_sqrt, BinaryenSqrtFloat64);

operator!(binary f64_add, BinaryenAddFloat64);
operator!(binary f64_sub, BinaryenSubFloat64);
operator!(binary f64_mul, BinaryenMulFloat64);
operator!(binary f64_div, BinaryenDivFloat64);
operator!(binary f64_min, BinaryenMinFloat64);
operator!(binary f64_max, BinaryenMaxFloat64);
operator!(binary f64_copysign, BinaryenCopySignFloat64);

operator!(unary i32_wrap_i64, BinaryenWrapInt64);
operator!(unary i32_trunc_f32_s, BinaryenTruncSFloat32ToInt32);
operator!(unary i32_trunc_f32_u, BinaryenTruncUFloat32ToInt32);
operator!(unary i32_trunc_f64_s, BinaryenTruncSFloat64ToInt32);
operator!(unary i32_trunc_f64_u, BinaryenTruncUFloat64ToInt32);
operator!(unary i64_extend_i32_s, BinaryenExtendSInt32);
operator!(unary i64_extend_i32_u, BinaryenExtendUInt32);
operator!(unary i64_trunc_f32_s, BinaryenTruncSFloat32ToInt64);
operator!(unary i64_trunc_f32_u, BinaryenTruncUFloat32ToInt64);
operator!(unary i64_trunc_f64_s, BinaryenTruncSFloat64ToInt64);
operator!(unary i64_trunc_f64_u, BinaryenTruncUFloat64ToInt64);
operator!(unary f32_convert_i32_s, BinaryenConvertSInt32ToFloat32);
operator!(unary f32_convert_i32_u, BinaryenConvertUInt32ToFloat32);
operator!(unary f32_convert_i64_s, BinaryenConvertSInt64ToFloat32);
operator!(unary f32_convert_i64_u, BinaryenConvertUInt64ToFloat32);
operator!(unary f32_demote_f64, BinaryenDemoteFloat64);
operator!(unary f64_convert_i32_s, BinaryenConvertSInt32ToFloat64);
operator!(unary f64_convert_i32_u, BinaryenConvertUInt32ToFloat64);
operator!(unary f64_convert_i64_s, BinaryenConvertSInt64ToFloat64);
operator!(unary f64_convert_i64_u, BinaryenConvertUInt64ToFloat64);
operator!(unary f64_promote_f32, BinaryenPromoteFloat32);
operator!(unary i32_extend_8_s, BinaryenExtendS8Int32);
operator!(unary i32_extend_16_s, BinaryenExtendS16Int32);
operator!(unary i64_extend_8_s, BinaryenExtendS8Int64);
operator!(unary i64_extend_16_s, BinaryenExtendS16Int64);
operator!(unary i64_extend_32_s, BinaryenExtendS32Int64);
operator!(unary i32_trunc_sat_f32_s, BinaryenTruncSatSFloat32ToInt32);
operator!(unary i32_trunc_sat_f32_u, BinaryenTruncSatUFloat32ToInt32);
operator!(unary i32_trunc_sat_f64_s, BinaryenTruncSatSFloat64ToInt32);
operator!(unary i32_trunc_sat_f64_u, BinaryenTruncSatUFloat64ToInt32);
operator!(unary i64_trunc_sat_f32_s, BinaryenTruncSatSFloat32ToInt64);
operator!(unary i64_trunc_sat_f32_u, BinaryenTruncSatUFloat32ToInt64);
operator!(unary i64_trunc_sat_f64_s, BinaryenTruncSatSFloat64ToInt64);
operator!(unary i64_trunc_sat_f64_u, BinaryenTruncSatUFloat64ToInt64);
operator!(unary f32_reinterpret_i32, BinaryenReinterpretInt32);
operator!(unary f64_reinterpret_i64, BinaryenReinterpretInt64);
operator!(unary i32_reinterpret_f32, BinaryenReinterpretFloat32);
operator!(unary i64_reinterpret_f64, BinaryenReinterpretFloat64);

pub type BinaryenModule = *const c_void;
type BinaryenFunction = *const c_void;
type BinaryenExpression = *const c_void;
type BinaryenExport = *const c_void;
type BinaryenRelooper = *const c_void;
type BinaryenRelooperBlock = *const c_void;
type BinaryenTable = *const c_void;
type BinaryenGlobal = *const c_void;
type BinaryenElementSegment = *const c_void;

#[repr(C)]
struct BinaryenModuleAllocateAndWriteResult {
    binary: *mut c_void,
    binary_bytes: libc::size_t,
    source_map: *mut c_char,
}

impl Drop for BinaryenModuleAllocateAndWriteResult {
    fn drop(&mut self) {
        unsafe {
            libc::free(self.binary);
            libc::free(self.source_map as *mut c_void);
        }
    }
}

pub struct Relooper(BinaryenModule, BinaryenRelooper);

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct RelooperBlock(BinaryenRelooperBlock);

impl Relooper {
    pub fn new(module: &Module) -> Relooper {
        let ptr = unsafe { RelooperCreate(module.0) };
        Relooper(module.0, ptr)
    }

    pub fn construct(self, entry: RelooperBlock, index_var: usize) -> Expression {
        let module = self.0;
        let expr = unsafe { RelooperRenderAndDispose(self.1, entry.0, index_var as BinaryenIndex) };
        std::mem::forget(self);
        Expression(module, expr)
    }

    pub fn add_block(&mut self, expr: Expression) -> RelooperBlock {
        RelooperBlock(unsafe { RelooperAddBlock(self.1, expr.1) })
    }

    pub fn add_block_with_switch(&mut self, expr: Expression, sel: Expression) -> RelooperBlock {
        RelooperBlock(unsafe { RelooperAddBlockWithSwitch(self.1, expr.1, sel.1) })
    }
}

impl RelooperBlock {
    pub fn cond_branch(&self, to: RelooperBlock, cond: Expression, edge: Expression) {
        unsafe {
            RelooperAddBranch(self.0, to.0, cond.1, edge.1);
        }
    }

    pub fn branch(&self, to: RelooperBlock, edge: Expression) {
        unsafe {
            RelooperAddBranch(self.0, to.0, std::ptr::null(), edge.1);
        }
    }

    pub fn switch(&self, to: RelooperBlock, edge: Expression, indices: &[BinaryenIndex]) {
        unsafe {
            RelooperAddBranchForSwitch(
                self.0,
                to.0,
                indices.as_ptr(),
                indices.len() as BinaryenIndex,
                edge.1,
            );
        }
    }
}

impl Drop for Relooper {
    fn drop(&mut self) {
        panic!("Relooper dropped without constructing/disposing");
    }
}

#[link(name = "binaryen")]
extern "C" {
    fn BinaryenModuleRead(data: *const u8, len: usize) -> BinaryenModule;
    fn BinaryenModuleCreate() -> BinaryenModule;
    fn BinaryenModuleDispose(ptr: BinaryenModule);
    fn BinaryenModuleAllocateAndWrite(
        ptr: BinaryenModule,
        sourceMapUrl: *const c_char,
    ) -> BinaryenModuleAllocateAndWriteResult;
    fn BinaryenGetNumFunctions(ptr: BinaryenModule) -> u32;
    fn BinaryenGetFunctionByIndex(ptr: BinaryenModule, index: u32) -> BinaryenFunction;
    fn BinaryenGetFunction(ptr: BinaryenModule, name: *const c_char) -> BinaryenFunction;
    fn BinaryenFunctionGetBody(ptr: BinaryenFunction) -> BinaryenExpression;
    fn BinaryenFunctionSetBody(ptr: BinaryenFunction, body: BinaryenExpression);
    fn BinaryenFunctionGetName(ptr: BinaryenFunction) -> *const c_char;
    fn BinaryenFunctionAddVar(ptr: BinaryenFunction, ty: BinaryenType) -> BinaryenIndex;
    fn BinaryenGetExport(ptr: BinaryenModule, name: *const c_char) -> BinaryenExport;
    fn BinaryenGetNumExports(ptr: BinaryenModule) -> u32;
    fn BinaryenGetExportByIndex(ptr: BinaryenModule, index: u32) -> BinaryenExport;
    fn BinaryenExportGetName(ptr: BinaryenFunction) -> *const c_char;
    fn BinaryenExportGetValue(ptr: BinaryenFunction) -> *const c_char;
    fn BinaryenExportGetKind(ptr: BinaryenFunction) -> u32;
    fn BinaryenExternalFunction() -> u32;
    fn BinaryenGetTableByIndex(ptr: BinaryenModule, index: BinaryenIndex) -> BinaryenTable;

    fn BinaryenTableGetName(table: BinaryenTable) -> *const c_char;

    fn BinaryenBlockGetNumChildren(ptr: BinaryenExpression) -> u32;
    fn BinaryenBlockAppendChild(
        ptr: BinaryenExpression,
        child: BinaryenExpression,
    ) -> BinaryenIndex;

    fn BinaryenGetNumMemorySegments(module: BinaryenModule) -> u32;
    fn BinaryenGetMemorySegmentByteOffset(module: BinaryenModule, index: u32) -> u32;
    fn BinaryenGetMemorySegmentByteLength(module: BinaryenModule, index: u32) -> usize;
    fn BinaryenCopyMemorySegmentData(module: BinaryenModule, index: u32, buffer: *mut u8);

    fn BinaryenTypeNone() -> BinaryenType;
    fn BinaryenTypeInt32() -> BinaryenType;
    fn BinaryenTypeInt64() -> BinaryenType;
    fn BinaryenTypeFloat32() -> BinaryenType;
    fn BinaryenTypeFloat64() -> BinaryenType;
    fn BinaryenTypeVec128() -> BinaryenType;

    fn BinaryenTypeCreate(tys: *const BinaryenType, n_tys: BinaryenIndex) -> BinaryenType;

    fn BinaryenConst(module: BinaryenModule, lit: BinaryenLiteral) -> BinaryenExpression;
    fn BinaryenUnreachable(module: BinaryenModule) -> BinaryenExpression;
    fn BinaryenNop(module: BinaryenModule) -> BinaryenExpression;
    fn BinaryenLocalGet(
        module: BinaryenModule,
        local: BinaryenIndex,
        ty: BinaryenType,
    ) -> BinaryenExpression;
    fn BinaryenLocalSet(
        module: BinaryenModule,
        local: BinaryenIndex,
        value: BinaryenExpression,
    ) -> BinaryenExpression;
    fn BinaryenLocalTee(
        module: BinaryenModule,
        local: BinaryenIndex,
        value: BinaryenExpression,
        ty: BinaryenType,
    ) -> BinaryenExpression;
    fn BinaryenGlobalGet(
        module: BinaryenModule,
        local: BinaryenIndex,
        ty: BinaryenType,
    ) -> BinaryenExpression;
    fn BinaryenGlobalSet(
        module: BinaryenModule,
        local: BinaryenIndex,
        value: BinaryenExpression,
    ) -> BinaryenExpression;
    fn BinaryenSelect(
        module: BinaryenModule,
        cond: BinaryenExpression,
        if_true: BinaryenExpression,
        if_false: BinaryenExpression,
        ty: BinaryenType,
    ) -> BinaryenExpression;
    fn BinaryenBlock(
        module: BinaryenModule,
        name: *const c_char,
        children: *const BinaryenExpression,
        n_children: BinaryenIndex,
        ty: BinaryenType,
    ) -> BinaryenExpression;
    fn BinaryenTupleMake(
        module: BinaryenModule,
        operands: *const BinaryenExpression,
        n_operands: BinaryenIndex,
    ) -> BinaryenExpression;
    fn BinaryenReturn(module: BinaryenModule, expr: BinaryenExpression) -> BinaryenExpression;
    fn BinaryenDrop(module: BinaryenModule, expr: BinaryenExpression) -> BinaryenExpression;
    fn BinaryenUnary(
        module: BinaryenModule,
        op: BinaryenOp,
        arg: BinaryenExpression,
    ) -> BinaryenExpression;
    fn BinaryenBinary(
        module: BinaryenModule,
        op: BinaryenOp,
        left: BinaryenExpression,
        right: BinaryenExpression,
    ) -> BinaryenExpression;
    fn BinaryenCall(
        module: BinaryenModule,
        target: *const c_char,
        operands: *const BinaryenExpression,
        n_operands: BinaryenIndex,
        ret_type: BinaryenType,
    ) -> BinaryenExpression;
    fn BinaryenCallIndirect(
        module: BinaryenModule,
        table: *const c_char,
        target: BinaryenExpression,
        operands: *const BinaryenExpression,
        n_operands: BinaryenIndex,
        param_type: BinaryenType,
        ret_type: BinaryenType,
    ) -> BinaryenExpression;
    fn BinaryenLoad(
        module: BinaryenModule,
        bytes: u32,
        signed: bool,
        offset: u32,
        align: u32,
        ty: BinaryenType,
        ptr: BinaryenExpression,
    ) -> BinaryenExpression;
    fn BinaryenStore(
        module: BinaryenModule,
        bytes: u32,
        offset: u32,
        align: u32,
        ptr: BinaryenExpression,
        value: BinaryenExpression,
        ty: BinaryenType,
    ) -> BinaryenExpression;
    fn BinaryenMemorySize(module: BinaryenModule) -> BinaryenExpression;
    fn BinaryenMemoryGrow(module: BinaryenModule, expr: BinaryenExpression) -> BinaryenExpression;
    fn BinaryenTableGet(
        module: BinaryenModule,
        name: *const c_char,
        index: BinaryenExpression,
        ty: BinaryenType,
    ) -> BinaryenExpression;
    fn BinaryenTableSet(
        module: BinaryenModule,
        name: *const c_char,
        index: BinaryenExpression,
        value: BinaryenExpression,
    ) -> BinaryenExpression;
    fn BinaryenTableGrow(
        module: BinaryenModule,
        name: *const c_char,
        value: BinaryenExpression,
        delta: BinaryenExpression,
    ) -> BinaryenExpression;
    fn BinaryenTableSize(module: BinaryenModule, name: *const c_char) -> BinaryenExpression;

    fn BinaryenAddFunc(
        module: BinaryenModule,
        name: *const c_char,
        params: BinaryenType,
        results: BinaryenType,
        vars: *const BinaryenType,
        n_vars: BinaryenIndex,
        body: BinaryenExpression,
    ) -> BinaryenFunction;

    fn BinaryenUndefined() -> BinaryenType;

    fn BinaryenLiteralInt32(x: i32) -> BinaryenLiteral;
    fn BinaryenLiteralInt64(x: i64) -> BinaryenLiteral;
    fn BinaryenLiteralFloat32Bits(x: i32) -> BinaryenLiteral;
    fn BinaryenLiteralFloat64Bits(x: i64) -> BinaryenLiteral;

    fn RelooperCreate(module: BinaryenModule) -> BinaryenRelooper;
    fn RelooperRenderAndDispose(
        r: BinaryenRelooper,
        entry: BinaryenRelooperBlock,
        labelVar: BinaryenIndex,
    ) -> BinaryenExpression;
    fn RelooperAddBlock(r: BinaryenRelooper, code: BinaryenExpression) -> BinaryenRelooperBlock;
    fn RelooperAddBranch(
        from: BinaryenRelooperBlock,
        to: BinaryenRelooperBlock,
        cond: BinaryenExpression,
        edge_code: BinaryenExpression,
    );
    fn RelooperAddBlockWithSwitch(
        r: BinaryenRelooper,
        code: BinaryenExpression,
        selector: BinaryenExpression,
    ) -> BinaryenRelooperBlock;
    fn RelooperAddBranchForSwitch(
        from: BinaryenRelooperBlock,
        to: BinaryenRelooperBlock,
        indices: *const BinaryenIndex,
        n_indices: BinaryenIndex,
        edge_code: BinaryenExpression,
    );

    fn BinaryenClzInt32() -> BinaryenOp;
    fn BinaryenCtzInt32() -> BinaryenOp;
    fn BinaryenPopcntInt32() -> BinaryenOp;
    fn BinaryenNegFloat32() -> BinaryenOp;
    fn BinaryenAbsFloat32() -> BinaryenOp;
    fn BinaryenCeilFloat32() -> BinaryenOp;
    fn BinaryenFloorFloat32() -> BinaryenOp;
    fn BinaryenTruncFloat32() -> BinaryenOp;
    fn BinaryenNearestFloat32() -> BinaryenOp;
    fn BinaryenSqrtFloat32() -> BinaryenOp;
    fn BinaryenEqZInt32() -> BinaryenOp;
    fn BinaryenClzInt64() -> BinaryenOp;
    fn BinaryenCtzInt64() -> BinaryenOp;
    fn BinaryenPopcntInt64() -> BinaryenOp;
    fn BinaryenNegFloat64() -> BinaryenOp;
    fn BinaryenAbsFloat64() -> BinaryenOp;
    fn BinaryenCeilFloat64() -> BinaryenOp;
    fn BinaryenFloorFloat64() -> BinaryenOp;
    fn BinaryenTruncFloat64() -> BinaryenOp;
    fn BinaryenNearestFloat64() -> BinaryenOp;
    fn BinaryenSqrtFloat64() -> BinaryenOp;
    fn BinaryenEqZInt64() -> BinaryenOp;
    fn BinaryenExtendSInt32() -> BinaryenOp;
    fn BinaryenExtendUInt32() -> BinaryenOp;
    fn BinaryenWrapInt64() -> BinaryenOp;
    fn BinaryenTruncSFloat32ToInt32() -> BinaryenOp;
    fn BinaryenTruncSFloat32ToInt64() -> BinaryenOp;
    fn BinaryenTruncUFloat32ToInt32() -> BinaryenOp;
    fn BinaryenTruncUFloat32ToInt64() -> BinaryenOp;
    fn BinaryenTruncSFloat64ToInt32() -> BinaryenOp;
    fn BinaryenTruncSFloat64ToInt64() -> BinaryenOp;
    fn BinaryenTruncUFloat64ToInt32() -> BinaryenOp;
    fn BinaryenTruncUFloat64ToInt64() -> BinaryenOp;
    fn BinaryenReinterpretFloat32() -> BinaryenOp;
    fn BinaryenReinterpretFloat64() -> BinaryenOp;
    fn BinaryenConvertSInt32ToFloat32() -> BinaryenOp;
    fn BinaryenConvertSInt32ToFloat64() -> BinaryenOp;
    fn BinaryenConvertUInt32ToFloat32() -> BinaryenOp;
    fn BinaryenConvertUInt32ToFloat64() -> BinaryenOp;
    fn BinaryenConvertSInt64ToFloat32() -> BinaryenOp;
    fn BinaryenConvertSInt64ToFloat64() -> BinaryenOp;
    fn BinaryenConvertUInt64ToFloat32() -> BinaryenOp;
    fn BinaryenConvertUInt64ToFloat64() -> BinaryenOp;
    fn BinaryenPromoteFloat32() -> BinaryenOp;
    fn BinaryenDemoteFloat64() -> BinaryenOp;
    fn BinaryenReinterpretInt32() -> BinaryenOp;
    fn BinaryenReinterpretInt64() -> BinaryenOp;
    fn BinaryenExtendS8Int32() -> BinaryenOp;
    fn BinaryenExtendS16Int32() -> BinaryenOp;
    fn BinaryenExtendS8Int64() -> BinaryenOp;
    fn BinaryenExtendS16Int64() -> BinaryenOp;
    fn BinaryenExtendS32Int64() -> BinaryenOp;
    fn BinaryenAddInt32() -> BinaryenOp;
    fn BinaryenSubInt32() -> BinaryenOp;
    fn BinaryenMulInt32() -> BinaryenOp;
    fn BinaryenDivSInt32() -> BinaryenOp;
    fn BinaryenDivUInt32() -> BinaryenOp;
    fn BinaryenRemSInt32() -> BinaryenOp;
    fn BinaryenRemUInt32() -> BinaryenOp;
    fn BinaryenAndInt32() -> BinaryenOp;
    fn BinaryenOrInt32() -> BinaryenOp;
    fn BinaryenXorInt32() -> BinaryenOp;
    fn BinaryenShlInt32() -> BinaryenOp;
    fn BinaryenShrUInt32() -> BinaryenOp;
    fn BinaryenShrSInt32() -> BinaryenOp;
    fn BinaryenRotLInt32() -> BinaryenOp;
    fn BinaryenRotRInt32() -> BinaryenOp;
    fn BinaryenEqInt32() -> BinaryenOp;
    fn BinaryenNeInt32() -> BinaryenOp;
    fn BinaryenLtSInt32() -> BinaryenOp;
    fn BinaryenLtUInt32() -> BinaryenOp;
    fn BinaryenLeSInt32() -> BinaryenOp;
    fn BinaryenLeUInt32() -> BinaryenOp;
    fn BinaryenGtSInt32() -> BinaryenOp;
    fn BinaryenGtUInt32() -> BinaryenOp;
    fn BinaryenGeSInt32() -> BinaryenOp;
    fn BinaryenGeUInt32() -> BinaryenOp;
    fn BinaryenAddInt64() -> BinaryenOp;
    fn BinaryenSubInt64() -> BinaryenOp;
    fn BinaryenMulInt64() -> BinaryenOp;
    fn BinaryenDivSInt64() -> BinaryenOp;
    fn BinaryenDivUInt64() -> BinaryenOp;
    fn BinaryenRemSInt64() -> BinaryenOp;
    fn BinaryenRemUInt64() -> BinaryenOp;
    fn BinaryenAndInt64() -> BinaryenOp;
    fn BinaryenOrInt64() -> BinaryenOp;
    fn BinaryenXorInt64() -> BinaryenOp;
    fn BinaryenShlInt64() -> BinaryenOp;
    fn BinaryenShrUInt64() -> BinaryenOp;
    fn BinaryenShrSInt64() -> BinaryenOp;
    fn BinaryenRotLInt64() -> BinaryenOp;
    fn BinaryenRotRInt64() -> BinaryenOp;
    fn BinaryenEqInt64() -> BinaryenOp;
    fn BinaryenNeInt64() -> BinaryenOp;
    fn BinaryenLtSInt64() -> BinaryenOp;
    fn BinaryenLtUInt64() -> BinaryenOp;
    fn BinaryenLeSInt64() -> BinaryenOp;
    fn BinaryenLeUInt64() -> BinaryenOp;
    fn BinaryenGtSInt64() -> BinaryenOp;
    fn BinaryenGtUInt64() -> BinaryenOp;
    fn BinaryenGeSInt64() -> BinaryenOp;
    fn BinaryenGeUInt64() -> BinaryenOp;
    fn BinaryenAddFloat32() -> BinaryenOp;
    fn BinaryenSubFloat32() -> BinaryenOp;
    fn BinaryenMulFloat32() -> BinaryenOp;
    fn BinaryenDivFloat32() -> BinaryenOp;
    fn BinaryenCopySignFloat32() -> BinaryenOp;
    fn BinaryenMinFloat32() -> BinaryenOp;
    fn BinaryenMaxFloat32() -> BinaryenOp;
    fn BinaryenEqFloat32() -> BinaryenOp;
    fn BinaryenNeFloat32() -> BinaryenOp;
    fn BinaryenLtFloat32() -> BinaryenOp;
    fn BinaryenLeFloat32() -> BinaryenOp;
    fn BinaryenGtFloat32() -> BinaryenOp;
    fn BinaryenGeFloat32() -> BinaryenOp;
    fn BinaryenAddFloat64() -> BinaryenOp;
    fn BinaryenSubFloat64() -> BinaryenOp;
    fn BinaryenMulFloat64() -> BinaryenOp;
    fn BinaryenDivFloat64() -> BinaryenOp;
    fn BinaryenCopySignFloat64() -> BinaryenOp;
    fn BinaryenMinFloat64() -> BinaryenOp;
    fn BinaryenMaxFloat64() -> BinaryenOp;
    fn BinaryenEqFloat64() -> BinaryenOp;
    fn BinaryenNeFloat64() -> BinaryenOp;
    fn BinaryenLtFloat64() -> BinaryenOp;
    fn BinaryenLeFloat64() -> BinaryenOp;
    fn BinaryenGtFloat64() -> BinaryenOp;
    fn BinaryenGeFloat64() -> BinaryenOp;
    fn BinaryenAtomicRMWAdd() -> BinaryenOp;
    fn BinaryenAtomicRMWSub() -> BinaryenOp;
    fn BinaryenAtomicRMWAnd() -> BinaryenOp;
    fn BinaryenAtomicRMWOr() -> BinaryenOp;
    fn BinaryenAtomicRMWXor() -> BinaryenOp;
    fn BinaryenAtomicRMWXchg() -> BinaryenOp;
    fn BinaryenTruncSatSFloat32ToInt32() -> BinaryenOp;
    fn BinaryenTruncSatSFloat32ToInt64() -> BinaryenOp;
    fn BinaryenTruncSatUFloat32ToInt32() -> BinaryenOp;
    fn BinaryenTruncSatUFloat32ToInt64() -> BinaryenOp;
    fn BinaryenTruncSatSFloat64ToInt32() -> BinaryenOp;
    fn BinaryenTruncSatSFloat64ToInt64() -> BinaryenOp;
    fn BinaryenTruncSatUFloat64ToInt32() -> BinaryenOp;
    fn BinaryenTruncSatUFloat64ToInt64() -> BinaryenOp;
    fn BinaryenSplatVecI8x16() -> BinaryenOp;
    fn BinaryenExtractLaneSVecI8x16() -> BinaryenOp;
    fn BinaryenExtractLaneUVecI8x16() -> BinaryenOp;
    fn BinaryenReplaceLaneVecI8x16() -> BinaryenOp;
    fn BinaryenSplatVecI16x8() -> BinaryenOp;
    fn BinaryenExtractLaneSVecI16x8() -> BinaryenOp;
    fn BinaryenExtractLaneUVecI16x8() -> BinaryenOp;
    fn BinaryenReplaceLaneVecI16x8() -> BinaryenOp;
    fn BinaryenSplatVecI32x4() -> BinaryenOp;
    fn BinaryenExtractLaneVecI32x4() -> BinaryenOp;
    fn BinaryenReplaceLaneVecI32x4() -> BinaryenOp;
    fn BinaryenSplatVecI64x2() -> BinaryenOp;
    fn BinaryenExtractLaneVecI64x2() -> BinaryenOp;
    fn BinaryenReplaceLaneVecI64x2() -> BinaryenOp;
    fn BinaryenSplatVecF32x4() -> BinaryenOp;
    fn BinaryenExtractLaneVecF32x4() -> BinaryenOp;
    fn BinaryenReplaceLaneVecF32x4() -> BinaryenOp;
    fn BinaryenSplatVecF64x2() -> BinaryenOp;
    fn BinaryenExtractLaneVecF64x2() -> BinaryenOp;
    fn BinaryenReplaceLaneVecF64x2() -> BinaryenOp;
    fn BinaryenEqVecI8x16() -> BinaryenOp;
    fn BinaryenNeVecI8x16() -> BinaryenOp;
    fn BinaryenLtSVecI8x16() -> BinaryenOp;
    fn BinaryenLtUVecI8x16() -> BinaryenOp;
    fn BinaryenGtSVecI8x16() -> BinaryenOp;
    fn BinaryenGtUVecI8x16() -> BinaryenOp;
    fn BinaryenLeSVecI8x16() -> BinaryenOp;
    fn BinaryenLeUVecI8x16() -> BinaryenOp;
    fn BinaryenGeSVecI8x16() -> BinaryenOp;
    fn BinaryenGeUVecI8x16() -> BinaryenOp;
    fn BinaryenEqVecI16x8() -> BinaryenOp;
    fn BinaryenNeVecI16x8() -> BinaryenOp;
    fn BinaryenLtSVecI16x8() -> BinaryenOp;
    fn BinaryenLtUVecI16x8() -> BinaryenOp;
    fn BinaryenGtSVecI16x8() -> BinaryenOp;
    fn BinaryenGtUVecI16x8() -> BinaryenOp;
    fn BinaryenLeSVecI16x8() -> BinaryenOp;
    fn BinaryenLeUVecI16x8() -> BinaryenOp;
    fn BinaryenGeSVecI16x8() -> BinaryenOp;
    fn BinaryenGeUVecI16x8() -> BinaryenOp;
    fn BinaryenEqVecI32x4() -> BinaryenOp;
    fn BinaryenNeVecI32x4() -> BinaryenOp;
    fn BinaryenLtSVecI32x4() -> BinaryenOp;
    fn BinaryenLtUVecI32x4() -> BinaryenOp;
    fn BinaryenGtSVecI32x4() -> BinaryenOp;
    fn BinaryenGtUVecI32x4() -> BinaryenOp;
    fn BinaryenLeSVecI32x4() -> BinaryenOp;
    fn BinaryenLeUVecI32x4() -> BinaryenOp;
    fn BinaryenGeSVecI32x4() -> BinaryenOp;
    fn BinaryenGeUVecI32x4() -> BinaryenOp;
    fn BinaryenEqVecI64x2() -> BinaryenOp;
    fn BinaryenNeVecI64x2() -> BinaryenOp;
    fn BinaryenLtSVecI64x2() -> BinaryenOp;
    fn BinaryenGtSVecI64x2() -> BinaryenOp;
    fn BinaryenLeSVecI64x2() -> BinaryenOp;
    fn BinaryenGeSVecI64x2() -> BinaryenOp;
    fn BinaryenEqVecF32x4() -> BinaryenOp;
    fn BinaryenNeVecF32x4() -> BinaryenOp;
    fn BinaryenLtVecF32x4() -> BinaryenOp;
    fn BinaryenGtVecF32x4() -> BinaryenOp;
    fn BinaryenLeVecF32x4() -> BinaryenOp;
    fn BinaryenGeVecF32x4() -> BinaryenOp;
    fn BinaryenEqVecF64x2() -> BinaryenOp;
    fn BinaryenNeVecF64x2() -> BinaryenOp;
    fn BinaryenLtVecF64x2() -> BinaryenOp;
    fn BinaryenGtVecF64x2() -> BinaryenOp;
    fn BinaryenLeVecF64x2() -> BinaryenOp;
    fn BinaryenGeVecF64x2() -> BinaryenOp;
    fn BinaryenNotVec128() -> BinaryenOp;
    fn BinaryenAndVec128() -> BinaryenOp;
    fn BinaryenOrVec128() -> BinaryenOp;
    fn BinaryenXorVec128() -> BinaryenOp;
    fn BinaryenAndNotVec128() -> BinaryenOp;
    fn BinaryenBitselectVec128() -> BinaryenOp;
    fn BinaryenAnyTrueVec128() -> BinaryenOp;
    fn BinaryenPopcntVecI8x16() -> BinaryenOp;
    fn BinaryenAbsVecI8x16() -> BinaryenOp;
    fn BinaryenNegVecI8x16() -> BinaryenOp;
    fn BinaryenAllTrueVecI8x16() -> BinaryenOp;
    fn BinaryenBitmaskVecI8x16() -> BinaryenOp;
    fn BinaryenShlVecI8x16() -> BinaryenOp;
    fn BinaryenShrSVecI8x16() -> BinaryenOp;
    fn BinaryenShrUVecI8x16() -> BinaryenOp;
    fn BinaryenAddVecI8x16() -> BinaryenOp;
    fn BinaryenAddSatSVecI8x16() -> BinaryenOp;
    fn BinaryenAddSatUVecI8x16() -> BinaryenOp;
    fn BinaryenSubVecI8x16() -> BinaryenOp;
    fn BinaryenSubSatSVecI8x16() -> BinaryenOp;
    fn BinaryenSubSatUVecI8x16() -> BinaryenOp;
    fn BinaryenMinSVecI8x16() -> BinaryenOp;
    fn BinaryenMinUVecI8x16() -> BinaryenOp;
    fn BinaryenMaxSVecI8x16() -> BinaryenOp;
    fn BinaryenMaxUVecI8x16() -> BinaryenOp;
    fn BinaryenAvgrUVecI8x16() -> BinaryenOp;
    fn BinaryenAbsVecI16x8() -> BinaryenOp;
    fn BinaryenNegVecI16x8() -> BinaryenOp;
    fn BinaryenAllTrueVecI16x8() -> BinaryenOp;
    fn BinaryenBitmaskVecI16x8() -> BinaryenOp;
    fn BinaryenShlVecI16x8() -> BinaryenOp;
    fn BinaryenShrSVecI16x8() -> BinaryenOp;
    fn BinaryenShrUVecI16x8() -> BinaryenOp;
    fn BinaryenAddVecI16x8() -> BinaryenOp;
    fn BinaryenAddSatSVecI16x8() -> BinaryenOp;
    fn BinaryenAddSatUVecI16x8() -> BinaryenOp;
    fn BinaryenSubVecI16x8() -> BinaryenOp;
    fn BinaryenSubSatSVecI16x8() -> BinaryenOp;
    fn BinaryenSubSatUVecI16x8() -> BinaryenOp;
    fn BinaryenMulVecI16x8() -> BinaryenOp;
    fn BinaryenMinSVecI16x8() -> BinaryenOp;
    fn BinaryenMinUVecI16x8() -> BinaryenOp;
    fn BinaryenMaxSVecI16x8() -> BinaryenOp;
    fn BinaryenMaxUVecI16x8() -> BinaryenOp;
    fn BinaryenAvgrUVecI16x8() -> BinaryenOp;
    fn BinaryenQ15MulrSatSVecI16x8() -> BinaryenOp;
    fn BinaryenExtMulLowSVecI16x8() -> BinaryenOp;
    fn BinaryenExtMulHighSVecI16x8() -> BinaryenOp;
    fn BinaryenExtMulLowUVecI16x8() -> BinaryenOp;
    fn BinaryenExtMulHighUVecI16x8() -> BinaryenOp;
    fn BinaryenAbsVecI32x4() -> BinaryenOp;
    fn BinaryenNegVecI32x4() -> BinaryenOp;
    fn BinaryenAllTrueVecI32x4() -> BinaryenOp;
    fn BinaryenBitmaskVecI32x4() -> BinaryenOp;
    fn BinaryenShlVecI32x4() -> BinaryenOp;
    fn BinaryenShrSVecI32x4() -> BinaryenOp;
    fn BinaryenShrUVecI32x4() -> BinaryenOp;
    fn BinaryenAddVecI32x4() -> BinaryenOp;
    fn BinaryenSubVecI32x4() -> BinaryenOp;
    fn BinaryenMulVecI32x4() -> BinaryenOp;
    fn BinaryenMinSVecI32x4() -> BinaryenOp;
    fn BinaryenMinUVecI32x4() -> BinaryenOp;
    fn BinaryenMaxSVecI32x4() -> BinaryenOp;
    fn BinaryenMaxUVecI32x4() -> BinaryenOp;
    fn BinaryenDotSVecI16x8ToVecI32x4() -> BinaryenOp;
    fn BinaryenExtMulLowSVecI32x4() -> BinaryenOp;
    fn BinaryenExtMulHighSVecI32x4() -> BinaryenOp;
    fn BinaryenExtMulLowUVecI32x4() -> BinaryenOp;
    fn BinaryenExtMulHighUVecI32x4() -> BinaryenOp;
    fn BinaryenAbsVecI64x2() -> BinaryenOp;
    fn BinaryenNegVecI64x2() -> BinaryenOp;
    fn BinaryenAllTrueVecI64x2() -> BinaryenOp;
    fn BinaryenBitmaskVecI64x2() -> BinaryenOp;
    fn BinaryenShlVecI64x2() -> BinaryenOp;
    fn BinaryenShrSVecI64x2() -> BinaryenOp;
    fn BinaryenShrUVecI64x2() -> BinaryenOp;
    fn BinaryenAddVecI64x2() -> BinaryenOp;
    fn BinaryenSubVecI64x2() -> BinaryenOp;
    fn BinaryenMulVecI64x2() -> BinaryenOp;
    fn BinaryenExtMulLowSVecI64x2() -> BinaryenOp;
    fn BinaryenExtMulHighSVecI64x2() -> BinaryenOp;
    fn BinaryenExtMulLowUVecI64x2() -> BinaryenOp;
    fn BinaryenExtMulHighUVecI64x2() -> BinaryenOp;
    fn BinaryenAbsVecF32x4() -> BinaryenOp;
    fn BinaryenNegVecF32x4() -> BinaryenOp;
    fn BinaryenSqrtVecF32x4() -> BinaryenOp;
    fn BinaryenAddVecF32x4() -> BinaryenOp;
    fn BinaryenSubVecF32x4() -> BinaryenOp;
    fn BinaryenMulVecF32x4() -> BinaryenOp;
    fn BinaryenDivVecF32x4() -> BinaryenOp;
    fn BinaryenMinVecF32x4() -> BinaryenOp;
    fn BinaryenMaxVecF32x4() -> BinaryenOp;
    fn BinaryenPMinVecF32x4() -> BinaryenOp;
    fn BinaryenPMaxVecF32x4() -> BinaryenOp;
    fn BinaryenCeilVecF32x4() -> BinaryenOp;
    fn BinaryenFloorVecF32x4() -> BinaryenOp;
    fn BinaryenTruncVecF32x4() -> BinaryenOp;
    fn BinaryenNearestVecF32x4() -> BinaryenOp;
    fn BinaryenAbsVecF64x2() -> BinaryenOp;
    fn BinaryenNegVecF64x2() -> BinaryenOp;
    fn BinaryenSqrtVecF64x2() -> BinaryenOp;
    fn BinaryenAddVecF64x2() -> BinaryenOp;
    fn BinaryenSubVecF64x2() -> BinaryenOp;
    fn BinaryenMulVecF64x2() -> BinaryenOp;
    fn BinaryenDivVecF64x2() -> BinaryenOp;
    fn BinaryenMinVecF64x2() -> BinaryenOp;
    fn BinaryenMaxVecF64x2() -> BinaryenOp;
    fn BinaryenPMinVecF64x2() -> BinaryenOp;
    fn BinaryenPMaxVecF64x2() -> BinaryenOp;
    fn BinaryenCeilVecF64x2() -> BinaryenOp;
    fn BinaryenFloorVecF64x2() -> BinaryenOp;
    fn BinaryenTruncVecF64x2() -> BinaryenOp;
    fn BinaryenNearestVecF64x2() -> BinaryenOp;
    fn BinaryenExtAddPairwiseSVecI8x16ToI16x8() -> BinaryenOp;
    fn BinaryenExtAddPairwiseUVecI8x16ToI16x8() -> BinaryenOp;
    fn BinaryenExtAddPairwiseSVecI16x8ToI32x4() -> BinaryenOp;
    fn BinaryenExtAddPairwiseUVecI16x8ToI32x4() -> BinaryenOp;
    fn BinaryenTruncSatSVecF32x4ToVecI32x4() -> BinaryenOp;
    fn BinaryenTruncSatUVecF32x4ToVecI32x4() -> BinaryenOp;
    fn BinaryenConvertSVecI32x4ToVecF32x4() -> BinaryenOp;
    fn BinaryenConvertUVecI32x4ToVecF32x4() -> BinaryenOp;
    fn BinaryenLoad8SplatVec128() -> BinaryenOp;
    fn BinaryenLoad16SplatVec128() -> BinaryenOp;
    fn BinaryenLoad32SplatVec128() -> BinaryenOp;
    fn BinaryenLoad64SplatVec128() -> BinaryenOp;
    fn BinaryenLoad8x8SVec128() -> BinaryenOp;
    fn BinaryenLoad8x8UVec128() -> BinaryenOp;
    fn BinaryenLoad16x4SVec128() -> BinaryenOp;
    fn BinaryenLoad16x4UVec128() -> BinaryenOp;
    fn BinaryenLoad32x2SVec128() -> BinaryenOp;
    fn BinaryenLoad32x2UVec128() -> BinaryenOp;
    fn BinaryenLoad32ZeroVec128() -> BinaryenOp;
    fn BinaryenLoad64ZeroVec128() -> BinaryenOp;
    fn BinaryenLoad8LaneVec128() -> BinaryenOp;
    fn BinaryenLoad16LaneVec128() -> BinaryenOp;
    fn BinaryenLoad32LaneVec128() -> BinaryenOp;
    fn BinaryenLoad64LaneVec128() -> BinaryenOp;
    fn BinaryenStore8LaneVec128() -> BinaryenOp;
    fn BinaryenStore16LaneVec128() -> BinaryenOp;
    fn BinaryenStore32LaneVec128() -> BinaryenOp;
    fn BinaryenStore64LaneVec128() -> BinaryenOp;
    fn BinaryenNarrowSVecI16x8ToVecI8x16() -> BinaryenOp;
    fn BinaryenNarrowUVecI16x8ToVecI8x16() -> BinaryenOp;
    fn BinaryenNarrowSVecI32x4ToVecI16x8() -> BinaryenOp;
    fn BinaryenNarrowUVecI32x4ToVecI16x8() -> BinaryenOp;
    fn BinaryenExtendLowSVecI8x16ToVecI16x8() -> BinaryenOp;
    fn BinaryenExtendHighSVecI8x16ToVecI16x8() -> BinaryenOp;
    fn BinaryenExtendLowUVecI8x16ToVecI16x8() -> BinaryenOp;
    fn BinaryenExtendHighUVecI8x16ToVecI16x8() -> BinaryenOp;
    fn BinaryenExtendLowSVecI16x8ToVecI32x4() -> BinaryenOp;
    fn BinaryenExtendHighSVecI16x8ToVecI32x4() -> BinaryenOp;
    fn BinaryenExtendLowUVecI16x8ToVecI32x4() -> BinaryenOp;
    fn BinaryenExtendHighUVecI16x8ToVecI32x4() -> BinaryenOp;
    fn BinaryenExtendLowSVecI32x4ToVecI64x2() -> BinaryenOp;
    fn BinaryenExtendHighSVecI32x4ToVecI64x2() -> BinaryenOp;
    fn BinaryenExtendLowUVecI32x4ToVecI64x2() -> BinaryenOp;
    fn BinaryenExtendHighUVecI32x4ToVecI64x2() -> BinaryenOp;
    fn BinaryenConvertLowSVecI32x4ToVecF64x2() -> BinaryenOp;
    fn BinaryenConvertLowUVecI32x4ToVecF64x2() -> BinaryenOp;
    fn BinaryenTruncSatZeroSVecF64x2ToVecI32x4() -> BinaryenOp;
    fn BinaryenTruncSatZeroUVecF64x2ToVecI32x4() -> BinaryenOp;
    fn BinaryenDemoteZeroVecF64x2ToVecF32x4() -> BinaryenOp;
    fn BinaryenPromoteLowVecF32x4ToVecF64x2() -> BinaryenOp;
    fn BinaryenSwizzleVec8x16() -> BinaryenOp;
    fn BinaryenRefIsNull() -> BinaryenOp;
    fn BinaryenRefIsFunc() -> BinaryenOp;
    fn BinaryenRefIsData() -> BinaryenOp;
    fn BinaryenRefIsI31() -> BinaryenOp;
    fn BinaryenRefAsNonNull() -> BinaryenOp;
    fn BinaryenRefAsFunc() -> BinaryenOp;
    fn BinaryenRefAsData() -> BinaryenOp;
    fn BinaryenRefAsI31() -> BinaryenOp;

    fn BinaryenAddGlobal(
        module: BinaryenModule,
        name: *const c_char,
        ty: BinaryenType,
        mutable: bool,
        init: BinaryenExpression,
    ) -> BinaryenGlobal;
    fn BinaryenGetNumGlobals(module: BinaryenModule) -> BinaryenIndex;

    fn BinaryenAddTable(
        module: BinaryenModule,
        name: *const c_char,
        initial: BinaryenIndex,
        max: BinaryenIndex,
        ty: BinaryenType,
    ) -> BinaryenTable;
    fn BinaryenGetNumTables(module: BinaryenModule) -> BinaryenIndex;

    fn BinaryenAddActiveElementSegment(
        module: BinaryenModule,
        table: *const c_char,
        name: *const c_char,
        func_names: *const *const c_char,
        num_funcs: BinaryenIndex,
        offset: BinaryenExpression,
    ) -> BinaryenElementSegment;

    fn BinaryenSetMemory(
        module: BinaryenModule,
        init: BinaryenIndex,
        max: BinaryenIndex,
        export_name: *const c_char,
        segments: *const *const c_char,
        seg_passive: *const bool,
        seg_offsets: *const BinaryenExpression,
        sizes: *const BinaryenIndex,
        n_segments: BinaryenIndex,
        shared: bool,
    );

    fn BinaryenAddTableImport(
        module: BinaryenModule,
        name: *const c_char,
        extern_module: *const c_char,
        extern_name: *const c_char,
    );
    fn BinaryenAddMemoryImport(
        module: BinaryenModule,
        name: *const c_char,
        extern_module: *const c_char,
        extern_name: *const c_char,
    );
    fn BinaryenAddGlobalImport(
        module: BinaryenModule,
        name: *const c_char,
        extern_module: *const c_char,
        extern_name: *const c_char,
        ty: BinaryenType,
        mutable: bool,
    );
    fn BinaryenAddFunctionImport(
        module: BinaryenModule,
        name: *const c_char,
        extern_module: *const c_char,
        extern_name: *const c_char,
        params: BinaryenType,
        results: BinaryenType,
    );

    fn BinaryenGlobalGetName(global: BinaryenGlobal) -> *const c_char;
    fn BinaryenGetGlobalByIndex(module: BinaryenModule, index: BinaryenIndex) -> BinaryenGlobal;
}

#[repr(C)]
struct BinaryenLiteral {
    _pad0: usize,
    _pad1: [u8; 16],
}

type BinaryenOp = i32;
