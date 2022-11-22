//! Binaryen bindings.

use crate::entity::EntityRef;
use crate::ir;
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
    pub fn ty(&self) -> Type {
        Type::from_binaryen(unsafe { BinaryenExpressionGetType(self.1) }).unwrap()
    }

    pub fn deep_clone(&self) -> Self {
        Expression(self.0, unsafe { BinaryenExpressionCopy(self.0, self.1) })
    }

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
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Other(u32),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    I32Add,
    I32Sub,
    I32Shl,
    I32ShrU,
    I32ShrS,
    Other(u32),
}

struct OpIds {
    i32_add: u32,
    i32_sub: u32,
    i32_shl: u32,
    i32_shr_u: u32,
    i32_shr_s: u32,
}

impl OpIds {
    fn get() -> Self {
        OpIds {
            i32_add: unsafe { BinaryenAddInt32() },
            i32_sub: unsafe { BinaryenSubInt32() },
            i32_shl: unsafe { BinaryenShlInt32() },
            i32_shr_u: unsafe { BinaryenShrUInt32() },
            i32_shr_s: unsafe { BinaryenShrSInt32() },
        }
    }
}

lazy_static! {
    static ref OP_IDS: OpIds = OpIds::get();
}

impl UnaryOp {
    fn from_binaryen(kind: u32) -> UnaryOp {
        UnaryOp::Other(kind)
    }
}

impl BinaryOp {
    fn from_binaryen(kind: u32) -> BinaryOp {
        let ids = &*OP_IDS;
        if kind == ids.i32_add {
            BinaryOp::I32Add
        } else if kind == ids.i32_sub {
            BinaryOp::I32Sub
        } else if kind == ids.i32_shl {
            BinaryOp::I32Shl
        } else if kind == ids.i32_shr_s {
            BinaryOp::I32ShrS
        } else if kind == ids.i32_shr_u {
            BinaryOp::I32ShrU
        } else {
            BinaryOp::Other(kind)
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Value {
    I32(i32),
    I64(i64),
    F32(u32),
    F64(u64),
}

pub type BinaryenModule = *const c_void;
type BinaryenFunction = *const c_void;
type BinaryenExpression = *const c_void;
type BinaryenExport = *const c_void;
type BinaryenRelooper = *const c_void;
type BinaryenRelooperBlock = *const c_void;
type BinaryenTable = *const c_void;

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

    fn BinaryenExpressionGetId(ptr: BinaryenExpression) -> u32;
    fn BinaryenExpressionGetType(ptr: BinaryenExpression) -> BinaryenType;
    fn BinaryenExpressionCopy(
        ptr: BinaryenExpression,
        module: BinaryenModule,
    ) -> BinaryenExpression;

    fn BinaryenConstGetValueI32(ptr: BinaryenExpression) -> i32;
    fn BinaryenConstGetValueI64(ptr: BinaryenExpression) -> i64;
    fn BinaryenConstGetValueF32(ptr: BinaryenExpression) -> f32;
    fn BinaryenConstGetValueF64(ptr: BinaryenExpression) -> f64;

    fn BinaryenBlockGetNumChildren(ptr: BinaryenExpression) -> u32;
    fn BinaryenBlockGetChildAt(ptr: BinaryenExpression, index: u32) -> BinaryenExpression;
    fn BinaryenBlockGetName(ptr: BinaryenExpression) -> *const c_char;
    fn BinaryenBlockAppendChild(
        ptr: BinaryenExpression,
        child: BinaryenExpression,
    ) -> BinaryenIndex;

    fn BinaryenLoopGetBody(ptr: BinaryenExpression) -> BinaryenExpression;
    fn BinaryenLoopGetName(ptr: BinaryenExpression) -> *const c_char;

    fn BinaryenIfGetCondition(ptr: BinaryenExpression) -> BinaryenExpression;
    fn BinaryenIfGetIfTrue(ptr: BinaryenExpression) -> BinaryenExpression;
    fn BinaryenIfGetIfFalse(ptr: BinaryenExpression) -> BinaryenExpression;

    fn BinaryenBreakGetName(ptr: BinaryenExpression) -> *const c_char;
    fn BinaryenBreakGetValue(ptr: BinaryenExpression) -> BinaryenExpression;

    fn BinaryenDropGetValue(ptr: BinaryenExpression) -> BinaryenExpression;

    fn BinaryenSwitchGetNumNames(ptr: BinaryenExpression) -> u32;
    fn BinaryenSwitchGetNameAt(ptr: BinaryenExpression, index: u32) -> *const c_char;
    fn BinaryenSwitchGetDefaultName(ptr: BinaryenExpression) -> *const c_char;
    fn BinaryenSwitchGetCondition(ptr: BinaryenExpression) -> BinaryenExpression;
    fn BinaryenSwitchGetValue(ptr: BinaryenExpression) -> BinaryenExpression;

    fn BinaryenCallGetTarget(ptr: BinaryenExpression) -> *const c_char;
    fn BinaryenCallGetNumOperands(ptr: BinaryenExpression) -> u32;
    fn BinaryenCallGetOperandAt(ptr: BinaryenExpression, index: u32) -> BinaryenExpression;

    fn BinaryenCallIndirectGetTarget(ptr: BinaryenExpression) -> BinaryenExpression;
    fn BinaryenCallIndirectGetNumOperands(ptr: BinaryenExpression) -> u32;
    fn BinaryenCallIndirectGetOperandAt(ptr: BinaryenExpression, index: u32) -> BinaryenExpression;

    fn BinaryenLocalGetGetIndex(ptr: BinaryenExpression) -> u32;
    fn BinaryenLocalSetGetIndex(ptr: BinaryenExpression) -> u32;
    fn BinaryenLocalSetGetValue(ptr: BinaryenExpression) -> BinaryenExpression;
    fn BinaryenGlobalGetGetName(ptr: BinaryenExpression) -> *const c_char;
    fn BinaryenGlobalSetGetName(ptr: BinaryenExpression) -> *const c_char;
    fn BinaryenGlobalSetGetValue(ptr: BinaryenExpression) -> BinaryenExpression;
    fn BinaryenTableGetGetTable(ptr: BinaryenExpression) -> *const c_char;
    fn BinaryenTableGetGetIndex(ptr: BinaryenExpression) -> BinaryenExpression;
    fn BinaryenTableSetGetTable(ptr: BinaryenExpression) -> *const c_char;
    fn BinaryenTableSetGetIndex(ptr: BinaryenExpression) -> BinaryenExpression;
    fn BinaryenTableSetGetValue(ptr: BinaryenExpression) -> BinaryenExpression;

    fn BinaryenLoadGetPtr(ptr: BinaryenExpression) -> BinaryenExpression;
    fn BinaryenLoadGetOffset(ptr: BinaryenExpression) -> u32;
    fn BinaryenStoreGetPtr(ptr: BinaryenExpression) -> BinaryenExpression;
    fn BinaryenStoreGetValue(ptr: BinaryenExpression) -> BinaryenExpression;
    fn BinaryenStoreGetOffset(ptr: BinaryenExpression) -> u32;

    fn BinaryenUnaryGetOp(ptr: BinaryenExpression) -> u32;
    fn BinaryenUnaryGetValue(ptr: BinaryenExpression) -> BinaryenExpression;

    fn BinaryenBinaryGetOp(ptr: BinaryenExpression) -> u32;
    fn BinaryenBinaryGetLeft(ptr: BinaryenExpression) -> BinaryenExpression;
    fn BinaryenBinaryGetRight(ptr: BinaryenExpression) -> BinaryenExpression;

    fn BinaryenSelectGetIfTrue(ptr: BinaryenExpression) -> BinaryenExpression;
    fn BinaryenSelectGetIfFalse(ptr: BinaryenExpression) -> BinaryenExpression;
    fn BinaryenSelectGetCondition(ptr: BinaryenExpression) -> BinaryenExpression;

    fn BinaryenReturnGetValue(ptr: BinaryenExpression) -> BinaryenExpression;

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

    fn BinaryenAddInt32() -> u32;
    fn BinaryenSubInt32() -> u32;
    fn BinaryenShlInt32() -> u32;
    fn BinaryenShrUInt32() -> u32;
    fn BinaryenShrSInt32() -> u32;

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
}

#[repr(C)]
struct BinaryenLiteral {
    _pad0: usize,
    _pad1: [u8; 16],
}

type BinaryenOp = i32;
