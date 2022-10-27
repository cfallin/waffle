//! Binaryen bindings.

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

struct ExprIds {
    nop: u32,
    block: u32,
    if_: u32,
    loop_: u32,
    break_: u32,
    switch: u32,
    call: u32,
    call_indirect: u32,
    local_get: u32,
    local_set: u32,
    global_get: u32,
    global_set: u32,
    table_get: u32,
    table_set: u32,
    load: u32,
    store: u32,
    const_: u32,
    unary: u32,
    binary: u32,
    select: u32,
    drop_: u32,
    return_: u32,
    unreachable: u32,
}

impl ExprIds {
    fn get() -> Self {
        Self {
            nop: unsafe { BinaryenNopId() },
            block: unsafe { BinaryenBlockId() },
            if_: unsafe { BinaryenIfId() },
            loop_: unsafe { BinaryenLoopId() },
            break_: unsafe { BinaryenBreakId() },
            switch: unsafe { BinaryenSwitchId() },
            call: unsafe { BinaryenCallId() },
            call_indirect: unsafe { BinaryenCallIndirectId() },
            local_get: unsafe { BinaryenLocalGetId() },
            local_set: unsafe { BinaryenLocalSetId() },
            global_get: unsafe { BinaryenGlobalGetId() },
            global_set: unsafe { BinaryenGlobalSetId() },
            table_get: unsafe { BinaryenTableGetId() },
            table_set: unsafe { BinaryenTableSetId() },
            load: unsafe { BinaryenLoadId() },
            store: unsafe { BinaryenStoreId() },
            const_: unsafe { BinaryenConstId() },
            unary: unsafe { BinaryenUnaryId() },
            binary: unsafe { BinaryenBinaryId() },
            select: unsafe { BinaryenSelectId() },
            drop_: unsafe { BinaryenDropId() },
            return_: unsafe { BinaryenReturnId() },
            unreachable: unsafe { BinaryenUnreachableId() },
        }
    }
}

lazy_static! {
    static ref EXPR_IDS: ExprIds = ExprIds::get();
}

struct TypeIds {
    none_t: u32,
    i32_t: u32,
    i64_t: u32,
    f32_t: u32,
    f64_t: u32,
}

impl TypeIds {
    fn get() -> Self {
        TypeIds {
            none_t: unsafe { BinaryenTypeNone() },
            i32_t: unsafe { BinaryenTypeInt32() },
            i64_t: unsafe { BinaryenTypeInt64() },
            f32_t: unsafe { BinaryenTypeFloat32() },
            f64_t: unsafe { BinaryenTypeFloat64() },
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
}

impl Type {
    fn from_kind(kind: u32) -> Option<Type> {
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

    fn to_kind(&self) -> u32 {
        let tys = &*TYPE_IDS;
        match self {
            &Type::None => tys.none_t,
            &Type::I32 => tys.i32_t,
            &Type::I64 => tys.i64_t,
            &Type::F32 => tys.f32_t,
            &Type::F64 => tys.f64_t,
        }
    }
}

fn name_to_string(name: *const c_char) -> Option<String> {
    if name.is_null() {
        None
    } else {
        Some(unsafe { CStr::from_ptr(name).to_str().unwrap().to_string() })
    }
}

impl Expression {
    pub fn unpack(&self) -> Expr {
        if self.1.is_null() {
            return Expr::None;
        }

        let kind = unsafe { BinaryenExpressionGetId(self.1) };
        let kinds = &*EXPR_IDS;

        if kind == kinds.nop {
            Expr::Nop
        } else if kind == kinds.block {
            let name = name_to_string(unsafe { BinaryenBlockGetName(self.1) });
            let children = unsafe {
                (0..BinaryenBlockGetNumChildren(self.1))
                    .map(|i| Expression(self.0, BinaryenBlockGetChildAt(self.1, i)))
                    .collect::<Vec<_>>()
            };
            Expr::Block(name, children)
        } else if kind == kinds.if_ {
            let cond = unsafe { Expression(self.0, BinaryenIfGetCondition(self.1)) };
            let if_true = unsafe { Expression(self.0, BinaryenIfGetIfTrue(self.1)) };
            let if_false = unsafe { Expression(self.0, BinaryenIfGetIfFalse(self.1)) };
            Expr::If(cond, if_true, if_false)
        } else if kind == kinds.loop_ {
            let name = name_to_string(unsafe { BinaryenLoopGetName(self.1) });
            let value = unsafe { Expression(self.0, BinaryenLoopGetBody(self.1)) };
            Expr::Loop(name, value)
        } else if kind == kinds.break_ {
            let name = name_to_string(unsafe { BinaryenBreakGetName(self.1) }).unwrap();
            let value = unsafe { Expression(self.0, BinaryenBreakGetValue(self.1)) };
            Expr::Break(name, value)
        } else if kind == kinds.switch {
            let n = unsafe { BinaryenSwitchGetNumNames(self.1) };
            let default_name = name_to_string(unsafe { BinaryenSwitchGetDefaultName(self.1) });
            let names = (0..n)
                .map(|i| name_to_string(unsafe { BinaryenSwitchGetNameAt(self.1, i) }))
                .collect::<Vec<_>>();
            let value = unsafe { Expression(self.0, BinaryenSwitchGetValue(self.1)) };
            let cond = Expression(self.0, unsafe { BinaryenSwitchGetCondition(self.1) });
            Expr::Switch(cond, value, default_name, names)
        } else if kind == kinds.call {
            let target = name_to_string(unsafe { BinaryenCallGetTarget(self.1) }).unwrap();
            let n = unsafe { BinaryenCallGetNumOperands(self.1) };
            let args = (0..n)
                .map(|i| unsafe { Expression(self.0, BinaryenCallGetOperandAt(self.1, i)) })
                .collect::<Vec<_>>();
            Expr::Call(target, args)
        } else if kind == kinds.call_indirect {
            let target = unsafe { Expression(self.0, BinaryenCallIndirectGetTarget(self.1)) };
            let n = unsafe { BinaryenCallIndirectGetNumOperands(self.1) };
            let args = (0..n)
                .map(|i| unsafe { Expression(self.0, BinaryenCallIndirectGetOperandAt(self.1, i)) })
                .collect::<Vec<_>>();
            Expr::CallIndirect(target, args)
        } else if kind == kinds.local_get {
            let index = unsafe { BinaryenLocalGetGetIndex(self.1) };
            Expr::LocalGet(index)
        } else if kind == kinds.local_set {
            let index = unsafe { BinaryenLocalSetGetIndex(self.1) };
            let value = unsafe { Expression(self.0, BinaryenLocalSetGetValue(self.1)) };
            Expr::LocalSet(index, value)
        } else if kind == kinds.global_get {
            let name = name_to_string(unsafe { BinaryenGlobalGetGetName(self.1) }).unwrap();
            Expr::GlobalGet(name)
        } else if kind == kinds.global_set {
            let name = name_to_string(unsafe { BinaryenGlobalSetGetName(self.1) }).unwrap();
            let value = unsafe { Expression(self.0, BinaryenGlobalSetGetValue(self.1)) };
            Expr::GlobalSet(name, value)
        } else if kind == kinds.table_get {
            let name = name_to_string(unsafe { BinaryenTableGetGetTable(self.1) }).unwrap();
            let index = unsafe { Expression(self.0, BinaryenTableGetGetIndex(self.1)) };
            Expr::TableGet(name, index)
        } else if kind == kinds.table_set {
            let name = name_to_string(unsafe { BinaryenTableSetGetTable(self.1) }).unwrap();
            let index = unsafe { Expression(self.0, BinaryenTableSetGetIndex(self.1)) };
            let value = unsafe { Expression(self.0, BinaryenTableSetGetIndex(self.1)) };
            Expr::TableSet(name, index, value)
        } else if kind == kinds.load {
            let ptr = unsafe { Expression(self.0, BinaryenLoadGetPtr(self.1)) };
            let offset = unsafe { BinaryenLoadGetOffset(self.1) };
            Expr::Load(ptr, offset)
        } else if kind == kinds.store {
            let ptr = unsafe { Expression(self.0, BinaryenStoreGetPtr(self.1)) };
            let offset = unsafe { BinaryenStoreGetOffset(self.1) };
            let value = unsafe { Expression(self.0, BinaryenStoreGetValue(self.1)) };
            Expr::Store(ptr, offset, value)
        } else if kind == kinds.const_ {
            let value = match self.ty() {
                Type::None => unreachable!(),
                Type::I32 => Value::I32(unsafe { BinaryenConstGetValueI32(self.1) }),
                Type::I64 => Value::I64(unsafe { BinaryenConstGetValueI64(self.1) }),
                Type::F32 => Value::F32(unsafe { BinaryenConstGetValueF32(self.1).to_bits() }),
                Type::F64 => Value::F64(unsafe { BinaryenConstGetValueF64(self.1).to_bits() }),
            };
            Expr::Const(value)
        } else if kind == kinds.unary {
            let op = unsafe { BinaryenUnaryGetOp(self.1) };
            let value = unsafe { Expression(self.0, BinaryenUnaryGetValue(self.1)) };
            Expr::Unary(UnaryOp::from_kind(op), value)
        } else if kind == kinds.binary {
            let op = unsafe { BinaryenBinaryGetOp(self.1) };
            let left = unsafe { Expression(self.0, BinaryenBinaryGetLeft(self.1)) };
            let right = unsafe { Expression(self.0, BinaryenBinaryGetRight(self.1)) };
            Expr::Binary(BinaryOp::from_kind(op), left, right)
        } else if kind == kinds.select {
            let cond = unsafe { Expression(self.0, BinaryenSelectGetCondition(self.1)) };
            let if_true = unsafe { Expression(self.0, BinaryenSelectGetIfTrue(self.1)) };
            let if_false = unsafe { Expression(self.0, BinaryenSelectGetIfFalse(self.1)) };
            Expr::Select(cond, if_true, if_false)
        } else if kind == kinds.drop_ {
            let value = unsafe { Expression(self.0, BinaryenDropGetValue(self.1)) };
            Expr::Drop(value)
        } else if kind == kinds.return_ {
            let value = unsafe { Expression(self.0, BinaryenReturnGetValue(self.1)) };
            Expr::Return(value)
        } else if kind == kinds.unreachable {
            Expr::Unreachable
        } else {
            panic!("Unknown kind: {}", kind);
        }
    }

    pub fn ty(&self) -> Type {
        Type::from_kind(unsafe { BinaryenExpressionGetType(self.1) }).unwrap()
    }

    pub fn deep_clone(&self) -> Self {
        Expression(self.0, unsafe { BinaryenExpressionCopy(self.0, self.1) })
    }

    pub fn module(&self) -> BinaryenModule {
        self.0
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
    fn from_kind(kind: u32) -> UnaryOp {
        UnaryOp::Other(kind)
    }
}

impl BinaryOp {
    fn from_kind(kind: u32) -> BinaryOp {
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

#[derive(Clone, Debug)]
pub enum Expr {
    None,
    Nop,
    Block(Option<String>, Vec<Expression>),
    If(Expression, Expression, Expression),
    Loop(Option<String>, Expression),
    Break(String, Expression),
    Switch(Expression, Expression, Option<String>, Vec<Option<String>>),
    Call(String, Vec<Expression>),
    CallIndirect(Expression, Vec<Expression>),
    LocalGet(u32),
    LocalSet(u32, Expression),
    GlobalGet(String),
    GlobalSet(String, Expression),
    TableGet(String, Expression),
    TableSet(String, Expression, Expression),
    Load(Expression, u32),
    Store(Expression, u32, Expression),
    Const(Value),
    Unary(UnaryOp, Expression),
    Binary(BinaryOp, Expression, Expression),
    Select(Expression, Expression, Expression),
    Drop(Expression),
    Return(Expression),
    Unreachable,
}

impl Expr {
    pub fn visit_children<F: FnMut(Expression)>(&self, mut f: F) {
        self.visit_children_and_ret(|e| {
            f(e);
            let ret: Option<()> = None;
            ret
        });
    }

    pub fn visit_children_and_ret<R, F: FnMut(Expression) -> Option<R>>(
        &self,
        mut f: F,
    ) -> Option<R> {
        match self {
            &Expr::None => None,
            &Expr::Block(_, ref subexprs) => {
                for e in subexprs {
                    if let Some(ret) = f(*e) {
                        return Some(ret);
                    }
                }
                None
            }
            &Expr::If(cond, if_true, if_false) => {
                if let Some(ret) = f(cond) {
                    return Some(ret);
                }
                if let Some(ret) = f(if_true) {
                    return Some(ret);
                }
                if let Some(ret) = f(if_false) {
                    return Some(ret);
                }
                None
            }
            &Expr::Loop(_, body) => f(body),
            &Expr::Drop(expr) => f(expr),

            &Expr::Break(_, value) => f(value),
            &Expr::Switch(index, value, ..) => {
                if let Some(ret) = f(index) {
                    return Some(ret);
                }
                if let Some(ret) = f(value) {
                    return Some(ret);
                }
                None
            }
            &Expr::Call(_, ref ops) => {
                for op in ops {
                    if let Some(ret) = f(*op) {
                        return Some(ret);
                    }
                }
                None
            }
            &Expr::CallIndirect(target, ref ops) => {
                if let Some(ret) = f(target) {
                    return Some(ret);
                }
                for op in ops {
                    if let Some(ret) = f(*op) {
                        return Some(ret);
                    }
                }
                None
            }
            &Expr::LocalGet(_) => None,
            &Expr::LocalSet(_, expr) => f(expr),
            &Expr::GlobalGet(_) => None,
            &Expr::GlobalSet(_, expr) => f(expr),
            &Expr::TableGet(_, index) => f(index),
            &Expr::TableSet(_, index, value) => {
                if let Some(val) = f(index) {
                    return Some(val);
                }
                if let Some(val) = f(value) {
                    return Some(val);
                }
                None
            }
            &Expr::Load(ptr, _) => f(ptr),
            &Expr::Store(ptr, _, value) => {
                if let Some(ret) = f(ptr) {
                    return Some(ret);
                }
                if let Some(ret) = f(value) {
                    return Some(ret);
                }
                None
            }
            &Expr::Const(_) => None,
            &Expr::Unary(_, value) => f(value),
            &Expr::Binary(_, left, right) => {
                if let Some(ret) = f(left) {
                    return Some(ret);
                }
                if let Some(ret) = f(right) {
                    return Some(ret);
                }
                None
            }
            &Expr::Select(cond, if_true, if_false) => {
                if let Some(ret) = f(cond) {
                    return Some(ret);
                }
                if let Some(ret) = f(if_true) {
                    return Some(ret);
                }
                if let Some(ret) = f(if_false) {
                    return Some(ret);
                }
                None
            }
            &Expr::Return(expr) => f(expr),
            &Expr::Unreachable => None,
            &Expr::Nop => None,
        }
    }

    pub fn to_expression(&self, module: BinaryenModule) -> Expression {
        match self {
            &Expr::Const(Value::I32(value)) => unsafe {
                let literal = BinaryenLiteralInt32(value);
                Expression(module, BinaryenConst(module, literal))
            },
            &Expr::LocalSet(idx, value) => unsafe {
                Expression(module, BinaryenLocalSet(module, idx, value.1))
            },
            _ => unimplemented!(),
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
    fn BinaryenGetExport(ptr: BinaryenModule, name: *const c_char) -> BinaryenExport;
    fn BinaryenGetNumExports(ptr: BinaryenModule) -> u32;
    fn BinaryenGetExportByIndex(ptr: BinaryenModule, index: u32) -> BinaryenExport;
    fn BinaryenExportGetName(ptr: BinaryenFunction) -> *const c_char;
    fn BinaryenExportGetValue(ptr: BinaryenFunction) -> *const c_char;
    fn BinaryenExportGetKind(ptr: BinaryenFunction) -> u32;
    fn BinaryenExternalFunction() -> u32;

    fn BinaryenExpressionGetId(ptr: BinaryenExpression) -> u32;
    fn BinaryenExpressionGetType(ptr: BinaryenExpression) -> u32;
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

    fn BinaryenNopId() -> u32;
    fn BinaryenBlockId() -> u32;
    fn BinaryenIfId() -> u32;
    fn BinaryenLoopId() -> u32;
    fn BinaryenBreakId() -> u32;
    fn BinaryenSwitchId() -> u32;
    fn BinaryenCallId() -> u32;
    fn BinaryenCallIndirectId() -> u32;
    fn BinaryenLocalGetId() -> u32;
    fn BinaryenLocalSetId() -> u32;
    fn BinaryenGlobalGetId() -> u32;
    fn BinaryenGlobalSetId() -> u32;
    fn BinaryenTableGetId() -> u32;
    fn BinaryenTableSetId() -> u32;
    fn BinaryenLoadId() -> u32;
    fn BinaryenStoreId() -> u32;
    fn BinaryenConstId() -> u32;
    fn BinaryenUnaryId() -> u32;
    fn BinaryenBinaryId() -> u32;
    fn BinaryenSelectId() -> u32;
    fn BinaryenDropId() -> u32;
    fn BinaryenReturnId() -> u32;
    fn BinaryenMemorySizeId() -> u32;
    fn BinaryenMemoryGrowId() -> u32;
    fn BinaryenUnreachableId() -> u32;
    fn BinaryenPopId() -> u32;

    fn BinaryenTypeNone() -> u32;
    fn BinaryenTypeInt32() -> u32;
    fn BinaryenTypeInt64() -> u32;
    fn BinaryenTypeFloat32() -> u32;
    fn BinaryenTypeFloat64() -> u32;

    fn BinaryenAddInt32() -> u32;
    fn BinaryenSubInt32() -> u32;
    fn BinaryenShlInt32() -> u32;
    fn BinaryenShrUInt32() -> u32;
    fn BinaryenShrSInt32() -> u32;

    fn BinaryenConst(module: BinaryenModule, lit: BinaryenLiteral) -> BinaryenExpression;
    fn BinaryenLocalSet(
        module: BinaryenModule,
        index: u32,
        value: BinaryenExpression,
    ) -> BinaryenExpression;

    fn BinaryenLiteralInt32(x: i32) -> BinaryenLiteral;
    fn BinaryenLiteralInt64(x: i64) -> BinaryenLiteral;
    fn BinaryenLiteralFloat32Bits(x: i32) -> BinaryenLiteral;
    fn BinaryenLiteralFloat64Bits(x: i64) -> BinaryenLiteral;
}

#[repr(C)]
struct BinaryenLiteral {
    _pad0: usize,
    _pad1: [u8; 16],
}
