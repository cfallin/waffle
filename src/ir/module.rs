use super::{Func, FuncDecl, Global, Signature, Table};
use crate::entity::EntityVec;
use crate::frontend;
use anyhow::Result;
use fxhash::FxHashSet;
use wasmparser::{FuncType, Type};

#[derive(Clone, Debug, Default)]
pub struct Module<'a> {
    orig_bytes: &'a [u8],
    funcs: EntityVec<Func, FuncDecl>,
    signatures: EntityVec<Signature, FuncType>,
    globals: EntityVec<Global, Type>,
    tables: EntityVec<Table, Type>,

    dirty_funcs: FxHashSet<Func>,
}

impl<'a> Module<'a> {
    pub(crate) fn with_orig_bytes(orig_bytes: &'a [u8]) -> Module<'a> {
        let mut m = Module::default();
        m.orig_bytes = orig_bytes;
        m
    }
}

impl<'a> Module<'a> {
    pub fn func<'b>(&'b self, id: Func) -> &'b FuncDecl {
        &self.funcs[id]
    }
    pub fn func_mut<'b>(&'b mut self, id: Func) -> &'b mut FuncDecl {
        self.dirty_funcs.insert(id);
        &mut self.funcs[id]
    }
    pub fn signature<'b>(&'b self, id: Signature) -> &'b FuncType {
        &self.signatures[id]
    }
    pub fn global_ty(&self, id: Global) -> Type {
        self.globals[id]
    }
    pub fn table_ty(&self, id: Table) -> Type {
        self.tables[id]
    }

    pub(crate) fn frontend_add_signature(&mut self, ty: FuncType) {
        self.signatures.push(ty);
    }
    pub(crate) fn frontend_add_func(&mut self, body: FuncDecl) {
        self.funcs.push(body);
    }
    pub(crate) fn frontend_add_table(&mut self, ty: Type) {
        self.tables.push(ty);
    }
    pub(crate) fn frontend_add_global(&mut self, ty: Type) {
        self.globals.push(ty);
    }

    pub fn from_wasm_bytes(bytes: &'a [u8]) -> Result<Self> {
        frontend::wasm_to_ir(bytes)
    }

    pub fn to_wasm_bytes(&self) -> Result<Vec<u8>> {
        todo!()
    }
}
