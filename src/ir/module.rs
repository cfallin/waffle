use super::{Func, FuncDecl, Global, ModuleDisplay, Signature, Table, Type};
use crate::entity::EntityVec;
use crate::frontend;
use anyhow::Result;
use fxhash::FxHashSet;

#[derive(Clone, Debug, Default)]
pub struct Module<'a> {
    orig_bytes: &'a [u8],
    funcs: EntityVec<Func, FuncDecl>,
    signatures: EntityVec<Signature, SignatureData>,
    globals: EntityVec<Global, Type>,
    tables: EntityVec<Table, Type>,

    dirty_funcs: FxHashSet<Func>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SignatureData {
    pub params: Vec<Type>,
    pub returns: Vec<Type>,
}

impl From<&wasmparser::FuncType> for SignatureData {
    fn from(fty: &wasmparser::FuncType) -> Self {
        Self {
            params: fty
                .params
                .iter()
                .map(|&ty| ty.into())
                .collect::<Vec<Type>>(),
            returns: fty
                .returns
                .iter()
                .map(|&ty| ty.into())
                .collect::<Vec<Type>>(),
        }
    }
}
impl From<wasmparser::FuncType> for SignatureData {
    fn from(fty: wasmparser::FuncType) -> Self {
        (&fty).into()
    }
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
    pub fn funcs<'b>(&'b self) -> impl Iterator<Item = (Func, &'b FuncDecl)> {
        self.funcs.entries()
    }
    pub fn signature<'b>(&'b self, id: Signature) -> &'b SignatureData {
        &self.signatures[id]
    }
    pub fn signatures<'b>(&'b self) -> impl Iterator<Item = (Signature, &'b SignatureData)> {
        self.signatures.entries()
    }
    pub fn global_ty(&self, id: Global) -> Type {
        self.globals[id]
    }
    pub fn globals<'b>(&'b self) -> impl Iterator<Item = (Global, Type)> + 'b {
        self.globals.entries().map(|(id, ty)| (id, *ty))
    }
    pub fn table_ty(&self, id: Table) -> Type {
        self.tables[id]
    }
    pub fn tables<'b>(&'b self) -> impl Iterator<Item = (Table, Type)> + 'b {
        self.tables.entries().map(|(id, ty)| (id, *ty))
    }

    pub(crate) fn frontend_add_signature(&mut self, ty: SignatureData) {
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
        let mut module = frontend::wasm_to_ir(bytes)?;
        for func_decl in module.funcs.values_mut() {
            if let Some(body) = func_decl.body_mut() {
                crate::passes::rpo::run(body);
                crate::passes::resolve_aliases::run(body);
            }
        }
        Ok(module)
    }

    pub fn to_wasm_bytes(&self) -> Result<Vec<u8>> {
        todo!()
    }

    pub fn display<'b>(&'b self) -> ModuleDisplay<'b>
    where
        'b: 'a,
    {
        ModuleDisplay(self)
    }
}
