use super::{Func, FuncDecl, Global, Memory, ModuleDisplay, Signature, Table, Type};
use crate::entity::{EntityRef, EntityVec};
use crate::ir::{Debug, DebugMap, FunctionBody};
use crate::{backend, frontend};
use anyhow::Result;
use indexmap::IndexMap;

pub use crate::frontend::FrontendOptions;

#[derive(Clone, Debug)]
pub struct Module<'a> {
    pub orig_bytes: &'a [u8],
    pub funcs: EntityVec<Func, FuncDecl<'a>>,
    pub signatures: EntityVec<Signature, SignatureData>,
    pub globals: EntityVec<Global, GlobalData>,
    pub tables: EntityVec<Table, TableData>,
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
    pub memories: EntityVec<Memory, MemoryData>,
    pub start_func: Option<Func>,
    pub debug: Debug,
    pub debug_map: DebugMap,
    pub custom_sections: IndexMap<String, Vec<u8>>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SignatureData {
    pub params: Vec<Type>,
    pub returns: Vec<Type>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MemoryData {
    pub initial_pages: usize,
    pub maximum_pages: Option<usize>,
    pub segments: Vec<MemorySegment>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MemorySegment {
    pub offset: usize,
    pub data: Vec<u8>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TableData {
    pub ty: Type,
    pub initial: u32,
    pub max: Option<u32>,
    pub func_elements: Option<Vec<Func>>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GlobalData {
    pub ty: Type,
    pub value: Option<u64>,
    pub mutable: bool,
}

impl From<&wasmparser::FuncType> for SignatureData {
    fn from(fty: &wasmparser::FuncType) -> Self {
        Self {
            params: fty
                .params()
                .iter()
                .map(|&ty| ty.into())
                .collect::<Vec<Type>>(),
            returns: fty
                .results()
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

#[derive(Clone, Debug)]
pub struct Import {
    pub module: String,
    pub name: String,
    pub kind: ImportKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ImportKind {
    Table(Table),
    Func(Func),
    Global(Global),
    Memory(Memory),
}

impl std::fmt::Display for ImportKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ImportKind::Table(table) => write!(f, "{}", table)?,
            ImportKind::Func(func) => write!(f, "{}", func)?,
            ImportKind::Global(global) => write!(f, "{}", global)?,
            ImportKind::Memory(mem) => write!(f, "{}", mem)?,
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct Export {
    pub name: String,
    pub kind: ExportKind,
}

#[derive(Clone, Debug)]
pub enum ExportKind {
    Table(Table),
    Func(Func),
    Global(Global),
    Memory(Memory),
}

impl std::fmt::Display for ExportKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ExportKind::Table(table) => write!(f, "{}", table)?,
            ExportKind::Func(func) => write!(f, "{}", func)?,
            ExportKind::Global(global) => write!(f, "{}", global)?,
            ExportKind::Memory(memory) => write!(f, "{}", memory)?,
        }
        Ok(())
    }
}

impl<'a> Module<'a> {
    pub(crate) fn with_orig_bytes(orig_bytes: &'a [u8]) -> Module<'a> {
        Module {
            orig_bytes,
            funcs: EntityVec::default(),
            signatures: EntityVec::default(),
            globals: EntityVec::default(),
            tables: EntityVec::default(),
            imports: vec![],
            exports: vec![],
            memories: EntityVec::default(),
            start_func: None,
            debug: Debug::default(),
            debug_map: DebugMap::default(),
            custom_sections: IndexMap::new(),
        }
    }

    pub fn without_orig_bytes(self) -> Module<'static> {
        Module {
            orig_bytes: &[],
            funcs: EntityVec::from(
                self.funcs
                    .into_vec()
                    .into_iter()
                    .map(|decl| decl.without_orig_bytes())
                    .collect::<Vec<_>>(),
            ),
            signatures: self.signatures,
            globals: self.globals,
            tables: self.tables,
            imports: self.imports,
            exports: self.exports,
            memories: self.memories,
            start_func: self.start_func,
            debug: self.debug,
            debug_map: self.debug_map,
            custom_sections: self.custom_sections,
        }
    }
}

impl<'a> Module<'a> {
    pub(crate) fn frontend_add_table(&mut self, ty: Type, initial: u32, max: Option<u32>) -> Table {
        let func_elements = Some(vec![]);
        self.tables.push(TableData {
            ty,
            func_elements,
            initial,
            max,
        })
    }

    pub fn from_wasm_bytes(bytes: &'a [u8], options: &FrontendOptions) -> Result<Self> {
        frontend::wasm_to_ir(bytes, options)
    }

    pub fn to_wasm_bytes(&self) -> Result<Vec<u8>> {
        backend::compile(self).map(|a|a.finish())
    }
    pub fn to_encoded_module(&self) -> Result<wasm_encoder::Module>{
        backend::compile(self)
    }

    pub fn per_func_body<F: Fn(&mut FunctionBody)>(&mut self, f: F) {
        for func_decl in self.funcs.values_mut() {
            if let Some(body) = func_decl.body_mut() {
                f(body);
            }
        }
    }

    pub fn expand_func<'b>(&'b mut self, id: Func) -> Result<&'b mut FuncDecl<'a>> {
        if let FuncDecl::Lazy(..) = self.funcs[id] {
            // End the borrow. This is cheap (a slice copy).
            let mut func = self.funcs[id].clone();
            func.parse(self)?;
            self.funcs[id] = func;
        }
        Ok(&mut self.funcs[id])
    }

    pub fn clone_and_expand_body(&self, id: Func) -> Result<FunctionBody> {
        let mut body = self.funcs[id].clone();
        body.parse(self)?;
        Ok(match body {
            FuncDecl::Body(_, _, body) => body,
            _ => unreachable!(),
        })
    }

    pub fn replace_body(&mut self, id: Func, body: FunctionBody) {
        let sig = self.funcs[id].sig();
        let name = self.funcs[id].name().to_owned();
        self.funcs[id] = FuncDecl::Body(sig, name, body);
    }

    pub fn expand_all_funcs(&mut self) -> Result<()> {
        for id in 0..self.funcs.len() {
            let id = Func::new(id);
            self.expand_func(id)?;
        }
        Ok(())
    }

    pub fn display<'b>(&'b self) -> ModuleDisplay<'b>
    where
        'b: 'a,
    {
        ModuleDisplay { module: self }
    }
}
