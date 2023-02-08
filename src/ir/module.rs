use super::{Func, FuncDecl, Global, Memory, ModuleDisplay, Signature, Table, Type};
use crate::entity::{EntityRef, EntityVec};
use crate::ir::debug::DebugMap;
use crate::ir::FunctionBody;
use crate::{backend, frontend};
use anyhow::Result;

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
    pub debug_map: DebugMap,
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

#[derive(Clone, Debug)]
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
            debug_map: DebugMap::default(),
        }
    }
}

impl<'a> Module<'a> {
    pub(crate) fn frontend_add_table(&mut self, ty: Type, max: Option<u32>) -> Table {
        let func_elements = if ty == Type::FuncRef {
            Some(vec![])
        } else {
            None
        };
        self.tables.push(TableData {
            ty,
            func_elements,
            max,
        })
    }

    pub fn from_wasm_bytes(bytes: &'a [u8]) -> Result<Self> {
        frontend::wasm_to_ir(bytes)
    }

    pub fn to_wasm_bytes(&self) -> Result<Vec<u8>> {
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
        ModuleDisplay(self)
    }
}
