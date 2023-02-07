use super::{Func, FuncDecl, Global, Memory, ModuleDisplay, Signature, Table, Type};
use crate::entity::EntityVec;
use crate::ir::FunctionBody;
use crate::{backend, frontend};
use anyhow::Result;

#[derive(Clone, Debug)]
pub struct Module<'a> {
    orig_bytes: &'a [u8],
    funcs: EntityVec<Func, FuncDecl>,
    signatures: EntityVec<Signature, SignatureData>,
    globals: EntityVec<Global, GlobalData>,
    tables: EntityVec<Table, TableData>,
    imports: Vec<Import>,
    exports: Vec<Export>,
    memories: EntityVec<Memory, MemoryData>,
    pub start_func: Option<Func>,
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
        }
    }
}

impl<'a> Module<'a> {
    pub fn func<'b>(&'b self, id: Func) -> &'b FuncDecl {
        &self.funcs[id]
    }
    pub fn func_mut<'b>(&'b mut self, id: Func) -> &'b mut FuncDecl {
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
    pub fn global<'b>(&'b self, id: Global) -> &'b GlobalData {
        &self.globals[id]
    }
    pub fn globals<'b>(&'b self) -> impl Iterator<Item = (Global, &'b GlobalData)> + 'b {
        self.globals.entries()
    }
    pub fn table<'b>(&'b self, id: Table) -> &'b TableData {
        &self.tables[id]
    }
    pub fn tables<'b>(&'b self) -> impl Iterator<Item = (Table, &'b TableData)> + 'b {
        self.tables.entries()
    }
    pub fn memories<'b>(&'b self) -> impl Iterator<Item = (Memory, &'b MemoryData)> + 'b {
        self.memories.entries()
    }
    pub fn imports<'b>(&'b self) -> impl Iterator<Item = &'b Import> + 'b {
        self.imports.iter()
    }
    pub fn exports<'b>(&'b self) -> impl Iterator<Item = &'b Export> + 'b {
        self.exports.iter()
    }
    pub fn table_mut<'b>(&'b mut self, table: Table) -> &'b mut TableData {
        &mut self.tables[table]
    }
    pub fn memory<'b>(&'b self, memory: Memory) -> &'b MemoryData {
        &self.memories[memory]
    }

    pub fn memory_mut<'b>(&'b mut self, memory: Memory) -> &'b mut MemoryData {
        &mut self.memories[memory]
    }

    pub(crate) fn frontend_add_signature(&mut self, ty: SignatureData) {
        self.signatures.push(ty);
    }
    pub(crate) fn frontend_add_func(&mut self, body: FuncDecl) -> Func {
        self.funcs.push(body)
    }
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
    pub(crate) fn frontend_add_global(&mut self, global: GlobalData) -> Global {
        self.globals.push(global)
    }
    pub(crate) fn frontend_add_import(&mut self, import: Import) {
        self.imports.push(import);
    }
    pub(crate) fn frontend_add_export(&mut self, export: Export) {
        self.exports.push(export);
    }
    pub(crate) fn frontend_add_memory(&mut self, memory: MemoryData) -> Memory {
        self.memories.push(memory)
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

    pub fn optimize(&mut self) {
        self.per_func_body(|body| {
            let cfg = crate::cfg::CFGInfo::new(body);
            crate::passes::basic_opt::gvn(body, &cfg);
            crate::passes::resolve_aliases::run(body);
            crate::passes::empty_blocks::run(body);
        });
    }

    pub fn convert_to_max_ssa(&mut self) {
        self.per_func_body(|body| {
            let cfg = crate::cfg::CFGInfo::new(body);
            crate::passes::maxssa::run(body, &cfg);
        });
    }

    pub fn display<'b>(&'b self) -> ModuleDisplay<'b>
    where
        'b: 'a,
    {
        ModuleDisplay(self)
    }

    pub fn add_func(&mut self, sig: Signature, func: FunctionBody) -> Func {
        self.funcs.push(FuncDecl::Body(sig, func))
    }
}
