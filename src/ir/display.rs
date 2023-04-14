//! Displaying IR.

use super::{FuncDecl, FunctionBody, Module, SourceLoc, ValueDef};
use crate::entity::EntityRef;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};

pub struct FunctionBodyDisplay<'a>(
    pub(crate) &'a FunctionBody,
    pub(crate) &'a str,
    pub(crate) bool,
    pub(crate) Option<&'a Module<'a>>,
);

impl<'a> Display for FunctionBodyDisplay<'a> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        let arg_tys = self
            .0
            .locals
            .values()
            .take(self.0.n_params)
            .map(|&ty| format!("{}", ty))
            .collect::<Vec<_>>();
        let ret_tys = self
            .0
            .rets
            .iter()
            .map(|&ty| format!("{}", ty))
            .collect::<Vec<_>>();
        writeln!(
            f,
            "{}function({}) -> {} {{",
            self.1,
            arg_tys.join(", "),
            ret_tys.join(", ")
        )?;

        let verbose = self.2;
        for (value, value_def) in self.0.values.entries() {
            match value_def {
                ValueDef::Operator(op, args, tys) if verbose => writeln!(
                    f,
                    "{}    {} = {} {} # {}",
                    self.1,
                    value,
                    op,
                    args.iter()
                        .map(|arg| format!("{}", arg))
                        .collect::<Vec<_>>()
                        .join(", "),
                    tys.iter()
                        .map(|arg| format!("{}", arg))
                        .collect::<Vec<_>>()
                        .join(", ")
                )?,
                ValueDef::BlockParam(block, idx, ty) if verbose => writeln!(
                    f,
                    "{}    {} = blockparam {}, {} # {}",
                    self.1, value, block, idx, ty
                )?,
                ValueDef::Alias(alias_target) => {
                    if verbose {
                        writeln!(f, "{}    {} = {}", self.1, value, alias_target)?
                    }
                }
                ValueDef::PickOutput(val, idx, ty) => {
                    writeln!(f, "{}    {} = {}.{} # {}", self.1, value, val, idx, ty)?
                }
                ValueDef::Placeholder(ty) => {
                    writeln!(f, "{}    {} = placeholder # {}", self.1, value, ty)?
                }
                ValueDef::None => panic!(),
                _ => {}
            }
        }

        for (block_id, block) in self.0.blocks.entries() {
            let block_params = block
                .params
                .iter()
                .map(|(ty, val)| format!("{}: {}", val, ty))
                .collect::<Vec<_>>();
            writeln!(
                f,
                "{}  {}({}): # {}",
                self.1,
                block_id,
                block_params.join(", "),
                block.desc
            )?;
            writeln!(
                f,
                "{}    # preds: {}",
                self.1,
                block
                    .preds
                    .iter()
                    .map(|pred| format!("{} ({})", pred, self.0.blocks[*pred].desc))
                    .collect::<Vec<_>>()
                    .join(", ")
            )?;
            writeln!(
                f,
                "{}    # succs: {}",
                self.1,
                block
                    .succs
                    .iter()
                    .map(|succ| format!("{} ({})", succ, self.0.blocks[*succ].desc))
                    .collect::<Vec<_>>()
                    .join(", ")
            )?;
            for (_, param) in &block.params {
                if let Some(local) = self.0.value_locals[*param] {
                    writeln!(f, "{}    # {}: {}", self.1, param, local)?;
                }
            }
            for &inst in &block.insts {
                if let Some(local) = self.0.value_locals[inst] {
                    writeln!(f, "{}    # {}: {}", self.1, inst, local)?;
                }
                match &self.0.values[inst] {
                    ValueDef::Operator(op, args, tys) => {
                        let args = args.iter().map(|&v| format!("{}", v)).collect::<Vec<_>>();
                        let tys = tys.iter().map(|&ty| format!("{}", ty)).collect::<Vec<_>>();
                        let loc = if self.0.source_locs[inst] != SourceLoc::invalid()
                            && self.3.is_some()
                        {
                            let module = self.3.as_ref().unwrap();
                            let loc = self.0.source_locs[inst];
                            let data = &module.debug.source_locs[loc];
                            let filename = &module.debug.source_files[data.file];
                            format!("@{} {}:{}:{}", loc, filename, data.line, data.col)
                        } else {
                            "".to_owned()
                        };
                        writeln!(
                            f,
                            "{}    {} = {} {} # {} {}",
                            self.1,
                            inst,
                            op,
                            args.join(", "),
                            tys.join(", "),
                            loc,
                        )?;
                    }
                    ValueDef::PickOutput(val, idx, ty) => {
                        writeln!(f, "{}    {} = {}.{} # {}", self.1, inst, val, idx, ty)?;
                    }
                    ValueDef::Alias(val) => {
                        writeln!(f, "{}    {} = {}", self.1, inst, val)?;
                    }
                    _ => unreachable!(),
                }
            }
            writeln!(f, "{}    {}", self.1, block.terminator)?;
        }

        writeln!(f, "{}}}", self.1)?;

        Ok(())
    }
}

pub struct ModuleDisplay<'a>(pub(crate) &'a Module<'a>);

impl<'a> Display for ModuleDisplay<'a> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        writeln!(f, "module {{")?;
        if let Some(func) = self.0.start_func {
            writeln!(f, "    start = {}", func)?;
        }
        let mut sig_strs = HashMap::new();
        for (sig, sig_data) in self.0.signatures.entries() {
            let arg_tys = sig_data
                .params
                .iter()
                .map(|&ty| format!("{}", ty))
                .collect::<Vec<_>>();
            let ret_tys = sig_data
                .returns
                .iter()
                .map(|&ty| format!("{}", ty))
                .collect::<Vec<_>>();
            let sig_str = format!("{} -> {}", arg_tys.join(", "), ret_tys.join(", "));
            sig_strs.insert(sig, sig_str.clone());
            writeln!(f, "  {}: {}", sig, sig_str)?;
        }
        for (global, global_data) in self.0.globals.entries() {
            writeln!(
                f,
                "  {}: {:?} # {}",
                global, global_data.value, global_data.ty
            )?;
        }
        for (table, table_data) in self.0.tables.entries() {
            writeln!(f, "  {}: {}", table, table_data.ty)?;
            if let Some(funcs) = &table_data.func_elements {
                for (i, &func) in funcs.iter().enumerate() {
                    writeln!(f, "    {}[{}]: {}", table, i, func)?;
                }
            }
        }
        for (memory, memory_data) in self.0.memories.entries() {
            writeln!(
                f,
                "  {}: initial {} max {:?}",
                memory, memory_data.initial_pages, memory_data.maximum_pages
            )?;
            for seg in &memory_data.segments {
                writeln!(
                    f,
                    "    {} offset {}: # {} bytes",
                    memory,
                    seg.offset,
                    seg.data.len()
                )?;
            }
        }
        for import in &self.0.imports {
            writeln!(
                f,
                "  import \"{}\".\"{}\": {}",
                import.module, import.name, import.kind
            )?;
        }
        for export in &self.0.exports {
            writeln!(f, "  export \"{}\": {}", export.name, export.kind)?;
        }
        for (func, func_decl) in self.0.funcs.entries() {
            match func_decl {
                FuncDecl::Body(sig, name, body) => {
                    writeln!(
                        f,
                        "  {} \"{}\": {} = # {}",
                        func,
                        name,
                        sig,
                        sig_strs.get(&sig).unwrap()
                    )?;
                    writeln!(f, "{}", body.display("    ", Some(self.0)))?;
                }
                FuncDecl::Lazy(sig, name, reader) => {
                    writeln!(
                        f,
                        "  {} \"{}\": {} = # {}",
                        func,
                        name,
                        sig,
                        sig_strs.get(&sig).unwrap()
                    )?;
                    writeln!(f, "  # raw bytes (length {})", reader.range().len())?;
                }
                FuncDecl::Compiled(sig, name, bytes) => {
                    writeln!(
                        f,
                        "  {} \"{}\": {} = # {}",
                        func,
                        name,
                        sig,
                        sig_strs.get(&sig).unwrap()
                    )?;
                    writeln!(f, "  # already compiled (length {})", bytes.len())?;
                }
                FuncDecl::Import(sig, name) => {
                    writeln!(
                        f,
                        "  {} \"{}\": {} # {}",
                        func,
                        name,
                        sig,
                        sig_strs.get(&sig).unwrap()
                    )?;
                }
                FuncDecl::None => {
                    writeln!(f, "  {}: none", func)?;
                }
            }
        }
        for (loc, loc_data) in self.0.debug.source_locs.entries() {
            writeln!(
                f,
                "  {} = {} line {} column {}",
                loc, loc_data.file, loc_data.line, loc_data.col
            )?;
        }
        for (file, file_name) in self.0.debug.source_files.entries() {
            writeln!(f, "  {} = \"{}\"", file, file_name)?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}
