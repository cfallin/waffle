//! Displaying IR.

use super::{FuncDecl, FunctionBody, Module, ValueDef};
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};

pub struct FunctionBodyDisplay<'a>(
    pub(crate) &'a FunctionBody,
    pub(crate) &'a str,
    pub(crate) bool,
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
            writeln!(f, "{}  {}({}):", self.1, block_id, block_params.join(", "))?;
            writeln!(
                f,
                "{}    # preds: {}",
                self.1,
                block
                    .preds
                    .iter()
                    .map(|pred| format!("{}", pred))
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
                    .map(|succ| format!("{}", succ))
                    .collect::<Vec<_>>()
                    .join(", ")
            )?;
            for &inst in &block.insts {
                match &self.0.values[inst] {
                    ValueDef::Operator(op, args, tys) => {
                        let args = args.iter().map(|&v| format!("{}", v)).collect::<Vec<_>>();
                        let tys = tys.iter().map(|&ty| format!("{}", ty)).collect::<Vec<_>>();
                        writeln!(
                            f,
                            "{}    {} = {} {} # {}",
                            self.1,
                            inst,
                            op,
                            args.join(", "),
                            tys.join(", ")
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
        let mut sig_strs = HashMap::new();
        for (sig, sig_data) in self.0.signatures() {
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
        for (global, global_data) in self.0.globals() {
            writeln!(
                f,
                "  {}: {:?} # {}",
                global, global_data.value, global_data.ty
            )?;
        }
        for (table, table_data) in self.0.tables() {
            writeln!(f, "  {}: {}", table, table_data.ty)?;
            if let Some(funcs) = &table_data.func_elements {
                for (i, &func) in funcs.iter().enumerate() {
                    writeln!(f, "    {}[{}]: {}", table, i, func)?;
                }
            }
        }
        for (memory, memory_data) in self.0.memories() {
            writeln!(
                f,
                "  {}: initial {} max {:?}",
                memory, memory_data.initial_pages, memory_data.maximum_pages
            )?;
            for seg in &memory_data.segments {
                writeln!(
                    f,
                    "    {} offset {}: [{}]",
                    memory,
                    seg.offset,
                    seg.data
                        .iter()
                        .map(|&byte| format!("0x{:02x}", byte))
                        .collect::<Vec<_>>()
                        .join(", ")
                )?;
            }
        }
        for import in self.0.imports() {
            writeln!(
                f,
                "  import \"{}\".\"{}\": {}",
                import.module, import.name, import.kind
            )?;
        }
        for export in self.0.exports() {
            writeln!(f, "  export \"{}\": {}", export.name, export.kind)?;
        }
        for (func, func_decl) in self.0.funcs() {
            match func_decl {
                FuncDecl::Body(sig, body) => {
                    writeln!(f, "  {}: {} = # {}", func, sig, sig_strs.get(&sig).unwrap())?;
                    writeln!(f, "{}", body.display("    "))?;
                }
                FuncDecl::Import(sig) => {
                    writeln!(f, "  {}: {} # {}", func, sig, sig_strs.get(&sig).unwrap())?;
                }
            }
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}
