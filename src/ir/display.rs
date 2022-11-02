//! Displaying IR.

use super::{FuncDecl, FunctionBody, Module, ValueDef};
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};

pub struct FunctionBodyDisplay<'a>(pub(crate) &'a FunctionBody, pub(crate) &'a str);

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

        for (value, value_def) in self.0.values.entries() {
            match value_def {
                ValueDef::Operator(..) | ValueDef::BlockParam(..) => {}
                ValueDef::Alias(_alias_target) => {}
                ValueDef::PickOutput(val, idx, ty) => {
                    writeln!(f, "{}    {} = {}.{} # {}", self.1, value, val, idx, ty)?
                }
                ValueDef::Placeholder(ty) => {
                    writeln!(f, "{}    {} = placeholder # {}", self.1, value, ty)?
                }
                ValueDef::None => panic!(),
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
        for (global, global_ty) in self.0.globals() {
            writeln!(f, "  {}: {}", global, global_ty)?;
        }
        for (table, table_ty) in self.0.tables() {
            writeln!(f, "  {}: {}", table, table_ty)?;
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
