//! Displaying IR.

use super::{FuncDecl, FunctionBody, Module, ValueDef};

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

        for (block_id, block) in self.0.blocks.entries() {
            let block_params = block
                .params
                .iter()
                .map(|(ty, val)| format!("{}: {}", val, ty))
                .collect::<Vec<_>>();
            writeln!(f, "{}  {}({}):", self.1, block_id, block_params.join(", "))?;
            for &pred in &block.preds {
                writeln!(f, "{}    # pred: {}", self.1, pred)?;
            }
            for &succ in &block.succs {
                writeln!(f, "{}    # succ: {}", self.1, succ)?;
            }
            for &inst in &block.insts {
                let inst = self.0.resolve_alias(inst);
                match &self.0.values[inst] {
                    ValueDef::Operator(op, args, tys) => {
                        let args = args
                            .iter()
                            .map(|&v| {
                                let v = self.0.resolve_alias(v);
                                format!("{}", v)
                            })
                            .collect::<Vec<_>>();
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
        }

        writeln!(f, "}}")?;

        Ok(())
    }
}

pub struct ModuleDisplay<'a>(pub(crate) &'a Module<'a>);

impl<'a> Display for ModuleDisplay<'a> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        writeln!(f, "module {{")?;
        for (func, func_decl) in self.0.funcs() {
            match func_decl {
                FuncDecl::Body(sig, body) => {
                    writeln!(f, "  {}: {} =", func, sig)?;
                    writeln!(f, "{}", body.display("    "))?;
                }
                FuncDecl::Import(sig) => {
                    writeln!(f, "  {}: {}", func, sig)?;
                }
            }
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}
