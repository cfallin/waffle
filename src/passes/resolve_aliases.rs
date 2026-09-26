//! Resolve all aliases.

use crate::{FunctionBody, ValueDef};

pub fn run(body: &mut FunctionBody) {
    log::debug!(
        "Resolve aliases: running on:\n{}\n",
        body.display_verbose("| ", None),
    );
    for value in body.values.iter() {
        let mut value_def = std::mem::take(&mut body.values[value]);
        match &mut value_def {
            ValueDef::Operator(_, args, _) => {
                for i in 0..args.len() {
                    let val = body.arg_pool[*args][i];
                    let val = body.resolve_and_update_alias(val);
                    body.arg_pool[*args][i] = val;
                }
            }
            ValueDef::PickOutput(val, _idx, _ty) => {
                *val = body.resolve_alias(*val);
            }
            ValueDef::Alias(val) => {
                *val = body.resolve_alias(*val);
            }
            _ => {}
        }
        body.values[value] = value_def;
    }
    // Every use in a terminator: branch args, and also a `CondBr`'s
    // condition, a `Select`'s value and `Return`'s values. (Resolving only
    // the branch args left aliases behind in the others, which the
    // reducifier then failed to remap when it duplicated the block.)
    let mut blocks = std::mem::take(&mut body.blocks);
    for block in blocks.values_mut() {
        block.terminator.update_uses(|u| {
            *u = body.resolve_alias(*u);
        });
    }
    body.blocks = blocks;
}

#[cfg(test)]
mod test {
    use crate::{BlockTarget, FunctionBody, Module, SignatureData, Terminator, Type, ValueDef};

    /// Aliases are resolved in every terminator use, not only in branch
    /// args: here a `CondBr`'s condition and a `Return`'s value.
    #[test]
    fn terminator_uses() {
        let mut module = Module::empty();
        let sig = module.signatures.push(SignatureData {
            params: vec![Type::I32],
            returns: vec![Type::I32],
        });
        let mut body = FunctionBody::new(&module, sig);
        let entry = body.entry;
        let exit = body.add_block();
        let arg = body.blocks[entry].params[0].1;
        let alias = body.add_value(ValueDef::Alias(arg));
        body.set_terminator(
            entry,
            Terminator::CondBr {
                cond: alias,
                if_true: BlockTarget {
                    block: exit,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: exit,
                    args: vec![],
                },
            },
        );
        body.set_terminator(
            exit,
            Terminator::Return {
                values: vec![alias],
            },
        );
        super::run(&mut body);
        match &body.blocks[entry].terminator {
            Terminator::CondBr { cond, .. } => assert_eq!(*cond, arg),
            t => panic!("{t:?}"),
        }
        match &body.blocks[exit].terminator {
            Terminator::Return { values } => assert_eq!(values, &vec![arg]),
            t => panic!("{t:?}"),
        }
    }
}
