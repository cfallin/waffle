//! Remove-useless-phis (blockparams) pass.

use crate::cfg::CFGInfo;
use crate::ir::*;

fn all_equal(mut vals: impl Iterator<Item = Value>) -> Option<Value> {
    match vals.next() {
        Some(val) if vals.all(|other_val| other_val == val) => Some(val),
        _ => None,
    }
}

fn delete_indices<T: Copy>(vec: &mut Vec<T>, indices: &[usize]) {
    let mut out = 0;
    let mut indices_idx = 0;
    for i in 0..vec.len() {
        if indices_idx < indices.len() && indices[indices_idx] == i {
            indices_idx += 1;
            // Deleted!
        } else {
            if out < i {
                vec[out] = vec[i];
            }
            out += 1;
        }
    }
    if out < vec.len() {
        vec.truncate(out);
    }
}

pub fn run(func: &mut FunctionBody, cfg: &CFGInfo) {
    // For every block, collect the arg-lists of preds. If a given
    // blockparam has all the same values for an arg, replace the
    // blockparam value with an alias to that one value, and then
    // remove it from the blockparams and target-lists of all preds.

    log::trace!(
        "remove_phis: running on func:\n{}\n",
        func.display_verbose("| ", None)
    );

    let mut deleted = vec![];
    for &block in cfg.rpo.values() {
        // Skip the entry block -- we can't remove any args, because
        // there is also an implicit in-edge from the function entry
        // with arguments.
        if block == func.entry {
            continue;
        }

        deleted.clear();

        // Gather arg-lists from each pred's terminator.
        let mut arglists = vec![];
        for (i, &pred) in func.blocks[block].preds.iter().enumerate() {
            let pos = func.blocks[block].pos_in_pred_succ[i];
            func.blocks[pred].terminator.visit_target(pos, |target| {
                assert_eq!(target.block, block);
                assert_eq!(target.args.len(), func.blocks[block].params.len());
                arglists.push(target.args.clone());
            });
        }

        // For each arg-position, check if all args are the same. If
        // so, rewrite value and mark index as deleted.
        for i in 0..func.blocks[block].params.len() {
            let blockparam = func.blocks[block].params[i].1;
            let same = all_equal(
                arglists
                    .iter()
                    .map(|arglist| func.resolve_alias(arglist[i])),
            );
            if let Some(val) = same {
                if val != blockparam {
                    log::trace!(
                        "deleting blockparam {} from block {}: now {}",
                        blockparam,
                        block,
                        val
                    );
                    func.values[blockparam] = ValueDef::Alias(val);
                    deleted.push(i);
                }
            }
        }

        // If anything was deleted, remove the appropriate indices in
        // the func's blockparams list and in targets' arg lists.
        if !deleted.is_empty() {
            delete_indices(&mut func.blocks[block].params, &deleted[..]);
            for i in 0..func.blocks[block].preds.len() {
                let pred = func.blocks[block].preds[i];
                let pos = func.blocks[block].pos_in_pred_succ[i];
                func.blocks[pred].terminator.update_target(pos, |target| {
                    delete_indices(&mut target.args, &deleted[..]);
                });
            }

            // Renumber blockparam values.
            for (i, &(_, param)) in func.blocks[block].params.iter().enumerate() {
                let ty = func.values[param].ty(&func.type_pool).unwrap();
                func.values[param] = ValueDef::BlockParam(block, i as u32, ty);
            }
        }
    }

    log::trace!("remove_phis: done:\n{}\n", func.display_verbose("| ", None));
}
