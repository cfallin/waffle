//! Conversion pass that creates "maximal SSA": only local uses (no
//! uses of defs in other blocks), with all values explicitly passed
//! through blockparams. This makes some other transforms easier
//! because it removes the need to worry about adding blockparams when
//! mutating the CFG (all possible blockparams are already there!).

use crate::cfg::CFGInfo;
use crate::entity::PerEntity;
use crate::ir::{Block, FunctionBody, Value, ValueDef};
use std::collections::{BTreeSet, HashMap};

pub fn run(body: &mut FunctionBody, cfg: &CFGInfo) {
    MaxSSAPass::new().run(body, cfg);
}

struct MaxSSAPass {
    /// Additional block args that must be passed to each block, in
    /// order. Value numbers are *original* values.
    new_args: PerEntity<Block, Vec<Value>>,
    /// For each block, a value map: from original value to local copy
    /// of value.
    value_map: HashMap<(Block, Value), Value>,
}

impl MaxSSAPass {
    fn new() -> Self {
        Self {
            new_args: PerEntity::default(),
            value_map: HashMap::new(),
        }
    }

    fn run(mut self, body: &mut FunctionBody, cfg: &CFGInfo) {
        for block in body.blocks.iter() {
            self.visit(body, cfg, block);
        }
        self.update(body);
    }

    fn visit(&mut self, body: &mut FunctionBody, cfg: &CFGInfo, block: Block) {
        // For each use in the block, process the use. Collect all
        // uses first to deduplicate and allow more efficient
        // processing (and to appease the borrow checker).
        let mut uses = BTreeSet::default();
        for &inst in &body.blocks[block].insts {
            match &body.values[inst] {
                &ValueDef::Operator(_, ref args, _) => {
                    for &arg in args {
                        let arg = body.resolve_alias(arg);
                        uses.insert(arg);
                    }
                }
                &ValueDef::PickOutput(value, ..) => {
                    let value = body.resolve_alias(value);
                    uses.insert(value);
                }
                _ => {}
            }
        }
        body.blocks[block].terminator.visit_uses(|u| {
            let u = body.resolve_alias(u);
            uses.insert(u);
        });

        for u in uses {
            self.visit_use(body, cfg, block, u);
        }
    }

    fn visit_use(&mut self, body: &mut FunctionBody, cfg: &CFGInfo, block: Block, value: Value) {
        if self.value_map.contains_key(&(block, value)) {
            return;
        }
        if cfg.def_block[value] == block {
            return;
        }
        self.new_args[block].push(value);

        // Create a blockparam.
        let ty = body.values[value].ty().unwrap();
        let blockparam = body.add_blockparam(block, ty);
        self.value_map.insert((block, value), blockparam);

        // Recursively visit preds and use the value there, to ensure
        // they have the value available as well.
        for i in 0..body.blocks[block].preds.len() {
            // Don't borrow for whole loop while iterating (`body` is
            // taken as mut by recursion, but we don't add preds).
            let pred = body.blocks[block].preds[i];
            self.visit_use(body, cfg, pred, value);
        }
    }

    fn update_preds(&mut self, body: &mut FunctionBody, block: Block) {
        for i in 0..body.blocks[block].preds.len() {
            let pred = body.blocks[block].preds[i];
            let pred_succ_idx = body.blocks[block].pos_in_pred_succ[i];
            body.blocks[pred]
                .terminator
                .update_target(pred_succ_idx, |target| {
                    for &new_arg in &self.new_args[block] {
                        let actual_value = self
                            .value_map
                            .get(&(pred, new_arg))
                            .copied()
                            .unwrap_or(new_arg);
                        target.args.push(actual_value);
                    }
                });
        }
    }

    fn update_uses(&mut self, body: &mut FunctionBody, block: Block) {
        let resolve = |body: &FunctionBody, value: Value| {
            let value = body.resolve_alias(value);
            self.value_map
                .get(&(block, value))
                .copied()
                .unwrap_or(value)
        };

        for i in 0..body.blocks[block].insts.len() {
            let inst = body.blocks[block].insts[i];
            let mut def = std::mem::take(&mut body.values[inst]);
            match &mut def {
                ValueDef::Operator(_, args, _) => {
                    for arg in args {
                        *arg = resolve(body, *arg);
                    }
                }
                ValueDef::PickOutput(value, ..) => {
                    *value = resolve(body, *value);
                }
                ValueDef::Alias(_) => {
                    // Nullify the alias: should no longer be needed.
                    def = ValueDef::None;
                }
                _ => {}
            }
            body.values[inst] = def;
        }
        let mut term = std::mem::take(&mut body.blocks[block].terminator);
        term.update_uses(|u| {
            *u = resolve(body, *u);
        });
        body.blocks[block].terminator = term;
    }

    fn update(&mut self, body: &mut FunctionBody) {
        for block in body.blocks.iter() {
            if self.new_args[block].len() > 0 {
                self.update_preds(body, block);
            }
            self.update_uses(body, block);
        }
    }
}
