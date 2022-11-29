//! Localification: a simple form of register allocation that picks
//! locations for SSA values in Wasm locals.

use crate::backend::treeify::Trees;
use crate::cfg::CFGInfo;
use crate::entity::{EntityVec, PerEntity};
use crate::ir::{Block, FunctionBody, Local, Type, Value, ValueDef};
use smallvec::{smallvec, SmallVec};
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug, Default)]
pub struct Localifier {
    values: PerEntity<Value, SmallVec<[Local; 2]>>,
    locals: EntityVec<Local, Type>,
}

impl Localifier {
    pub fn compute(body: &FunctionBody, cfg: &CFGInfo, trees: &Trees) -> Self {
        Context::new(body, cfg, trees).compute()
    }
}

struct Context<'a> {
    body: &'a FunctionBody,
    cfg: &'a CFGInfo,
    trees: &'a Trees,
    results: Localifier,

    // Affinities (blockparam value to input arg value).
    affinities: HashMap<Value, SmallVec<[Value; 4]>>,

    // Livein to each block from dominators.
    livein_values: PerEntity<Block, HashSet<Value>>,
    livein_locals: PerEntity<Block, HashSet<Local>>,
}

impl<'a> Context<'a> {
    fn new(body: &'a FunctionBody, cfg: &'a CFGInfo, trees: &'a Trees) -> Self {
        let mut results = Localifier::default();
        // Create locals for function args.
        for &(ty, value) in &body.blocks[body.entry].params {
            let param_local = results.locals.push(ty);
            results.values[value] = smallvec![param_local];
        }
        // Compute affinities.
        let mut affinities = HashMap::default();
        for block in body.blocks.values() {
            block.terminator.visit_targets(|target| {
                for (&arg, &param) in target
                    .args
                    .iter()
                    .zip(body.blocks[target.block].params.iter().map(|(_, val)| val))
                {
                    affinities
                        .entry(param)
                        .or_insert_with(|| smallvec![])
                        .push(arg);
                }
            });
        }

        Self {
            body,
            cfg,
            trees,
            results,
            affinities,
            livein_values: PerEntity::default(),
            livein_locals: PerEntity::default(),
        }
    }

    fn compute(mut self) -> Localifier {
        // Create domtree preorder for traversal (we iterate in
        // reverse preorder below).
        let mut order = vec![];
        order.push(self.body.entry);
        let mut i = 0;
        while i < order.len() {
            for child in self.cfg.dom_children(order[i]) {
                order.push(child);
            }
            i += 1;
        }

        for &block in order.iter().rev() {
            self.process(block);
        }

        debug_assert!(self.livein_values[self.body.entry].is_empty());
        debug_assert!(self.livein_locals[self.body.entry].is_empty());

        self.results
    }

    fn process(&mut self, block: Block) {
        let mut live_values = HashSet::new();
        let mut live_locals = HashSet::new();

        // Collect liveins of all dominated blocks; this is our initial live-set.
        for child in self.cfg.dom_children(block) {
            for &livein_value in &self.livein_values[child] {
                log::trace!(
                    "localify: block {} gets livein value {} from block {}",
                    block,
                    livein_value,
                    child
                );
                live_values.insert(livein_value);
            }
            for &livein_local in &self.livein_locals[child] {
                live_locals.insert(livein_local);
            }
        }

        log::trace!(
            "localify: process block {}: liveout values {:?} locals {:?}",
            block,
            live_values,
            live_locals
        );

        // For each use/def in reverse order, update live-set; on last
        // use (first observed), allocate a local.
        fn handle_use(
            body: &FunctionBody,
            u: Value,
            live_values: &mut HashSet<Value>,
            live_locals: &mut HashSet<Local>,
            results: &mut Localifier,
            affinities: &HashMap<Value, SmallVec<[Value; 4]>>,
        ) {
            let u = body.resolve_alias(u);
            if live_values.insert(u) {
                // If there is already an allocation (e.g. for a
                // function parameter), return.
                if !results.values[u].is_empty() {
                    return;
                }

                // Need to create an allocation.
                let def = &body.values[u];
                match def {
                    &ValueDef::Alias(_value) => {
                        unreachable!();
                    }
                    &ValueDef::PickOutput(value, idx, _) => {
                        handle_use(body, value, live_values, live_locals, results, affinities);
                        results.values[u] = smallvec![results.values[value][idx]];
                    }
                    &ValueDef::BlockParam(..) | &ValueDef::Operator(..) => {
                        let locals = def
                            .tys()
                            .iter()
                            .map(|&ty| {
                                // Try to find a local of the right type that is not live.
                                let affinities = affinities.get(&u).map(|v| &v[..]).unwrap_or(&[]);
                                let local = affinities
                                    .iter()
                                    .filter_map(|&aff_val| {
                                        let local = *results.values[aff_val].get(0)?;
                                        Some((local, results.locals[local]))
                                    })
                                    .chain(results.locals.entries().map(|(local, &ty)| (local, ty)))
                                    .filter_map(|(local, local_ty)| {
                                        if local_ty == ty && live_locals.insert(local) {
                                            Some(local)
                                        } else {
                                            None
                                        }
                                    })
                                    .next();
                                local.unwrap_or_else(|| {
                                    let local = results.locals.push(ty);
                                    live_locals.insert(local);
                                    local
                                })
                            })
                            .collect::<SmallVec<_>>();
                        results.values[u] = locals;
                    }
                    &ValueDef::Placeholder(_) | &ValueDef::None => unreachable!(),
                }
            }
        }

        fn handle_def(
            body: &FunctionBody,
            d: Value,
            live_values: &mut HashSet<Value>,
            live_locals: &mut HashSet<Local>,
            results: &Localifier,
        ) {
            if let ValueDef::Alias(..) = &body.values[d] {
                return;
            }

            if live_values.remove(&d) {
                for &local in &results.values[d] {
                    live_locals.remove(&local);
                }
            }
        }

        self.body.blocks[block].terminator.visit_uses(|u| {
            handle_use(
                self.body,
                u,
                &mut live_values,
                &mut live_locals,
                &mut self.results,
                &self.affinities,
            )
        });

        for &inst in self.body.blocks[block].insts.iter().rev() {
            handle_def(
                self.body,
                inst,
                &mut live_values,
                &mut live_locals,
                &self.results,
            );
            self.body.values[inst].visit_uses(|u| {
                // If treeified, then don't process use.
                if !self.trees.owner.contains_key(&u) {
                    handle_use(
                        self.body,
                        u,
                        &mut live_values,
                        &mut live_locals,
                        &mut self.results,
                        &self.affinities,
                    )
                }
            });
        }

        for &(_, param) in &self.body.blocks[block].params {
            handle_def(
                self.body,
                param,
                &mut live_values,
                &mut live_locals,
                &self.results,
            );
        }

        log::trace!(
            "localify: process block {}: livein values {:?} locals {:?}",
            block,
            live_values,
            live_locals
        );
        self.livein_locals[block] = live_locals;
        self.livein_values[block] = live_values;
    }
}
