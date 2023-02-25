//! Localification: a simple form of register allocation that picks
//! locations for SSA values in Wasm locals.

use crate::backend::treeify::Trees;
use crate::cfg::CFGInfo;
use crate::entity::{EntityVec, PerEntity};
use crate::ir::{Block, FunctionBody, Local, Type, Value, ValueDef};
use smallvec::{smallvec, SmallVec};
use std::collections::{hash_map::Entry, HashMap};

#[derive(Clone, Debug, Default)]
pub struct Localifier {
    pub values: PerEntity<Value, SmallVec<[Local; 2]>>,
    pub locals: EntityVec<Local, Type>,
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

    /// Liveranges for each Value, in an arbitrary index space
    /// (concretely, the span of first to last instruction visit step
    /// index in an RPO walk over the function body).
    ranges: HashMap<Value, std::ops::Range<usize>>,
    /// Number of points.
    points: usize,
}

impl<'a> Context<'a> {
    fn new(body: &'a FunctionBody, cfg: &'a CFGInfo, trees: &'a Trees) -> Self {
        let mut results = Localifier::default();

        // Create locals for function args.
        for &(ty, value) in &body.blocks[body.entry].params {
            let param_local = results.locals.push(ty);
            results.values[value] = smallvec![param_local];
        }

        Self {
            body,
            cfg,
            trees,
            results,
            ranges: HashMap::default(),
            points: 0,
        }
    }

    fn find_ranges(&mut self) {
        let mut point = 0;

        let mut live: HashMap<Value, usize> = HashMap::default();
        let mut block_starts: HashMap<Block, usize> = HashMap::default();
        for &block in self.cfg.rpo.values().rev() {
            block_starts.insert(block, point);

            self.body.blocks[block].terminator.visit_uses(|u| {
                self.handle_use(&mut live, &mut point, u);
            });
            point += 1;

            for &inst in self.body.blocks[block].insts.iter().rev() {
                self.handle_inst(&mut live, &mut point, inst, /* root = */ true);
                point += 1;
            }

            for &(_, param) in &self.body.blocks[block].params {
                self.handle_def(&mut live, &mut point, param);
            }
            point += 1;

            // If there were any in-edges from blocks numbered earlier
            // in postorder ("loop backedges"), extend the start of
            // the backward-range on all live values at this point to
            // the origin of the edge. (In forward program order,
            // extend the *end* of the liverange down to the end of
            // the loop.)
            //
            // Note that we do this *after* inserting our own start
            // above, so we handle self-loops properly.
            for &pred in &self.body.blocks[block].preds {
                if let Some(&start) = block_starts.get(&pred) {
                    for live_start in live.values_mut() {
                        *live_start = std::cmp::min(*live_start, start);
                    }
                }
            }
        }

        self.points = point;
    }

    fn handle_def(&mut self, live: &mut HashMap<Value, usize>, point: &mut usize, value: Value) {
        // If the value was not live, make it so just for this
        // point. Otherwise, end the liverange.
        log::trace!("localify: point {}: live {:?}: def {}", point, live, value);
        match live.entry(value) {
            Entry::Vacant(_) => {
                log::trace!(" -> was dead; use {}..{}", *point, *point + 1);
                self.ranges.insert(value, *point..(*point + 1));
            }
            Entry::Occupied(o) => {
                let start = o.remove();
                log::trace!(" -> was live; use {}..{}", start, *point + 1);
                self.ranges.insert(value, start..(*point + 1));
            }
        }
    }

    fn handle_use(&mut self, live: &mut HashMap<Value, usize>, point: &mut usize, value: Value) {
        let value = self.body.resolve_alias(value);
        log::trace!("localify: point {}: live {:?}: use {}", point, live, value);
        if self.trees.owner.contains_key(&value) {
            log::trace!(" -> treeified, going to inst");
            // If this is a treeified value, then don't process the use,
            // but process the instruction directly here.
            self.handle_inst(live, point, value, /* root = */ false);
        } else {
            // Otherwise, update liveranges: make value live at this
            // point if not live already.
            live.entry(value).or_insert(*point);
        }
    }

    fn handle_inst(
        &mut self,
        live: &mut HashMap<Value, usize>,
        point: &mut usize,
        value: Value,
        root: bool,
    ) {
        log::trace!(
            "localify: point {}: live {:?}: handling inst {} root {}",
            point,
            live,
            value,
            root
        );

        // If this is an instruction...
        if let ValueDef::Operator(_, ref args, _) = &self.body.values[value] {
            // If root, we need to process the def.
            if root {
                *point += 1;
                log::trace!(" -> def {}", value);
                self.handle_def(live, point, value);
            }
            *point += 1;
            // Handle uses.
            for &arg in args {
                log::trace!(" -> arg {}", arg);
                self.handle_use(live, point, arg);
            }
        }
        // Otherwise, it may be an alias (but resolved above) or
        // PickOutput, which we "see through" in handle_use of
        // consumers.
    }

    fn allocate(&mut self) {
        // Sort values by ranges' starting points, then value to break ties.
        let mut ranges: Vec<(Value, std::ops::Range<usize>)> =
            self.ranges.iter().map(|(k, v)| (*k, v.clone())).collect();
        ranges.sort_unstable_by_key(|(val, range)| (range.start, *val));

        // Keep a list of expiring Locals by expiry point.
        let mut expiring: HashMap<usize, SmallVec<[(Type, Local); 8]>> = HashMap::new();

        // Iterate over allocation space, processing range starts (at
        // which point we allocate) and ends (at which point we add to
        // the freelist).
        let mut range_idx = 0;
        let mut freelist: HashMap<Type, Vec<Local>> = HashMap::new();

        for i in 0..self.points {
            // Process ends. (Ends are exclusive, so we do them
            // first; another range can grab the local at the same
            // point index in this same iteration.)
            if let Some(expiring) = expiring.remove(&i) {
                for (ty, local) in expiring {
                    log::trace!(" -> expiring {} of type {} back to freelist", local, ty);
                    freelist.entry(ty).or_insert_with(|| vec![]).push(local);
                }
            }

            // Process starts.
            while range_idx < ranges.len() && ranges[range_idx].1.start == i {
                let (value, range) = ranges[range_idx].clone();
                range_idx += 1;
                log::trace!(
                    "localify: processing range for {}: {}..{}",
                    value,
                    range.start,
                    range.end
                );

                // If the value is an arg on block0, ignore; these
                // already have fixed locations.
                if let &ValueDef::BlockParam(b, _, _) = &self.body.values[value] {
                    if b == self.body.entry {
                        continue;
                    }
                }

                // Try getting a local from the freelist; if not,
                // allocate a new one.
                let mut allocs = smallvec![];
                let expiring = expiring.entry(range.end).or_insert_with(|| smallvec![]);
                for &ty in self.body.values[value].tys() {
                    let local = freelist
                        .get_mut(&ty)
                        .and_then(|v| v.pop())
                        .unwrap_or_else(|| {
                            log::trace!(" -> allocating new local of type {}", ty);
                            self.results.locals.push(ty)
                        });
                    log::trace!(" -> got local {} of type {}", local, ty);
                    allocs.push(local);
                    expiring.push((ty, local));
                }
                self.results.values[value] = allocs;
            }
        }
    }

    fn compute(mut self) -> Localifier {
        self.find_ranges();
        self.allocate();
        self.results
    }
}
