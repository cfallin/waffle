//! Location assignment (pseudo-regalloc) for SSA values onto
//! locals/operand-stack values.

use crate::{FunctionBody, LocalId, Value};
use fxhash::FxHashMap;

use super::{SerializedBody, SerializedOperator};

#[derive(Debug)]
pub struct Locations {
    pub locations: FxHashMap<(Value, usize), LocalId>,
    pub new_locals: Vec<wasmparser::Type>,
}

pub struct Allocator<'a> {
    locations: &'a mut Locations,
    f: &'a FunctionBody,
    spans: FxHashMap<(Value, usize), ValueSpan>,
    starts: Vec<ValueSpan>,
    ends: Vec<ValueSpan>,
    freelist: FxHashMap<wasmparser::Type, Vec<LocalId>>,
}

#[derive(Clone, Copy, Debug, Default)]
pub struct ValueSpan {
    value: Value,
    multi_value_index: usize,
    /// First index in serialized body at which value is live.
    start: usize,
    /// First index in serialized body at which value is no longer live.
    end: usize,
}

impl ValueSpan {
    fn len(&self) -> usize {
        self.end - self.start
    }
}

impl Locations {
    pub fn compute(f: &FunctionBody, body: &SerializedBody) -> Locations {
        let mut locations = Locations {
            locations: FxHashMap::default(),
            new_locals: vec![],
        };
        let mut allocator = Allocator {
            locations: &mut locations,
            f,
            freelist: FxHashMap::default(),
            spans: FxHashMap::default(),
            starts: vec![],
            ends: vec![],
        };

        allocator.compute_spans(&body.operators[..]);

        locations
    }
}

impl<'a> Allocator<'a> {
    fn handle_op(&mut self, location: usize, op: &SerializedOperator) {
        let mut reads = vec![];
        let mut writes = vec![];
        op.visit_value_locals(
            &mut |value, index| {
                reads.push((value, index));
            },
            &mut |value, index| {
                writes.push((value, index));
            },
        );

        log::trace!(
            "handle_op: at location {} op {:?} reads {:?} writes {:?}",
            location,
            op,
            reads,
            writes
        );

        for (value, index) in reads {
            let span = match self.spans.get_mut(&(value, index)) {
                Some(span) => span,
                None => {
                    panic!("Read before any write to local ({},{})", value, index);
                }
            };
            span.end = location + 1;
            log::trace!(" -> span for {}: {:?}", value, span);
        }

        for (value, index) in writes {
            let span = self.spans.entry((value, index)).or_insert(ValueSpan {
                value,
                multi_value_index: index,
                start: location,
                end: location + 1,
            });
            span.end = location + 1;
            log::trace!(" -> span for {}: {:?}", value, span);
        }
    }

    fn compute_spans(&mut self, operators: &[SerializedOperator]) {
        // For each operator, get the reads and writes and construct spans.
        for (index, operator) in operators.iter().enumerate() {
            self.handle_op(index, operator);
        }

        // Build lists of spans sorted by start and end.
        self.starts = self.spans.values().cloned().collect();
        self.ends = self.starts.clone();
        self.starts.sort_unstable_by_key(|span| span.start);
        self.ends.sort_unstable_by_key(|span| span.end);

        // Finally, assign locals to (value, index) pairs.
        let mut start_idx = 0;
        let mut end_idx = 0;
        while start_idx < self.starts.len() || end_idx < self.ends.len() {
            if start_idx < self.starts.len() && end_idx < self.ends.len() {
                if self.ends[end_idx].end <= self.starts[start_idx].start {
                    let span = self.ends[end_idx];
                    end_idx += 1;
                    self.handle_end(&span);
                } else {
                    let span = self.ends[start_idx];
                    start_idx += 1;
                    self.handle_start(&span);
                }
            } else if start_idx < self.starts.len() {
                let span = self.ends[start_idx];
                start_idx += 1;
                self.handle_start(&span);
            } else {
                let span = self.ends[end_idx];
                end_idx += 1;
                self.handle_end(&span);
            }
        }
    }

    fn handle_end(&mut self, span: &ValueSpan) {
        let local = self
            .locations
            .locations
            .get(&(span.value, span.multi_value_index))
            .cloned()
            .unwrap();
        let ty = self.f.types[span.value.index()][span.multi_value_index];
        self.freelist
            .entry(ty)
            .or_insert_with(|| vec![])
            .push(local);
    }

    fn handle_start(&mut self, span: &ValueSpan) {
        let ty = self.f.types[span.value.index()][span.multi_value_index];
        if let Some(list) = self.freelist.get_mut(&ty) {
            if let Some(local) = list.pop() {
                self.locations
                    .locations
                    .insert((span.value, span.multi_value_index), local);
                return;
            }
        }

        let new_local = self.f.locals.len() + self.locations.new_locals.len();
        self.locations.new_locals.push(ty);
        self.locations
            .locations
            .insert((span.value, span.multi_value_index), new_local as u32);
    }
}
