//! Lattice trait definition and some common implementations.

use crate::ir::*;
use regalloc2::indexset::IndexSet;
use std::fmt::Debug;

/// A lattice type used for an analysis.
///
/// The `meet` operator must compute the greatest lower bound for its
/// operands (that is, its result must be "less than or equal to" its
/// operands, according to the lattice's partial order, and must be
/// the greatest value that satisfies this condition). It must obey
/// the usual lattice laws:
///
/// * a `meet` a == a  (reflexivity)
/// * a `meet` b == b `meet` a (commutativity)
/// * a `meet` (b `meet` c) == (a `meet` b) `meet` c (associativity)
/// * a `meet` top == a
/// * a `meet` bottom == bottom
///
/// Note that while we require that the lattice is a consistent
/// partial order, we don't actually require the user to implement
/// `PartialOrd` on the type, because we never make direct ordering
/// comparisons when we perform a dataflow analysis. Instead the
/// ordering is only implicitly depended upon, in order to ensure that
/// the analysis terminates. For this to be true, we also require that
/// the lattice has only a finite chain length -- that is, there must
/// not be an infinite ordered sequence in the lattice (or, moving to
/// "lesser" values will always reach bottom in finite steps).
pub trait Lattice: Clone + Debug {
    /// Return the `top` lattice value.
    fn top() -> Self;
    /// Return the `bottom` lattice value.
    fn bottom() -> Self;
    /// Mutate self to `meet(self, other)`. Returns `true` if any
    /// changes occurred.
    fn meet_with(&mut self, other: &Self) -> bool;
}

/// An analysis-value lattice whose values are sets of `ValueId`
/// indices. `top` is empty and `bottom` is the universe set; the
/// `meet` function is a union. This is useful for may-analyses,
/// i.e. when an analysis computes whether a property *may* be true
/// about a value in some case.
#[derive(Clone, Debug)]
pub struct UnionBitSet {
    set: IndexSet,
    /// The set has degenerated to contain "the universe" (all
    /// possible values).
    universe: bool,
}

impl Lattice for UnionBitSet {
    fn top() -> Self {
        UnionBitSet {
            set: IndexSet::new(),
            universe: false,
        }
    }

    fn bottom() -> Self {
        UnionBitSet {
            set: IndexSet::new(),
            universe: true,
        }
    }

    fn meet_with(&mut self, other: &UnionBitSet) -> bool {
        if !self.universe && other.universe {
            self.universe = true;
            return true;
        }
        self.set.union_with(&other.set)
    }
}

impl UnionBitSet {
    pub fn contains(&self, index: usize) -> bool {
        self.universe || self.set.get(index)
    }

    pub fn add(&mut self, index: usize) {
        if !self.universe {
            self.set.set(index, true);
        }
    }

    pub fn remove(&mut self, index: usize) {
        if !self.universe {
            self.set.set(index, false);
        }
    }
}

/// An analysis-value lattice whose values are sets of `ValueId`
/// indices. `top` is the universe set and `bottom` is the empty set;
/// the `meet` function is an intersection. This is useful for
/// must-analyses, i.e. when an analysis computes whether a property
/// *must* be true about a value in all cases.
#[derive(Clone, Debug)]
pub struct IntersectionBitSet {
    /// We store the dual to the actual set, i.e., elements that are
    /// *not* included.
    not_set: UnionBitSet,
}

impl Lattice for IntersectionBitSet {
    fn top() -> Self {
        // `top` here is the universe-set; the dual of this set is the
        // empty-set, which is UnionBitSet's `top()`.
        Self {
            not_set: UnionBitSet::top(),
        }
    }

    fn bottom() -> Self {
        Self {
            not_set: UnionBitSet::bottom(),
        }
    }

    fn meet_with(&mut self, other: &IntersectionBitSet) -> bool {
        self.not_set.meet_with(&other.not_set)
    }
}

impl IntersectionBitSet {
    pub fn contains(&self, index: usize) -> bool {
        !self.not_set.contains(index)
    }

    pub fn add(&mut self, index: usize) {
        self.not_set.remove(index);
    }

    pub fn remove(&mut self, index: usize) {
        self.not_set.add(index);
    }
}
