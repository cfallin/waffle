//! Scoped hashmap.
//!
//! This container keeps a map from a type `K` to `V`, but with a
//! twist: it supports "push" and "pop" operations, which alter a
//! stack-level, and on "pop", any mappings created at the current
//! stack level or greater are removed. This is useful in conjunction
//! with tree-structured walks over code: for example, a GVN pass that
//! traverses the domtree, keeping "availability" of already-computed
//! expressions in the map only with the scope of a subtree.
//!
//! For efficiency, the scoped hashmap does not actually perform a
//! "remove" operation in the underlying hashmap for every operation
//! removed by a pop. This would result in O(n) pop cost,
//! worst-case. Instead, we fast-invalidate all mappings by
//! incrementing a generation number, and checking that generation on
//! access, ignoring stale entries. We need a separate generation
//! number for each level of the scoped hashmap; "push" increments the
//! generation on the new level ("this is the 64th time we have been
//! at level 3") and when a value is inserted, it records the level it
//! is at and the current generation of that level.
//!
//! Note that this scheme does *not* support shadowing: if we set a
//! particular key `k1` to value `v1` at level 3, then set it again at
//! level 4, we will remove it when we pop level 4; the old value set
//! at level 3 does not re-appear. Doing so would require a more
//! complex data structure, and is unnecessary for our use-cases. In
//! particular, we use this for GVN, where if a key already exists, we
//! use it rather than setting it again in a more nested scope.

use fxhash::FxHashMap;
use std::fmt::Debug;
use std::hash::Hash;

/// A scoped hashmap: a key-value map with "push" and "pop" operations
/// and the ability to quickly remove mappings created at a given
/// level when popping.
#[derive(Clone, Debug)]
pub struct ScopedMap<K: Hash + Eq + Clone + Debug, V: Clone + Debug> {
    map: FxHashMap<K, ScopedMapEntry<V>>,
    gen: u32,
    gen_by_level: Vec<u32>,
}

impl<K: Hash + Eq + Clone + Debug, V: Clone + Debug> std::default::Default for ScopedMap<K, V> {
    fn default() -> Self {
        ScopedMap::new()
    }
}

/// An entry in the scoped hashmap.
#[derive(Clone, Debug)]
struct ScopedMapEntry<V: Clone + Debug> {
    /// The generation of the level at which this entry was created,
    /// when it was created.
    gen: u32,
    /// The level at which this entry was created.
    level: u32,
    /// The value associated with this key.
    value: V,
}

impl<K: Hash + Eq + Clone + Debug, V: Clone + Debug> ScopedMap<K, V> {
    /// Create an empty scoped hashmap.
    pub fn new() -> ScopedMap<K, V> {
        ScopedMap {
            map: FxHashMap::default(),
            gen: 0,
            gen_by_level: vec![0],
        }
    }

    /// Create a new sub-level.
    pub fn push_level(&mut self) {
        self.gen += 1;
        self.gen_by_level.push(self.gen);
    }

    /// Pop the current level, removing all mappings created at this
    /// level.
    pub fn pop_level(&mut self) {
        self.gen_by_level.pop();
    }

    /// Insert a mapping, associating it with the current level, and
    /// overwriting if one already exists.
    pub fn insert(&mut self, k: K, v: V) {
        self.map.insert(
            k,
            ScopedMapEntry {
                gen: *self.gen_by_level.last().unwrap(),
                level: (self.gen_by_level.len() - 1) as u32,
                value: v,
            },
        );
    }

    /// Get the mapping for the given key, if any.
    pub fn get(&self, k: &K) -> Option<&V> {
        self.map.get(k).and_then(|entry| {
            let level = entry.level as usize;
            if level < self.gen_by_level.len() && entry.gen == self.gen_by_level[level] {
                Some(&entry.value)
            } else {
                None
            }
        })
    }
}
