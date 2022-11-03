//! Scoped hashmap.

use fxhash::FxHashMap;
use std::fmt::Debug;
use std::hash::Hash;

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

#[derive(Clone, Debug)]
struct ScopedMapEntry<V: Clone + Debug> {
    gen: u32,
    level: u32,
    value: V,
}

impl<K: Hash + Eq + Clone + Debug, V: Clone + Debug> ScopedMap<K, V> {
    pub fn new() -> ScopedMap<K, V> {
        ScopedMap {
            map: FxHashMap::default(),
            gen: 0,
            gen_by_level: vec![0],
        }
    }

    pub fn push_level(&mut self) {
        self.gen += 1;
        self.gen_by_level.push(self.gen);
    }

    pub fn pop_level(&mut self) {
        self.gen_by_level.pop();
    }

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
