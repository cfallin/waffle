//! Scoped hashmap.

use fxhash::FxHashMap;
use std::hash::Hash;

pub struct ScopedMap<K: Hash + Eq, V> {
    map: FxHashMap<K, ScopedMapEntry<V>>,
    gen: u32,
    gen_by_level: Vec<u32>,
}

struct ScopedMapEntry<V> {
    gen: u32,
    level: u32,
    value: V,
}

impl<K: Hash + Eq, V> ScopedMap<K, V> {
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
                gen: self.gen,
                level: self.gen_by_level.len() as u32,
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
