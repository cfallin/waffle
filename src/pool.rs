//! Pooled list data structure.

use std::convert::TryFrom;
use std::default::Default;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

#[derive(Clone, Debug)]
pub struct ListPool<T: Clone + Debug> {
    storage: Vec<T>,
}

impl<T: Clone + Debug> Default for ListPool<T> {
    fn default() -> Self {
        ListPool { storage: vec![] }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct ListRef<T>(u32, u32, PhantomData<T>);

impl<T> Default for ListRef<T> {
    fn default() -> Self {
        ListRef(0, 0, PhantomData)
    }
}

impl<T: Clone + Debug> ListPool<T> {
    pub fn from_iter<I: Iterator<Item = T>>(&mut self, iter: I) -> ListRef<T> {
        let start = u32::try_from(self.storage.len()).unwrap();
        self.storage.extend(iter);
        let end = u32::try_from(self.storage.len()).unwrap();
        ListRef(start, end, PhantomData)
    }
    pub fn single(&mut self, value: T) -> ListRef<T> {
        self.from_iter(std::iter::once(value))
    }
    pub fn allocate(&mut self, size: usize, initial: T) -> ListRef<T> {
        self.from_iter(std::iter::repeat(initial).take(size))
    }
}

impl<T: Clone + Debug> Index<ListRef<T>> for ListPool<T> {
    type Output = [T];
    fn index(&self, index: ListRef<T>) -> &[T] {
        &self.storage[index.0 as usize..index.1 as usize]
    }
}

impl<T: Clone + Debug> IndexMut<ListRef<T>> for ListPool<T> {
    fn index_mut(&mut self, index: ListRef<T>) -> &mut [T] {
        &mut self.storage[index.0 as usize..index.1 as usize]
    }
}

impl<T> ListRef<T> {
    pub fn len(&self) -> usize {
        (self.1 - self.0) as usize
    }
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}
