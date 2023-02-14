//! Debug info (currently, source-location maps).

use crate::declare_entity;
use crate::entity::EntityVec;
use addr2line::gimli;
use std::collections::hash_map::Entry as HashEntry;
use std::collections::HashMap;
use std::convert::TryFrom;

declare_entity!(SourceFile, "file");
declare_entity!(SourceLoc, "loc");

#[derive(Clone, Debug, Default)]
pub struct Debug {
    pub source_files: EntityVec<SourceFile, String>,
    source_file_dedup: HashMap<String, SourceFile>,
    pub source_locs: EntityVec<SourceLoc, SourceLocData>,
    source_loc_dedup: HashMap<SourceLocData, SourceLoc>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceLocData {
    pub file: SourceFile,
    pub line: u32,
    pub col: u32,
}

impl Debug {
    pub fn intern_file(&mut self, path: &str) -> SourceFile {
        if let Some(id) = self.source_file_dedup.get(path) {
            return *id;
        }
        let id = self.source_files.push(path.to_owned());
        self.source_file_dedup.insert(path.to_owned(), id);
        id
    }

    pub fn intern_loc(&mut self, file: SourceFile, line: u32, col: u32) -> SourceLoc {
        let data = SourceLocData { file, line, col };
        match self.source_loc_dedup.entry(data) {
            HashEntry::Vacant(v) => {
                let id = self.source_locs.push(data);
                *v.insert(id)
            }
            HashEntry::Occupied(o) => *o.get(),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct DebugMap {
    tuples: Vec<(u32, u32, SourceLoc)>,
}

impl DebugMap {
    pub(crate) fn from_dwarf<R: gimli::Reader>(
        dwarf: gimli::Dwarf<R>,
        debug: &mut Debug,
    ) -> anyhow::Result<DebugMap> {
        let ctx = addr2line::Context::from_dwarf(dwarf)?;
        let mut tuples = vec![];

        let mut locs = ctx.find_location_range(0, u64::MAX).unwrap();
        while let Some((start, len, loc)) = locs.next() {
            let file = debug.intern_file(loc.file.unwrap_or(""));
            let loc = debug.intern_loc(file, loc.line.unwrap_or(0), loc.column.unwrap_or(0));
            tuples.push((start as u32, len as u32, loc));
        }
        tuples.sort();

        let mut last = 0;
        tuples.retain(|&(start, len, _)| {
            let retain = start >= last;
            if retain {
                last = start + len;
            }
            retain
        });

        Ok(DebugMap { tuples })
    }

    pub(crate) fn locs_from_offset<'a>(&'a self, offset: usize) -> &'a [(u32, u32, SourceLoc)] {
        let offset = u32::try_from(offset).unwrap();
        let start = match self.tuples.binary_search_by(|&(start, len, _)| {
            if offset < start {
                std::cmp::Ordering::Greater
            } else if offset >= (start + len) {
                std::cmp::Ordering::Less
            } else {
                std::cmp::Ordering::Equal
            }
        }) {
            Ok(idx) => idx,
            Err(first_after) => first_after,
        };
        &self.tuples[start..]
    }
}
