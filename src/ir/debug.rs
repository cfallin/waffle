//! Debug info (currently, source-location maps).

use crate::declare_entity;
use crate::entity::EntityVec;
#[cfg(feature = "dwarf")]
use addr2line::gimli;
use fxhash::FxHashMap as HashMap;
use std::collections::hash_map::Entry as HashEntry;

declare_entity!(SourceFile, "file");
declare_entity!(SourceLoc, "loc");

#[derive(Clone, Debug, Default)]
pub struct Debug {
    /// Interned source-file names, indexed by a `SourceFile` entity
    /// index.
    pub source_files: EntityVec<SourceFile, String>,
    source_file_dedup: HashMap<String, SourceFile>,
    /// Interned source locations (file, line, and column),, indexed
    /// by a `SourceLoc` entity index.
    pub source_locs: EntityVec<SourceLoc, SourceLocData>,
    source_loc_dedup: HashMap<SourceLocData, SourceLoc>,
}

/// A "source location": a filename (interned to an ID), a line, and a
/// column.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceLocData {
    pub file: SourceFile,
    pub line: u32,
    pub col: u32,
}

impl Debug {
    /// Intern a filename to an ID.
    pub fn intern_file(&mut self, path: &str) -> SourceFile {
        if let Some(id) = self.source_file_dedup.get(path) {
            return *id;
        }
        let id = self.source_files.push(path.to_owned());
        self.source_file_dedup.insert(path.to_owned(), id);
        id
    }

    /// Intern a location (line and column) in an already-interned
    /// filename.
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

/// A map from ranges of offsets in the original Wasm file to source
/// locations.
#[derive(Clone, Debug, Default)]
pub struct DebugMap {
    /// Offset of code section relative to the Wasm file start.
    pub code_offset: u32,
    /// Each tuple is `(start, len, loc)`. The `start` offset is
    /// relative to the code section.
    pub tuples: Vec<(u32, u32, SourceLoc)>,
}

impl DebugMap {
    #[cfg(feature = "dwarf")]
    pub(crate) fn from_dwarf<R: gimli::Reader>(
        dwarf: gimli::Dwarf<R>,
        debug: &mut Debug,
        code_offset: u32,
    ) -> anyhow::Result<DebugMap> {
        let ctx = addr2line::Context::from_dwarf(dwarf)?;
        let mut tuples = vec![];

        let mut locs = ctx.find_location_range(0, u64::MAX).unwrap();
        while let Some((start, len, loc)) = locs.next() {
            let file = debug.intern_file(loc.file.unwrap_or(""));
            let loc = debug.intern_loc(file, loc.line.unwrap_or(0), loc.column.unwrap_or(0));
            log::trace!("tuple: loc {} start {:x} len {:x}", loc, start, len);
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

        Ok(DebugMap {
            code_offset,
            tuples,
        })
    }
}

/// Where each function landed in a compiled module's code section, for
/// producers that emit debug info (e.g. DWARF) for the output module.
///
/// Offsets are relative to the start of the code section's contents (the
/// function-count field), which is the address space DWARF uses for
/// WebAssembly.
#[derive(Clone, Debug, Default)]
pub struct CodeLayout {
    /// Each defined (non-imported) function, in code-section order.
    pub funcs: Vec<(crate::Func, FuncCodeLayout)>,
}

/// One function's placement in the code section (see [`CodeLayout`]).
#[derive(Clone, Debug, Default)]
pub struct FuncCodeLayout {
    /// Offset of the function's entry (its body-size field).
    pub start: u32,
    /// Offset of its body proper (the locals declarations).
    pub body_start: u32,
    /// Offset just past its last byte.
    pub end: u32,
    /// For a function compiled from IR: the offset of each emitted
    /// instruction whose value carries a valid source location, with that
    /// location, in ascending offset order. Instructions without a
    /// location (control flow, moves between locals) are not listed.
    pub locs: Vec<(u32, SourceLoc)>,
}
