//! Debug-info support (line-number mappings).

use std::collections::hash_map::Entry;
use std::collections::HashMap;

#[derive(Clone, Debug, Default)]
pub struct DebugMap {
    filenames: Vec<String>,
    lines: Vec<DebugLine>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct DebugLine {
    pc: u32,
    file: u32,
    line: u32,
    col: u32,
}

impl DebugMap {
    pub(crate) fn compute<'a>(
        debug_info: gimli::read::DebugInfo<gimli::read::EndianSlice<'a, gimli::LittleEndian>>,
        debug_abbrev: gimli::read::DebugAbbrev<gimli::read::EndianSlice<'a, gimli::LittleEndian>>,
        debug_line: gimli::read::DebugLine<gimli::read::EndianSlice<'a, gimli::LittleEndian>>,
        debug_str: gimli::read::DebugStr<gimli::read::EndianSlice<'a, gimli::LittleEndian>>,
    ) -> Self {
        log::trace!("Computing debug map");

        let mut filenames = vec![];
        let mut filenames_dedup = HashMap::new();
        let mut lines = vec![];

        // For each compilation unit...
        let mut units = debug_info.units();
        while let Some(unit) = units.next().unwrap() {
            let address_size = unit.address_size();
            let abbreviations = unit.abbreviations(&debug_abbrev).unwrap();
            let mut entries = unit.entries(&abbreviations);
            while let Some((_, entry)) = entries.next_dfs().unwrap() {
                let stmt_list = match entry.attr(gimli::DW_AT_stmt_list).unwrap() {
                    Some(stmt_list) => stmt_list,
                    None => continue,
                };
                let stmt_list = match stmt_list.value() {
                    gimli::read::AttributeValue::DebugLineRef(offset) => offset,
                    _ => continue,
                };
                let low_pc = match entry.attr(gimli::DW_AT_low_pc).unwrap() {
                    Some(low_pc) => low_pc,
                    None => continue,
                };
                let low_pc = match low_pc.value() {
                    gimli::read::AttributeValue::Addr(low_pc) => low_pc,
                    _ => continue,
                };

                println!(
                    "DebugInfoEntry has line-number program at {:?} for low_pc {:?}",
                    stmt_list, low_pc,
                );

                // Parse the line-number program.
                let prog = debug_line
                    .program(stmt_list, address_size, None, None)
                    .unwrap();

                // Parse the filename table.
                let unit_filenames = prog
                    .header()
                    .file_names()
                    .iter()
                    .map(|entry| match entry.path_name() {
                        gimli::read::AttributeValue::DebugStrRef(off) => debug_str
                            .get_str(off)
                            .unwrap()
                            .to_string()
                            .unwrap()
                            .to_owned(),
                        _ => "".to_owned(),
                    })
                    .collect::<Vec<_>>();

                println!("Filenames: {:?}", unit_filenames);

                // Intern the filenames and build a
                // map from index in this compilation
                // unit to global filename index.
                let filename_map = unit_filenames
                    .into_iter()
                    .map(|filename| match filenames_dedup.entry(filename.clone()) {
                        Entry::Vacant(v) => {
                            let index = filenames.len();
                            filenames.push(filename);
                            *v.insert(index)
                        }
                        Entry::Occupied(o) => *o.get(),
                    })
                    .collect::<Vec<_>>();

                // Parse the line-number program.

                let (prog, sequences) = prog.sequences().unwrap();
                for seq in sequences {
                    let mut rows = prog.resume_from(&seq);
                    while let Ok(Some((_, row))) = rows.next_row() {
                        let pc = row.address() + low_pc;
                        let pc = pc as u32;
                        let file = (row.file_index() - 1) as usize;
                        let file = filename_map[file] as u32;
                        let line = row.line().map(|v| v.get()).unwrap_or(0) as u32;
                        let col = match row.column() {
                            gimli::read::ColumnType::LeftEdge => 0,
                            gimli::read::ColumnType::Column(col) => col.get(),
                        } as u32;

                        lines.push(DebugLine {
                            pc,
                            file,
                            line,
                            col,
                        });
                    }
                }
            }
        }

        lines.sort();

        for line in &lines {
            println!("{:?}", line);
        }

        DebugMap { filenames, lines }
    }
}
