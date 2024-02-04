use std::collections::BTreeMap;

use crate::{
    entity::EntityRef, ExportKind, Func, FuncDecl, FunctionBody, ImportKind, Module, Operator,
    Terminator, ValueDef,
};

pub fn reorder_funcs_in_body(b: &mut FunctionBody, f: &BTreeMap<Func, Func>) {
    for v in b.values.values_mut() {
        if let ValueDef::Operator(a, _, _) = v {
            if let Operator::Call { function_index } = a {
                *function_index = *f.get(&*function_index).unwrap();
            }
        }
    }
    for k in b.blocks.values_mut() {
        if let Terminator::ReturnCall { func, args } = &mut k.terminator {
            *func = *f.get(&*func).unwrap();
        }
    }
}
pub fn reorder_funcs(m: &mut Module, fs: &BTreeMap<Func, Func>) {
    let mut n = m.funcs.clone();
    for (f, b) in m.funcs.entries() {
        let mut b = b.clone();
        if let Some(b) = b.body_mut() {
            reorder_funcs_in_body(b, fs);
        }
        n[*fs.get(&f).unwrap()] = b;
    }
    m.funcs = n;
    for t in m.tables.values_mut() {
        if let Some(e) = t.func_elements.as_mut() {
            for e in e.iter_mut() {
                *e = *fs.get(&*e).unwrap();
            }
        }
    }
    for i in m.imports.iter_mut() {
        if let ImportKind::Func(f) = &mut i.kind {
            *f = *fs.get(&*f).unwrap();
        }
    }
    for i in m.exports.iter_mut() {
        if let ExportKind::Func(f) = &mut i.kind {
            *f = *fs.get(&*f).unwrap();
        }
    }
}
pub fn fixup_orders(m: &mut Module) {
    let mut fs = BTreeMap::new();
    let mut a = vec![];
    let mut b = vec![];
    for (f, d) in m.funcs.entries() {
        if let FuncDecl::Import(_, _) = d {
            a.push(f)
        } else {
            b.push(f)
        }
    }
    let mut i = 0;
    for v in a {
        fs.insert(v, Func::new(i));
        i += 1;
    }
    for v in b {
        fs.insert(v, Func::new(i));
        i += 1;
    }
    assert_eq!(fs.len(),m.funcs.len());
    reorder_funcs(m, &fs);
    return;
}
