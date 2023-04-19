use super::{Block, Type, Value};
use crate::pool::{ListPool, ListRef};
use crate::Operator;

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub enum ValueDef {
    BlockParam(Block, u32, Type),
    Operator(Operator, ListRef<Value>, ListRef<Type>),
    PickOutput(Value, u32, Type),
    Alias(Value),
    Placeholder(Type),
    Trace(usize, ListRef<Value>),
    #[default]
    None,
}

impl ValueDef {
    pub fn ty(&self, types: &ListPool<Type>) -> Option<Type> {
        match self {
            &ValueDef::BlockParam(_, _, ty) => Some(ty),
            &ValueDef::Operator(_, _, tys) if tys.len() == 0 => None,
            &ValueDef::Operator(_, _, tys) if tys.len() == 1 => Some(types[tys][0]),
            &ValueDef::PickOutput(_, _, ty) => Some(ty),
            &ValueDef::Placeholder(ty) => Some(ty),
            &ValueDef::Trace(_, _) => None,
            _ => None,
        }
    }

    pub fn tys<'a>(&'a self, types: &'a ListPool<Type>) -> &'a [Type] {
        match self {
            &ValueDef::Operator(_, _, tys) => &types[tys],
            &ValueDef::BlockParam(_, _, ref ty)
            | &ValueDef::PickOutput(_, _, ref ty)
            | &ValueDef::Placeholder(ref ty) => std::slice::from_ref(ty),
            _ => &[],
        }
    }

    pub fn visit_uses<F: FnMut(Value)>(&self, arg_pool: &ListPool<Value>, mut f: F) {
        match self {
            &ValueDef::BlockParam { .. } => {}
            &ValueDef::Operator(_, args, _) => {
                for &arg in &arg_pool[args] {
                    f(arg);
                }
            }
            &ValueDef::PickOutput(from, ..) => f(from),
            &ValueDef::Alias(value) => f(value),
            &ValueDef::Placeholder(_) => {}
            &ValueDef::Trace(_, args) => {
                for &arg in &arg_pool[args] {
                    f(arg);
                }
            }
            &ValueDef::None => panic!(),
        }
    }

    pub fn update_uses<F: FnMut(&mut Value)>(&mut self, arg_pool: &mut ListPool<Value>, mut f: F) {
        match self {
            &mut ValueDef::BlockParam { .. } => {}
            &mut ValueDef::Operator(_, args, _) => {
                for arg in &mut arg_pool[args] {
                    f(arg);
                }
            }
            &mut ValueDef::PickOutput(ref mut from, ..) => f(from),
            &mut ValueDef::Alias(ref mut value) => f(value),
            &mut ValueDef::Placeholder(_) => {}
            &mut ValueDef::Trace(_, args) => {
                for arg in &mut arg_pool[args] {
                    f(arg);
                }
            }
            &mut ValueDef::None => panic!(),
        }
    }
}
