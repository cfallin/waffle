use super::{Block, Type, Value};
use crate::pool::{ListPool, ListRef};
use crate::Operator;

/// A definition of an SSA value.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub enum ValueDef {
    /// This value is a block parameter of the given block, with the
    /// given parameter position/index, and the given type.
    BlockParam(Block, u32, Type),
    /// This value is an operator, taking the given arguments, and
    /// producing the given result types.
    ///
    /// The result of an operator may be a single `Type` or a tuple of
    /// types; in the latter case, valid IR must use `PickOutput` to
    /// project out individual elements and use them.
    Operator(Operator, ListRef<Value>, ListRef<Type>),
    /// This value projects out one result of a multi-result
    /// instruction: given the value, the index in the result tuple,
    /// it produces a value of the given type.
    PickOutput(Value, u32, Type),
    /// This value is an alias of another value.
    Alias(Value),
    /// This value is a placeholder to be filled in later (e.g.,
    /// during SSA construction, may become a blockparam or an
    /// alias). Placeholders have fixed types that cannot change once
    /// they are filled in.
    Placeholder(Type),
    /// No value: must be filled in before processing.
    #[default]
    None,
}

impl ValueDef {
    /// Get the type of this value. Requires the type-pool. If this
    /// value is an operator with zero or multiple result types, this
    /// returns `None`.
    pub fn ty(&self, types: &ListPool<Type>) -> Option<Type> {
        match self {
            &ValueDef::BlockParam(_, _, ty) => Some(ty),
            &ValueDef::Operator(_, _, tys) if tys.len() == 0 => None,
            &ValueDef::Operator(_, _, tys) if tys.len() == 1 => Some(types[tys][0]),
            &ValueDef::PickOutput(_, _, ty) => Some(ty),
            &ValueDef::Placeholder(ty) => Some(ty),
            _ => None,
        }
    }

    /// Get the tuple of types of this value.
    pub fn tys<'a>(&'a self, types: &'a ListPool<Type>) -> &'a [Type] {
        match self {
            &ValueDef::Operator(_, _, tys) => &types[tys],
            &ValueDef::BlockParam(_, _, ref ty)
            | &ValueDef::PickOutput(_, _, ref ty)
            | &ValueDef::Placeholder(ref ty) => std::slice::from_ref(ty),
            _ => &[],
        }
    }

    /// Visit all other values used by this value with the given
    /// visitor function.
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
            &ValueDef::None => panic!(),
        }
    }

    /// Visit and update all other values used by this value with the
    /// given visitor function.
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
            &mut ValueDef::None => panic!(),
        }
    }
}
