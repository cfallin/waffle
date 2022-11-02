use super::{Block, Value, Type};
use crate::Operator;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ValueDef {
    BlockParam(Block, usize, Type),
    Operator(Operator, Vec<Value>, Vec<Type>),
    PickOutput(Value, usize, Type),
    Alias(Value),
    Placeholder(Type),
}

impl ValueDef {
    pub fn visit_uses<F: FnMut(Value)>(&self, mut f: F) {
        match self {
            &ValueDef::BlockParam { .. } => {}
            &ValueDef::Operator(_, ref args, _) => {
                for &arg in args {
                    f(arg);
                }
            }
            &ValueDef::PickOutput(from, ..) => f(from),
            &ValueDef::Alias(value) => f(value),
            &ValueDef::Placeholder(_) => {}
        }
    }

    pub fn update_uses<F: FnMut(&mut Value)>(&mut self, mut f: F) {
        match self {
            &mut ValueDef::BlockParam { .. } => {}
            &mut ValueDef::Operator(_, ref mut args, _) => {
                for arg in args {
                    f(arg);
                }
            }
            &mut ValueDef::PickOutput(ref mut from, ..) => f(from),
            &mut ValueDef::Alias(ref mut value) => f(value),
            &mut ValueDef::Placeholder(_) => {}
        }
    }
}
