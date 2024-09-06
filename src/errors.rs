use crate::{parser, types::Term};
use core::fmt;

#[derive(PartialEq, Debug, Clone)]
pub enum Error {
    TypeError(Term),
    NotCallable(Term),
    ArithError(Term),
    UnsetVar(String),
    Unknown(Term),
    ParsingError(parser::ParsingError),
    NoMatch,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Error::*;
        match self {
            TypeError(term) => write!(f, "{} has an invalid type", term),
            NotCallable(term) => write!(f, "{} is not callable", term),
            ArithError(term) => write!(f, "{} cannot be part of an arithmetic expression", term),
            UnsetVar(var) => write!(f, "variable {} was not instantiated", var),
            Unknown(term) => match term {
                Term::Atom(id) => write!(f, "unknown procedure: {}/0", id),
                Term::Struct(id, args) => write!(f, "unknown procedure: {}/{}", id, args.len()),
                _ => unreachable!(),
            },
            NoMatch => write!(f, "query returned no matches"),
            ParsingError(err) => err.fmt(f),
        }
    }
}

impl From<parser::ParsingError> for Error {
    fn from(value: parser::ParsingError) -> Self {
        Self::ParsingError(value)
    }
}
