use crate::parser::is_operator;
use std::fmt::{self};

#[derive(Clone, PartialEq)]
pub enum Term {
    Number(i32),
    Functor(String, Vec<Term>), // id(args...), it also is used for lists [1,2|[]] is .(1, .(2, []))
    Nil,                        // empty list []
    // special forms
    Variable(String, usize),    // Id
    Any,                        // wildcard _
    Rule(Box<Term>, Vec<Term>), // head :- body
    Question(Vec<Term>),        // ?- body.
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Term::*;
        match self {
            Any => write!(f, "_"),
            term @ Functor(name, _) if name == "." => {
                let (list, last) = list_to_vec(term);
                let s = join(&list);
                match last {
                    Some(last) => write!(f, "[{}|{}]", s, last),
                    None => write!(f, "[{}]", s),
                }
            }
            Functor(op, args) if args.len() == 2 && is_operator(op) => {
                if op.chars().any(|c| c.is_alphabetic()) {
                    write!(f, "{} {} {}", args[0], op, args[1])
                } else {
                    write!(f, "{}{}{}", args[0], op, args[1])
                }
            }
            Functor(name, args) => {
                if args.is_empty() {
                    write!(f, "{}", name)
                } else {
                    write!(f, "{}({})", name, join(args))
                }
            }
            Variable(id, _) => write!(f, "{}", id),
            Number(val) => write!(f, "{}", val),
            Nil => write!(f, "[]"),
            Rule(head, body) => write!(f, "{} :- {}.", head, join(body)),
            Question(body) => write!(f, "?- {}.", join(body)),
        }
    }
}

impl fmt::Debug for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Term::{Functor, Question};
        match self {
            Functor(name, args) => {
                if args.is_empty() {
                    write!(f, "{}/0", name)
                } else {
                    write!(f, "{}/{}({})", name, args.len(), debug_join(args))
                }
            }
            Question(body) => write!(f, "?- {}.", debug_join(body)),
            _ => write!(f, "{}", self),
        }
    }
}

fn list_to_vec(mut term: &Term) -> (Vec<Term>, Option<Term>) {
    use Term::{Functor, Nil};

    let mut res = Vec::new();
    let mut last = None;
    loop {
        match term {
            Functor(id, args) => {
                if id == "." && args.len() == 2 {
                    res.push(args[0].clone());
                    term = &args[1];
                }
            }
            Nil => break,
            other => {
                last = Some(other.clone());
                break;
            }
        }
    }
    (res, last)
}

fn join<T>(seq: &[T]) -> String
where
    T: fmt::Display,
{
    seq.iter()
        .map(|t| t.to_string())
        .reduce(|acc, x| format!("{}, {}", acc, x))
        .unwrap_or_default()
}

fn debug_join<T>(seq: &[T]) -> String
where
    T: fmt::Debug,
{
    seq.iter()
        .map(|x| format!("{:?}", x))
        .reduce(|acc, x| format!("{}, {}", acc, x))
        .unwrap_or_default()
}

/// Iterate over dotted pairs (lists).
pub struct ConsIter {
    term: Option<Term>,
}

impl From<Term> for ConsIter {
    fn from(value: Term) -> Self {
        Self { term: Some(value) }
    }
}

impl Iterator for ConsIter {
    type Item = Term;

    fn next(&mut self) -> Option<Self::Item> {
        use Term::{Functor, Nil};
        match self.term.clone()? {
            Functor(id, args) if args.len() == 2 && id == "." => {
                self.term = match &args[1] {
                    Nil => None,
                    other => Some(other.clone()),
                };
                Some(args[0].clone())
            }
            other => {
                self.term = None;
                Some(other)
            }
        }
    }
}

#[macro_export]
macro_rules! var {
    ( $id:expr ) => {
        Variable($id.to_string(), 0)
    };
}

#[macro_export]
macro_rules! atom {
    ( $id:expr ) => {
        Functor($id.to_string(), Vec::new())
    };
}

#[macro_export]
macro_rules! structure {
    ( $id:expr , $($x:expr),+ $(,)? ) => {
        Functor($id.to_string(), vec![$($x),+])
    };
}

#[cfg(test)]
mod tests {
    use super::Term::{self, *};
    use test_case::test_case;

    #[test_case(
        Number(42),
        "42";
        "number"
    )]
    #[test_case(
        atom!("true"),
        "true";
        "atom"
    )]
    #[test_case(
        Nil,
        "[]";
        "empty list"
    )]
    #[test_case(
        Any,
        "_";
        "wildcard"
    )]
    #[test_case(
        var!("Foo"),
        "Foo";
        "variable"
    )]
    #[test_case(
        structure!("foo", atom!("a"), Number(-5)),
        "foo(a,-5)";
        "simple struct"
    )]
    #[test_case(
        structure!("+", Number(1), structure!("*", Number(2), Number(-5))),
        "1+2*-5";
        "operators"
    )]
    #[test_case(
        structure!("is", var!("X"), structure!("+", Number(2), Number(1))),
        "X is 2+1";
        "operators with spaces"
    )]
    fn fmt(input: Term, expected: &str) {
        assert_eq!(input.to_string(), expected);
    }

    #[test]
    fn macros_expansion() {
        let tt = [
            (atom!("foo"), Functor("foo".to_string(), Vec::new())),
            (var!("Bar"), Variable("Bar".to_string(), 0)),
            (
                structure!("foo", Number(1)),
                Functor("foo".to_string(), vec![Number(1)]),
            ),
            (
                structure!("bar", Number(1), structure!("foo", Number(2), var!("X"))),
                Functor(
                    "bar".to_string(),
                    vec![
                        Number(1),
                        Functor(
                            "foo".to_string(),
                            vec![Number(2), Variable("X".to_string(), 0)],
                        ),
                    ],
                ),
            ),
            (
                structure!(".", Number(1), Nil),
                Functor(".".to_string(), vec![Number(1), Nil]),
            ),
        ];
        for (input, expected) in tt {
            assert_eq!(input, expected)
        }
    }
}
