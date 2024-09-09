// FIXME
#![allow(dead_code)]

use crate::types::Term;

#[derive(Debug, PartialEq)]
enum Flat {
    Value(Term),
    Struct(String, Vec<usize>),
}

#[derive(Debug, PartialEq)]
struct Flattened {
    map: Vec<Flat>,
}

impl Flattened {
    fn push(&mut self, value: Term) -> usize {
        use Term::*;
        match value {
            Variable(_) | Number(_) => {
                self.map.push(Flat::Value(value));
                self.map.len() - 1
            }
            _ => todo!(),
        }
    }
}

impl From<Term> for Flattened {
    fn from(value: Term) -> Self {
        let mut this = Flattened { map: Vec::new() };
        this.push(value);
        this
    }
}

#[cfg(test)]
mod tests {
    use super::{Flat, Flattened};
    use crate::{types::Term, var};

    #[test]
    fn variable() {
        use Flat::*;
        use Term::Variable;
        let result = Flattened::from(var!("X"));
        let expected = Flattened {
            map: vec![Value(var!("X"))],
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn number() {
        use Flat::*;
        use Term::Number;
        let result = Flattened::from(Number(42));
        let expected = Flattened {
            map: vec![Value(Number(42))],
        };
        assert_eq!(result, expected);
    }
}
