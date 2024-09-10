// FIXME
#![allow(dead_code)]

use crate::types::Term;
use std::fmt::Error;

#[derive(Debug, PartialEq)]
enum Cell {
    /// Variable reference
    Ref(usize),
    /// Structure reference
    Str(usize),
    /// Functor head
    Fun(String, usize),
    /// Number
    Num(i32),
}

/// Warren Abstract Machine
pub struct WAM {
    heap: Vec<Cell>,
}

impl WAM {
    pub fn run(&mut self, term: Term) -> Result<(), Error> {
        use Term::*;
        match term {
            Atom(_) | Number(_) => self.set_value(term),
            _ => todo!(),
        }
        Ok(())
    }

    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let heap = Vec::new();
        Self { heap }
    }

    fn set_value(&mut self, term: Term) {
        use Cell::*;
        use Term::*;
        match term {
            Atom(id) => self.heap.push(Fun(id, 0)),
            Number(val) => self.heap.push(Num(val)),
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::WAM;
    use crate::{atom, types::Term::*};

    #[test]
    fn run_atom() {
        use super::Cell::*;
        let mut wam = WAM::new();
        wam.run(atom!("a")).unwrap();

        let expected = vec![Fun("a".to_string(), 0)];
        assert_eq!(wam.heap, expected);
    }

    #[test]
    fn run_number() {
        use super::Cell::*;
        let mut wam = WAM::new();
        wam.run(Number(42)).unwrap();

        let expected = vec![Num(42)];
        assert_eq!(wam.heap, expected);
    }
}
