#![allow(dead_code)]

use std::fmt::Error;

use crate::types::Term;

#[derive(Debug, PartialEq)]
enum Cell {
    /// Variable reference
    Ref(usize),
    /// Structure representation
    Str(String, usize),
    /// Functor
    Fun(String, usize),
}

/// Warren Abstract Machine
pub struct WAM {
    heap: Vec<Cell>,
}

impl WAM {
    pub fn run(&mut self, term: Term) -> Result<(), Error> {
        todo!()
    }

    pub fn new() -> Self {
        let heap = Vec::new();
        Self { heap }
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
}
