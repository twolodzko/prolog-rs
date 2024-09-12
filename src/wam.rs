// FIXME
#![expect(dead_code)]

use std::collections::VecDeque;

use crate::{errors::Error, types::Term};

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

#[derive(Debug, PartialEq)]
enum Instruction {
    SetVal(usize),
    SetVar(usize),
    PutStr(String, usize, usize),
}

/// Warren Abstract Machine
pub struct WAM {
    code: Vec<Instruction>,
    heap: Vec<Cell>,
}

impl WAM {
    pub fn compile(&mut self, term: &Term) -> Result<(), Error> {
        let registers = flatten(term.clone());
        println!("registers: {:?}", registers);
        self.compile_query(&registers)
    }

    fn compile_query(&mut self, registers: &[Term]) -> Result<(), Error> {
        Ok(())
    }

    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let code = Vec::new();
        let heap = Vec::new();
        Self { code, heap }
    }
}

fn flatten(term: Term) -> Vec<Term> {
    use Term::*;
    let mut flat = vec![term];
    let mut i = 0;
    while i < flat.len() {
        if let Struct(id, args) = flat[i].clone() {
            let mut repl = Vec::new();
            for arg in args {
                if let Some(addr) = flat.iter().position(|rec| *rec == arg) {
                    repl.push(Addr(addr));
                    continue;
                }
                repl.push(Addr(flat.len()));
                flat.push(arg.clone());
            }
            flat[i] = Struct(id, repl);
        }
        i += 1;
    }
    flat
}

fn deref(mut addr: usize, store: &[Cell]) -> usize {
    loop {
        match &store[addr] {
            Cell::Ref(value) if *value != addr => {
                addr = *value;
            }
            _ => return addr,
        }
    }
}

#[derive(Debug, Default)]
struct Vars(Vec<(String, usize)>);

impl Vars {
    fn get_val(&self, key: &str) -> Option<&usize> {
        self.0
            .iter()
            .find_map(|(k, v)| if key == k { Some(v) } else { None })
    }

    fn get_key(&self, val: &usize) -> Option<&String> {
        self.0
            .iter()
            .find_map(|(k, v)| if val == v { Some(k) } else { None })
    }

    fn insert(&mut self, key: String, val: usize) {
        debug_assert!(self.get_val(&key).is_none());
        self.0.push((key, val))
    }
}

#[cfg(test)]
mod tests {
    use super::WAM;
    use crate::{structure, types::Term::*, var};

    // #[test]
    // fn compile_atom() {
    //     use super::Cell::*;
    //     let mut wam = WAM::new();
    //     wam.compile_query(&atom!("a")).unwrap();

    //     let expected = vec![Fun("a".to_string(), 0)];
    //     assert_eq!(wam.heap, expected);
    // }

    // #[test]
    // fn compile_number() {
    //     use super::Cell::*;
    //     let mut wam = WAM::new();
    //     wam.compile_query(&Number(42)).unwrap();

    //     let expected = vec![Num(42)];
    //     assert_eq!(wam.heap, expected);
    // }

    #[test]
    fn compile_struct() {
        use super::Instruction::*;
        let mut wam = WAM::new();
        wam.compile(&structure!(
            "p",
            var!("Z"),
            structure!("h", var!("Z"), var!("W")),
            structure!("f", var!("W")),
        ))
        .unwrap();

        println!("code: {:?}", wam.code);

        let instructions = vec![
            PutStr("h".to_string(), 2, 0),
            SetVar(1),
            SetVar(5),
            PutStr("f".to_string(), 1, 3),
            SetVal(4),
            PutStr("p".to_string(), 3, 0),
            SetVal(1),
            SetVal(2),
            SetVal(3),
        ];

        assert_eq!(instructions, wam.code)

        // let expected = vec![
        //     Str(1),
        //     Fun("h".to_string(), 2),
        //     Ref(2),
        //     Ref(3),
        //     Str(5),
        //     Fun("f".to_string(), 1),
        //     Ref(3),
        //     Str(8),
        //     Fun("p".to_string(), 3),
        //     Ref(2),
        //     Str(1),
        //     Str(5),
        // ];
        // assert_eq!(wam.heap, expected);
    }
}
