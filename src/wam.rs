// FIXME
#![expect(dead_code)]

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
        let mut registers = Vec::new();
        flatten(term.clone(), &mut registers);
        println!("registers: {:?}", registers);
        self.compile_query(&registers)
    }

    fn compile_query(&mut self, registers: &[Term]) -> Result<(), Error> {
        use Instruction::*;
        use Term::*;
        for (i, reg) in registers.iter().enumerate() {
            match reg {
                Struct(id, args) => {
                    let arity = args.len();
                    self.code.push(PutStr(id.to_string(), arity, i));
                    for arg in args {
                        match arg {
                            Addr(addr) => self.code.push(SetVal(*addr)),
                            _ => unreachable!(),
                        }
                    }
                }
                Variable(_) => self.code.push(SetVar(i)),
                _ => self.code.push(SetVal(i)),
            }
        }
        Ok(())
    }

    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let code = Vec::new();
        let heap = Vec::new();
        Self { code, heap }
    }
}

/// Take the term and the register.
/// Store the term in the "flattened" form in the register.
fn flatten(term: Term, registers: &mut Vec<Term>) -> usize {
    use Term::{Addr, Struct, Variable};
    match term {
        Struct(id, args) => {
            let mut flat = Vec::new();
            for arg in args {
                let addr = if let Variable(_) = arg {
                    // get it's address or initialize
                    registers
                        .iter()
                        .position(|reg| *reg == arg)
                        .unwrap_or_else(|| flatten(arg, registers))
                } else {
                    flatten(arg, registers)
                };
                flat.push(Addr(addr));
            }
            registers.push(Struct(id, flat));
        }
        other => registers.push(other),
    }
    registers.len() - 1
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
    use crate::{atom, structure, types::Term::*, var};

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
