// FIXME
#![expect(dead_code)]
#![expect(unused_variables)]

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

enum Instruction {
    SetValue(usize),
    SetVariable(usize),
    PutStruct(String, usize, usize),
}

/// Warren Abstract Machine
pub struct WAM {
    heap: Vec<Cell>,
    vars: Vars,
}

impl WAM {
    pub fn run(&mut self, term: &Term) -> Result<(), Error> {
        self.compile_query(term)?;
        Ok(())
    }

    fn compile_query(&mut self, term: &Term) -> Result<usize, Error> {
        use Term::*;
        match term {
            Atom(_) | Number(_) => self.set_value(term),
            Variable(id) => {
                if let Some(pos) = self.vars.get_val(id.as_str()) {
                    return Ok(*pos);
                } else {
                    self.set_variable();
                }
            }
            Struct(id, args) => {
                let mut flat = self.flatten(args)?;
                self.put_structure(id, args.len());
                let head = self.heap.len() - 1;
                self.heap.append(&mut flat);
                return Ok(head);
            }
            _ => unreachable!(),
        }
        Ok(self.heap.len() - 1)
    }

    /// Push the arguments of a struct to the stack.
    /// Return the list of references to the stack cells that were created.
    fn flatten(&mut self, args: &[Term]) -> Result<Vec<Cell>, Error> {
        use Cell::*;
        use Term::*;

        let mut flat = Vec::new();
        let mut h = 1;
        for arg in args {
            h += 1;
            match arg {
                Struct(_, _) => {
                    h = self.compile_query(arg)?;
                    flat.push(Str(h));
                }
                Variable(id) => {
                    if let Some(pos) = self.vars.get_val(id.as_str()) {
                        flat.push(Ref(*pos));
                    } else {
                        flat.push(Ref(h));
                        self.vars.insert(id.to_string(), h);
                    }
                }
                _ => flat.push(Ref(h)),
            };
        }
        Ok(flat)
    }

    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let heap = Vec::new();
        let vars = Vars::default();
        Self { heap, vars }
    }

    fn set_value(&mut self, term: &Term) {
        use Cell::*;
        use Term::*;
        match term {
            Atom(id) => self.heap.push(Fun(id.to_string(), 0)),
            Number(val) => self.heap.push(Num(*val)),
            _ => todo!(),
        }
    }

    fn put_structure(&mut self, id: &str, arity: usize) {
        let h = self.heap.len() + 1;
        self.heap.push(Cell::Str(h));
        self.heap.push(Cell::Fun(id.to_string(), arity))
    }

    fn set_variable(&mut self) {
        let h = self.heap.len();
        self.heap.push(Cell::Ref(h));
    }

    fn compile_program(&mut self, term: &Term) -> Result<usize, Error> {
        use Term::*;
        match term {
            Atom(_) | Number(_) => self.unify_value(term),
            Variable(id) => self.unify_variable(term),
            Struct(id, args) => todo!(),
            _ => unreachable!(),
        }
        Ok(self.heap.len() - 1)
    }

    fn unify_value(&mut self, term: &Term) {
        todo!()
    }

    fn unify_variable(&mut self, term: &Term) {
        todo!()
    }

    fn get_structure(&self, id: &str, arity: usize) {
        todo!()
    }
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

    #[test]
    fn compile_atom() {
        use super::Cell::*;
        let mut wam = WAM::new();
        wam.compile_query(&atom!("a")).unwrap();

        let expected = vec![Fun("a".to_string(), 0)];
        assert_eq!(wam.heap, expected);
    }

    #[test]
    fn compile_number() {
        use super::Cell::*;
        let mut wam = WAM::new();
        wam.compile_query(&Number(42)).unwrap();

        let expected = vec![Num(42)];
        assert_eq!(wam.heap, expected);
    }

    #[test]
    fn compile_struct() {
        use super::Cell::*;
        let mut wam = WAM::new();
        wam.compile_query(&structure!(
            "p",
            var!("Z"),
            structure!("h", var!("Z"), var!("W")),
            structure!("f", var!("W")),
        ))
        .unwrap();

        let expected = vec![
            Str(1),
            Fun("h".to_string(), 2),
            Ref(2),
            Ref(3),
            Str(5),
            Fun("f".to_string(), 1),
            Ref(3),
            Str(8),
            Fun("p".to_string(), 3),
            Ref(2),
            Str(1),
            Str(5),
        ];
        assert_eq!(wam.heap, expected);
    }
}
