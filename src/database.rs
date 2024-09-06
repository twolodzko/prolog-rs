use crate::{errors::Error, types::Term};
use std::{cell::RefCell, collections::HashMap, ops::Deref, rc::Rc};

/// The name and arity of the predicate.
type Key = (String, usize);
type Predicates = Vec<Term>;

#[derive(PartialEq, Clone)]
pub struct Database(Rc<RefCell<HashMap<Key, Predicates>>>);

impl Database {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Database(Rc::new(RefCell::new(HashMap::new())))
    }

    /// Assert (record) the predicate in the database.
    pub fn assert(&mut self, term: &Term) -> Result<(), Error> {
        use Term::*;
        match term {
            Atom(id) => self.insert(id.to_string(), 0, term.clone()),
            Struct(id, args) => self.insert(id.to_string(), args.len(), term.clone()),
            Rule(head, _) => match head.deref() {
                Atom(id) => self.insert(id.to_string(), 0, term.clone()),
                Struct(id, args) => self.insert(id.to_string(), args.len(), term.clone()),
                other => return Err(Error::TypeError(other.clone())),
            },
            other => return Err(Error::TypeError(other.clone())),
        }
        Ok(())
    }

    /// Query the database for the predicate.
    pub(crate) fn query(&self, term: &Term) -> Option<Vec<Term>> {
        use Term::{Atom, Struct};
        match term {
            Atom(id) => self.get(id.to_string(), 0),
            Struct(id, args) => self.get(id.to_string(), args.len()),
            _ => unreachable!(),
        }
    }

    fn insert(&mut self, name: String, arity: usize, val: Term) {
        let key = (name, arity);
        if let Some(elems) = self.0.borrow_mut().get_mut(&key) {
            elems.push(val);
            return;
        }
        self.0.borrow_mut().insert(key, vec![val]);
    }

    fn get(&self, name: String, arity: usize) -> Option<Vec<Term>> {
        let key = (name, arity);
        self.0.borrow().get(&key).cloned()
    }
}

impl std::fmt::Debug for Database {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Database({:p})", self.0.as_ptr())
    }
}
