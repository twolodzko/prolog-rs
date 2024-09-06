use crate::types::Term::{self, *};

pub(super) const UNINIT: usize = 0;
pub(super) const BASE: usize = 1;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Vars {
    map: Vec<(Term, Term)>,
    branch: usize,
}

impl Vars {
    pub(super) fn get(&self, key: &Term) -> Option<&Term> {
        self.map
            .iter()
            .rev()
            .find(|(term, _)| term == key)
            .map(|(_, val)| val)
    }

    pub(super) fn insert(&mut self, key: Term, val: Term) {
        debug_assert!(matches!(key, Variable(_, scope) if scope != UNINIT));
        self.map.push((key, val));
    }

    /// Retain the variables from the initial goal mapped to their values.
    pub(super) fn prune(&mut self) {
        self.flatten();
        self.map.retain(|(key, val)| match (key, val) {
            (Variable(_, x), Variable(_, y)) => *x == BASE && *y == BASE,
            (_, Variable(_, BASE)) | (Variable(_, BASE), _) => true,
            _ => false,
        });
        self.branch = UNINIT;
    }

    /// Replace the references to the values with the values.
    pub(super) fn flatten(&mut self) {
        for i in (0..self.len()).rev() {
            let old = &self.map[i].1;
            if let Some(new) = self.replacement(old) {
                self.map[i].1 = self.subst(&new);
            }
        }
    }

    /// Find a replacement value for `val` when flattening `Vars`.
    pub(super) fn replacement(&self, val: &Term) -> Option<Term> {
        match val {
            Variable(_, BASE) => None,
            Variable(_, _) => self.get(val).cloned(),
            Struct(id, args) => {
                let new_args = args
                    .iter()
                    .map(|arg| self.replacement(arg).unwrap_or(arg.clone()))
                    .collect();
                Some(Struct(id.to_string(), new_args))
            }
            _ => None,
        }
    }

    pub fn iter(&self) -> std::slice::Iter<(Term, Term)> {
        self.map.iter()
    }

    pub(super) fn branch(&mut self) {
        self.branch += 1;
    }

    pub(super) fn len(&self) -> usize {
        self.map.len()
    }

    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    pub(super) fn truncate(&mut self, len: usize) {
        self.map.truncate(len)
    }

    /// Recursively find the earliest key pointing to this value.
    pub(super) fn find_origin(&self, mut val: Term) -> Term {
        loop {
            match self.map.iter().find(|(_, v)| *v == val) {
                None => return val,
                Some((key, _)) => val = key.clone(),
            }
        }
    }

    /// Initialize uninitialized variables.
    pub(super) fn init(&self, term: &Term) -> Term {
        match term {
            Variable(id, UNINIT) => Variable(id.to_string(), self.branch),
            Variable(_, _) => self.get(term).unwrap_or(term).clone(),
            Struct(id, args) => Struct(id.to_string(), self.init_all(args)),
            Rule(head, body) => Rule(Box::new(self.init(head)), self.init_all(body)),
            other => other.clone(),
        }
    }

    /// Initialize uninitialized variables.
    pub(super) fn init_all(&self, terms: &[Term]) -> Vec<Term> {
        terms.iter().map(|term| self.init(term)).collect()
    }

    /// Substitute variables with their values.
    pub(super) fn subst(&self, term: &Term) -> Term {
        match term {
            Variable(_, _) => match self.get(term) {
                Some(term) => self.subst(term),
                None => term.clone(),
            },
            Struct(id, args) => Struct(id.to_string(), self.subst_all(args)),
            Rule(head, body) => Rule(Box::new(self.init(head)), self.subst_all(body)),
            other => other.clone(),
        }
    }

    /// Substitute variables with their values.
    pub(super) fn subst_all(&self, terms: &[Term]) -> Vec<Term> {
        terms.iter().map(|term| self.subst(term)).collect()
    }
}

#[cfg(test)]
impl<const SIZE: usize> From<[(Term, Term); SIZE]> for Vars {
    fn from(value: [(Term, Term); SIZE]) -> Self {
        Vars {
            map: value.to_vec(),
            branch: UNINIT,
        }
    }
}
