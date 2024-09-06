use super::Vars;
use crate::types::Term;
use std::iter::zip;

impl Term {
    fn ord(&self) -> u8 {
        use Term::*;
        match self {
            Variable(_, _) | Any => 0,
            Number(_) => 1,
            Atom(_) => 2,
            Struct(_, _) | Nil => 3,
            _ => unreachable!(),
        }
    }
}

impl Vars {
    #[allow(clippy::only_used_in_recursion)]
    pub(super) fn cmp(&self, lhs: &Term, rhs: &Term) -> std::cmp::Ordering {
        use std::cmp::Ordering::*;
        use Term::*;

        match lhs.ord().cmp(&rhs.ord()) {
            Equal => (),
            other => return other,
        };
        match (lhs, rhs) {
            (Any, _) => Less,
            (_, Any) => Greater,
            (Nil, Nil) => Equal,
            // variables should be sorted by address,
            // but here they don't reserve memory, so it is not possible
            // comparing them by scope seems the closest we can get
            (Variable(_, lhs), Variable(_, rhs)) => lhs.cmp(rhs),
            (Number(lhs), Number(rhs)) => lhs.cmp(rhs),
            (Atom(lhs), Atom(rhs)) => lhs.cmp(rhs),
            (Struct(lhs_id, lhs_args), Struct(rhs_id, rhs_args)) => {
                match lhs_args.len().cmp(&rhs_args.len()) {
                    Equal => (),
                    other => return other,
                };
                match lhs_id.cmp(rhs_id) {
                    Equal => (),
                    other => return other,
                };
                zip(lhs_args, rhs_args)
                    .find_map(|(a, b)| match self.cmp(a, b) {
                        Equal => None,
                        other => Some(other),
                    })
                    .unwrap_or(Equal)
            }
            _ => unreachable!(),
        }
    }
}

impl PartialOrd for Term {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use std::cmp::Ordering::Equal;
        use Term::*;
        match self.ord().partial_cmp(&other.ord()) {
            Some(Equal) | None => (),
            other => return other,
        };
        match (self, other) {
            (Nil, Nil) => Some(Equal),
            (Variable(_, _), Variable(_, _)) => todo!(),
            (Atom(lhs), Atom(rhs)) => lhs.partial_cmp(rhs),
            (Struct(lhs_id, lhs_args), Struct(rhs_id, rhs_args)) => {
                match lhs_args.len().partial_cmp(&rhs_args.len()) {
                    Some(Equal) | None => (),
                    other => return other,
                };
                match lhs_id.partial_cmp(rhs_id) {
                    Some(Equal) | None => (),
                    other => return other,
                };
                // this shou,kd search for first non equal
                zip(lhs_args, rhs_args).find_map(|(a, b)| a.partial_cmp(b))
            }
            _ => unreachable!(),
        }
    }
}
