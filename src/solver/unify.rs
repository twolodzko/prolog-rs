use super::vars::Vars;
use crate::types::Term::{self, *};
use std::iter::zip;

/// Unification algorithm.
///
/// * Atoms are checked for equality.
/// * The wildcard "_" unifies with anything.
/// * Compound terms have same IDs, arity, and elements.
/// * Variable unifies with a value if it is not initialized
///   or it was initialized with a value that unifies with the new value.
pub(crate) fn unify(lhs: &Term, rhs: &Term, vars: &mut Vars) -> bool {
    use Term::*;
    match (lhs, rhs) {
        (Any, _) | (_, Any) => true,
        (Struct(lhs_id, lhs_args), Struct(rhs_id, rhs_args)) => {
            if lhs_id != rhs_id || lhs_args.len() != rhs_args.len() {
                return false;
            }
            let prev_vars = vars.len();
            if !unify_all(lhs_args, rhs_args, vars) {
                // rollback the changes
                vars.truncate(prev_vars);
                return false;
            }
            true
        }
        (var @ Variable(_, _), val) | (val, var @ Variable(_, _)) => unify_var(var, val, vars),
        (lhs, rhs) => lhs == rhs,
    }
}

fn unify_all(lhs: &[Term], rhs: &[Term], vars: &mut Vars) -> bool {
    zip(lhs, rhs).all(|(a, b)| unify(a, b, vars))
}

fn unify_var(var: &Term, val: &Term, vars: &mut Vars) -> bool {
    if var == val {
        return true;
    }
    if let Some(ref new) = vars.get(var).cloned() {
        return unify(new, val, vars);
    }
    // Dereferencing the value to avoid invalid unification for cycles, as described in
    // Norvig "Correcting A Widespread Error in Unification Algorithms"
    // https://norvig.com/unify-bug.pdf
    if let Variable(_, _) = val {
        if let Some(ref val) = vars.get(val).cloned() {
            return unify(var, val, vars);
        }
    };
    vars.insert(var.clone(), val.clone());
    true
}
