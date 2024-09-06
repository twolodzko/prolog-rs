mod byrd;
mod cmp;
mod eval;
mod math;
mod unify;
mod vars;

pub use byrd::{ByrdBox, Solver};
pub use eval::{eval_expr, eval_file};
pub(crate) use unify::unify;
pub use vars::Vars;

#[cfg(test)]
mod tests;

pub static mut TRACE: bool = false;
