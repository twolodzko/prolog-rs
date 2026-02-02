mod byrd;
mod cmp;
pub mod compile;
mod math;
mod unify;
mod vars;

pub use byrd::{ByrdBox, Solver};
pub use compile::{expr, file, main};
pub(crate) use unify::unify;
pub use vars::Vars;

#[cfg(test)]
mod tests;

pub static mut TRACE: bool = false;
