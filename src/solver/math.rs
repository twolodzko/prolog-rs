use std::borrow::Cow;

use super::Vars;
use crate::{errors::Error, types::Term};

pub(super) fn eval(term: &Term, vars: &Vars) -> Result<Term, Error> {
    use Term::*;
    let mut term = Cow::Borrowed(term);
    loop {
        match term.as_ref() {
            Number(_) => return Ok(term.into_owned()),
            Struct(ref id, ref args) if args.len() == 1 => {
                let num = match eval(&args[0], vars)? {
                    Number(val) => val,
                    other => return Err(Error::TypeError(other)),
                };
                let val = match id.as_str() {
                    "-" => -num,
                    "+" => num,
                    "abs" => num.abs(),
                    "sign" => num.signum(),
                    _ => return Err(Error::TypeError(term.into_owned())),
                };
                return Ok(Number(val));
            }
            Struct(ref id, ref args) if args.len() == 1 && id == "+" => {
                return eval(&args[0], vars)
            }
            Struct(ref id, ref args) if args.len() == 2 => {
                let (lhs, rhs) = eval_args(args, vars)?;
                match id.as_str() {
                    "+" => return Ok(Number(lhs + rhs)),
                    "-" => return Ok(Number(lhs - rhs)),
                    "*" => return Ok(Number(lhs * rhs)),
                    "/" | "//" => return Ok(Number(lhs / rhs)),
                    "div" => return Ok(Number(lhs.div_euclid(rhs))),
                    "rem" => return Ok(Number(lhs % rhs)),
                    "mod" => return Ok(Number(lhs.rem_euclid(rhs))),
                    _ => return Err(Error::ArithError(term.into_owned())),
                }
            }
            Variable(_, _) => match vars.get(&term) {
                Some(val) => term = Cow::Borrowed(val),
                None => {
                    return {
                        let var = vars.find_origin(&term);
                        Err(Error::UnsetVar(var.to_string()))
                    }
                }
            },
            _ => return Err(Error::ArithError(term.into_owned())),
        }
    }
}

pub(super) fn eval_args(args: &[Term], vars: &Vars) -> Result<(i32, i32), Error> {
    use Term::Number;
    debug_assert!(args.len() == 2);

    let lhs = eval(&args[0], vars)?;
    let rhs = eval(&args[1], vars)?;
    match (lhs, rhs) {
        (Number(lhs), Number(rhs)) => Ok((lhs, rhs)),
        (Number(_), other) | (other, _) => Err(Error::TypeError(other)),
    }
}
