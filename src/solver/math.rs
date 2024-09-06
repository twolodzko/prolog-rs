use super::Vars;
use crate::{errors::Error, types::Term};

pub(super) fn eval(mut term: Term, vars: &Vars) -> Result<Term, Error> {
    use Term::*;
    loop {
        match term {
            Number(_) => return Ok(term),
            Struct(ref id, ref args) if args.len() == 1 => {
                let num = match eval(args[0].clone(), vars)? {
                    Number(val) => val,
                    other => return Err(Error::TypeError(other)),
                };
                let val = match id.as_str() {
                    "-" => -num,
                    "+" => num,
                    "abs" => num.abs(),
                    "sign" => num.signum(),
                    _ => return Err(Error::TypeError(term)),
                };
                return Ok(Number(val));
            }
            Struct(ref id, ref args) if args.len() == 1 && id == "+" => {
                return eval(args[0].clone(), vars)
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
                    _ => return Err(Error::ArithError(term)),
                }
            }
            Variable(_, _) => match vars.get(&term) {
                Some(val) => term = val.clone(),
                None => {
                    return {
                        let var = vars.find_origin(term.clone());
                        Err(Error::UnsetVar(var.to_string()))
                    }
                }
            },
            _ => return Err(Error::ArithError(term)),
        }
    }
}

pub(super) fn eval_args(args: &[Term], vars: &Vars) -> Result<(i32, i32), Error> {
    use Term::Number;
    debug_assert!(args.len() == 2);

    let lhs = eval(args[0].clone(), vars)?;
    let rhs = eval(args[1].clone(), vars)?;
    match (lhs, rhs) {
        (Number(lhs), Number(rhs)) => Ok((lhs, rhs)),
        (Number(_), other) | (other, _) => Err(Error::TypeError(other)),
    }
}
