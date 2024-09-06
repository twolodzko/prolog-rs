use super::{byrd::ByrdBox, Solver};
use crate::{
    database::Database,
    errors::Error,
    parser::{self, FileReader, Lexer},
    types::Term::{self, Question},
};
use std::borrow::BorrowMut;

pub fn eval_file(path: &str, db: Database) -> Result<(), Error> {
    use parser::ParsingError::*;

    let mut reader = match FileReader::from(path) {
        Ok(reader) => reader,
        Err(err) => return Err(err.into()),
    };
    let lex = &mut Lexer::from(&mut reader);

    loop {
        match parser::next(lex) {
            Ok(ref expr) => {
                if let Some(mut solver) = eval_expr(expr, db.clone())? {
                    match solver.next() {
                        Some(Ok(_)) => (),
                        Some(Err(err)) => return Err(err),
                        None => return Err(Error::NoMatch),
                    }
                }
            }
            Err(Interrupted | EndOfInput) => return Ok(()),
            Err(msg) => return Err(msg.into()),
        }
    }
}

pub fn eval_expr(term: &Term, mut db: Database) -> Result<Option<Solver>, Error> {
    match term {
        Question(goals) => {
            let solver = ByrdBox::from(goals, db.clone())?.iter();
            Ok(Some(solver))
        }
        _ => {
            db.borrow_mut().assert(term)?;
            Ok(None)
        }
    }
}
