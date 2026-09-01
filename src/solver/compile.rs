use super::{Solver, byrd::ByrdBox};
use crate::{
    atom,
    database::Database,
    errors::Error,
    parser::{self, FileReader, Lexer},
    types::Term::{self, Atom, Question},
};
use std::borrow::BorrowMut;

pub fn file(path: &str, db: Database) -> Result<(), Error> {
    use parser::ParsingError::*;

    let mut reader = FileReader::from(path)?;
    let lex = &mut Lexer::from(&mut reader);

    loop {
        match parser::next(lex) {
            Ok(ref expr) => {
                if let Some(mut solver) = super::expr(expr, db.clone())? {
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

pub fn main(db: Database) -> Result<(), Error> {
    if db.query(&atom!("main")).is_some() {
        match expr(&Question(vec![atom!("main")]), db) {
            Ok(Some(mut solver)) => match solver.next() {
                Some(_) => (),
                None => return Err(Error::NoMatch),
            },
            Ok(None) => (),
            Err(err) => return Err(err),
        }
    }
    Ok(())
}

pub fn expr(term: &Term, mut db: Database) -> Result<Option<Solver>, Error> {
    match term {
        Question(goals) => {
            let solver = ByrdBox::from(goals, db.clone())?.iter();
            Ok(Some(solver))
        }
        _ => {
            db.borrow_mut().assert(term.clone())?;
            Ok(None)
        }
    }
}
