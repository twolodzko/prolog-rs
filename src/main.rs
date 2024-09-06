use prologrs::{
    database::Database,
    errors::Error,
    parser::{
        self, switch_prompt, Lexer,
        ParsingError::{EndOfInput, Interrupted},
        StdinReader,
    },
    solver::{eval_expr, eval_file, Solver},
};
use std::env;

macro_rules! err {
    ( $msg:expr ) => {
        println!("Error: {}", $msg)
    };
}

fn repl(db: Database) {
    println!("Press ^C to exit. Press enter key or type ; for more solutions.\n");

    let mut reader = StdinReader::new().unwrap();
    let lex = &mut Lexer::from(&mut reader);

    loop {
        let expr = match parser::next(lex) {
            Ok(expr) => expr,
            Err(Interrupted | EndOfInput) => return,
            Err(msg) => {
                lex.drain();
                err!(msg);
                continue;
            }
        };
        let solver = match eval_expr(&expr, db.clone()) {
            Ok(Some(solver)) => solver,
            Ok(None) => continue,
            Err(msg) => {
                lex.drain();
                err!(msg);
                continue;
            }
        };
        if let Err(err) = print_solutions(solver, lex) {
            lex.drain();
            err!(err);
        }
    }
}

fn print_solutions(mut solver: Solver, lex: &mut Lexer) -> Result<(), Error> {
    loop {
        match solver.next() {
            Some(Ok(vars)) => {
                if vars.is_empty() {
                    println!("yes");
                    return Ok(());
                }
                for (k, v) in vars.iter() {
                    println!("{} = {}", k, v);
                }
            }
            None => {
                println!("no");
                return Ok(());
            }
            Some(Err(err)) => {
                return Err(err);
            }
        }

        // wait for input what to do next
        switch_prompt();
        lex.drain();
        let result = lex.read_char();
        switch_prompt();

        match result {
            Ok(';' | '\n') => (),
            Ok(_) => return Ok(()),
            Err(err) => {
                lex.drain();
                return Err(err.into());
            }
        }
    }
}

pub fn print_help() {
    println!("{} [-e][-h] [FILE...]\n", env::args().next().unwrap());
    println!(" -e, --exit\timmediately exit after evaluating file(s)");
    println!(" -n, --no-std\tdo not load the standard library");
    println!(" -h, --help\tdisplay this help");
}

fn main() {
    let mut no_std = false;
    let mut exit = false;
    let mut files = Vec::new();
    for arg in env::args().skip(1) {
        match arg.as_ref() {
            "-h" | "--help" => {
                print_help();
                return;
            }
            "-e" | "--exit" => exit = true,
            "-n" | "--no-std" => no_std = true,
            name => files.push(name.to_string()),
        }
    }

    let db = Database::new();

    if !no_std {
        let stdlib = "lib/stdlib.pl";
        if let Err(msg) = eval_file(stdlib, db.clone()) {
            err!(format!("failed to load stdlib: {}", msg));
        }
    }

    for path in files {
        if let Err(msg) = eval_file(&path, db.clone()) {
            err!(msg);
            std::process::exit(1);
        }
    }

    if !exit {
        repl(db)
    }
}
