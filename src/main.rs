use prologrs::{
    database::Database,
    parser::{
        self, Lexer,
        ParsingError::{EndOfInput, Interrupted},
        StdinReader,
    },
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
        println!("{:?}", expr);
    }
}

pub fn print_help() {
    println!("{} [-e][-h] [FILE...]\n", env::args().next().unwrap());
    println!(" -e, --exit\trun the main/0 goal and exit");
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
        // if let Err(msg) = eval_file(stdlib, db.clone()) {
        //     err!(format!("failed to load stdlib: {}", msg));
        // }
    }

    for path in files {
        // if let Err(msg) = eval_file(&path, db.clone()) {
        //     err!(msg);
        //     std::process::exit(1);
        // }
    }

    if exit {
        // if let Err(msg) = eval_main(db) {
        //     err!(msg);
        //     std::process::exit(1);
        // }
    } else {
        repl(db)
    }
}
