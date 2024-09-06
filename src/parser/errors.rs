use rustyline::error::ReadlineError;
use std::fmt;

#[derive(PartialEq, Debug, Clone)]
pub enum ParsingError {
    EndOfInput,
    Interrupted,
    Invalid(char),
    Unexpected(String),
    Missing(String),
    IoError(String),
    SyntaxError,
}

impl fmt::Display for ParsingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ParsingError::*;
        match self {
            Interrupted => write!(f, "interrupted"),
            EndOfInput => write!(f, "end of input"),
            Invalid(ch) => write!(f, "invalid character: '{}'", ch),
            Missing(msg) => write!(f, "missing {}", msg),
            IoError(msg) => msg.fmt(f),
            SyntaxError => write!(f, "syntax error"),
            Unexpected(token) => write!(f, "unexpected token: {}", token),
        }
    }
}

impl From<ReadlineError> for ParsingError {
    fn from(value: ReadlineError) -> Self {
        match value {
            ReadlineError::Eof => ParsingError::EndOfInput,
            ReadlineError::Interrupted => ParsingError::Interrupted,
            other => ParsingError::IoError(other.to_string()),
        }
    }
}
