use super::ParsingError;
use rustyline::{Config, DefaultEditor};
use std::{
    borrow::BorrowMut,
    fs::File,
    io::{BufRead, BufReader, Lines},
    iter::Peekable,
    vec::IntoIter,
};

const PROMPT: &str = "| ";
pub static mut USE_PROMPT: bool = true;

pub fn switch_prompt() {
    unsafe {
        USE_PROMPT = !USE_PROMPT;
    }
}

pub trait Reader {
    fn skip(&mut self);
    fn peek(&mut self) -> Result<char, ParsingError>;
    fn drain(&mut self);

    fn next(&mut self) -> Result<char, ParsingError> {
        let c = self.peek()?;
        self.skip();
        Ok(c)
    }
}

pub struct StringReader {
    cache: Peekable<IntoIter<char>>,
}

impl StringReader {
    pub fn from(s: &str) -> StringReader {
        StringReader {
            cache: s.chars().collect::<Vec<_>>().into_iter().peekable(),
        }
    }

    fn empty() -> StringReader {
        StringReader::from("")
    }
}

impl Reader for StringReader {
    fn skip(&mut self) {
        self.cache.next();
    }

    fn peek(&mut self) -> Result<char, ParsingError> {
        self.cache.peek().ok_or(ParsingError::EndOfInput).cloned()
    }

    fn drain(&mut self) {
        self.cache.borrow_mut().for_each(drop);
    }
}

pub struct FileReader {
    lines: Lines<BufReader<File>>,
    iter: StringReader,
}

impl FileReader {
    pub fn from(filename: &str) -> Result<Self, ParsingError> {
        let file = File::open(filename).map_err(|msg| ParsingError::IoError(msg.to_string()))?;
        let mut lines = BufReader::new(file).lines();
        let iter = FileReader::next_line(&mut lines)?;
        Ok(FileReader { lines, iter })
    }

    fn next_line(lines: &mut Lines<BufReader<File>>) -> Result<StringReader, ParsingError> {
        match lines.next() {
            Some(Ok(line)) => Ok(StringReader::from(&format!("{}\n", line))),
            Some(Err(msg)) => Err(ParsingError::IoError(msg.to_string())),
            None => Err(ParsingError::EndOfInput),
        }
    }
}

impl Reader for FileReader {
    fn skip(&mut self) {
        self.iter.skip()
    }

    fn peek(&mut self) -> Result<char, ParsingError> {
        loop {
            match self.iter.peek() {
                Err(ParsingError::EndOfInput) => {
                    self.iter = FileReader::next_line(&mut self.lines)?
                }
                result => return result,
            }
        }
    }

    fn drain(&mut self) {
        self.iter.drain();
    }
}

pub struct StdinReader {
    reader: DefaultEditor,
    buffer: StringReader,
}

impl StdinReader {
    pub fn new() -> Result<Self, ParsingError> {
        let config = Config::builder().auto_add_history(true).build();
        let reader = match DefaultEditor::with_config(config) {
            Ok(editor) => editor,
            Err(msg) => return Err(ParsingError::IoError(msg.to_string())),
        };
        Ok(StdinReader {
            reader,
            buffer: StringReader::empty(),
        })
    }

    fn next_line(&mut self) -> Result<StringReader, ParsingError> {
        let prompt = if unsafe { USE_PROMPT } { PROMPT } else { "" };
        match self.reader.readline(prompt) {
            Ok(line) => Ok(StringReader::from(&format!("{}\n", line))),
            Err(err) => Err(err.into()),
        }
    }
}

impl Reader for StdinReader {
    fn skip(&mut self) {
        self.buffer.skip()
    }

    fn peek(&mut self) -> Result<char, ParsingError> {
        loop {
            match self.buffer.peek() {
                Err(ParsingError::EndOfInput) => self.buffer = self.next_line()?,
                result => return result,
            }
        }
    }

    fn drain(&mut self) {
        self.buffer.drain();
    }
}

#[cfg(test)]
mod tests {
    use super::{FileReader, ParsingError, Reader, StringReader};

    #[test]
    fn string_reader() {
        let mut r = StringReader::from("hello");
        assert_eq!(r.next(), Ok('h'));
        assert_eq!(r.peek(), Ok('e'));
        assert_eq!(r.peek(), Ok('e'));
        assert_eq!(r.next(), Ok('e'));
        assert_eq!(r.next(), Ok('l'));
        assert_eq!(r.next(), Ok('l'));
        assert_eq!(r.next(), Ok('o'));
        assert_eq!(r.next(), Err(ParsingError::EndOfInput));
        assert_eq!(r.next(), Err(ParsingError::EndOfInput));
        assert_eq!(r.peek(), Err(ParsingError::EndOfInput));
    }

    #[test]
    fn file_reader() {
        // FileReader works the same as just iterating over the lines and characters

        use std::fs::File;
        use std::io::{BufRead, BufReader};

        let filename = "src/parser/reader.rs";

        let file = BufReader::new(File::open(&filename).expect("Unable to open file"));
        let chars = &mut Vec::<char>::new();
        for line in file.lines() {
            for ch in line.expect("Unable to read line").chars() {
                chars.push(ch);
            }
            chars.push('\n');
        }

        let reader = &mut FileReader::from(filename).unwrap();
        for ch in chars {
            assert_eq!(Ok(*ch), reader.next());
        }
    }
}
