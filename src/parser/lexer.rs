use super::{ParsingError, Reader};
use core::fmt;

#[derive(Debug, Clone, PartialEq)]
pub(super) enum Token {
    Atom(String),
    Variable(String),
    Number(String),
    Op(String, u16, bool),
    Bracket(char), // '(', ')'
    Curly(char),   // '{', '}'
    List(char),    // '[', '|', ']'
    Not,           // '\+'
    Question,      // '?-'
    Implies,       // ':-'
    Comma,         // ','
    Dot,           // '.'
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Token::*;
        match self {
            Atom(s) | Variable(s) | Number(s) | Op(s, _, _) => write!(f, "{}", s),
            Bracket(c) | Curly(c) | List(c) => write!(f, "{}", c),
            Not => write!(f, "\\+"),
            Question => write!(f, "?-"),
            Implies => write!(f, ":-"),
            Comma => write!(f, ","),
            Dot => write!(f, "."),
        }
    }
}

pub struct Lexer<'a> {
    reader: &'a mut dyn Reader,
    cache: Option<Token>,
}

impl<'a, R> From<&'a mut R> for Lexer<'a>
where
    R: Reader,
{
    fn from(value: &'a mut R) -> Self {
        Lexer {
            reader: value,
            cache: None,
        }
    }
}

impl<'a> Lexer<'a> {
    pub(super) fn peek(&mut self) -> Result<Token, ParsingError> {
        loop {
            match self.cache {
                Some(ref token) => return Ok(token.clone()),
                None => self.cache = Some(self.read_token()?),
            }
        }
    }

    pub(super) fn skip(&mut self) {
        self.cache = None;
    }

    pub(super) fn next(&mut self) -> Result<Token, ParsingError> {
        let result = self.peek();
        self.skip();
        result
    }

    fn read_token(&mut self) -> Result<Token, ParsingError> {
        use Token::*;

        self.skip_whitespace()?;
        let c = self.reader.peek()?;
        let token = match c {
            '%' => {
                loop {
                    if let Ok('\n') = self.reader.next() {
                        break;
                    }
                }
                return self.read_token();
            }
            '\'' => self.read_atom()?,
            '.' => {
                self.reader.skip();
                Dot
            }
            ',' => {
                self.reader.skip();
                Comma
            }
            '(' | ')' => {
                self.reader.skip();
                Bracket(c)
            }
            '{' | '}' => {
                self.reader.skip();
                Curly(c)
            }
            '[' | '|' | ']' => {
                self.reader.skip();
                List(c)
            }
            '!' => {
                self.reader.skip();
                Atom("!".to_string())
            }
            c if c.is_numeric() => {
                let num = self.read_while(|c| c.is_numeric())?;
                Number(num)
            }
            c if is_symbol(c) => {
                let id = self.read_while(is_symbol)?;
                token_type(id)
            }
            c if c.is_lowercase() => {
                let id = self.read_while(is_identifier)?;
                token_type(id)
            }
            c if c.is_uppercase() || c == '_' => {
                let id = self.read_while(is_identifier)?;
                Variable(id)
            }
            c => {
                self.reader.skip();
                return Err(ParsingError::Invalid(c));
            }
        };
        Ok(token)
    }

    fn read_while(&mut self, condition: fn(char) -> bool) -> Result<String, ParsingError> {
        let mut chars: Vec<char> = vec![];
        loop {
            match self.reader.peek() {
                Ok(c) => {
                    if !condition(c) {
                        break;
                    }
                    self.reader.skip();
                    chars.push(c);
                }
                Err(ParsingError::EndOfInput) => break,
                Err(msg) => return Err(msg),
            }
        }
        Ok(chars.into_iter().collect())
    }

    fn read_atom(&mut self) -> Result<Token, ParsingError> {
        // https://www.swi-prolog.org/pldoc/man?section=charescapes
        // https://github.com/saghm/unescape-rs/blob/master/src/lib.rs
        use Token::Atom;
        self.reader.skip();
        let mut id = Vec::new();
        loop {
            match self.reader.next()? {
                '\'' => return Ok(Atom(id.iter().collect())),
                '\\' => (),
                c => {
                    id.push(c);
                    continue;
                }
            }
            match self.reader.next()? {
                '"' => id.push('"'),
                '\'' => id.push('\''),
                '\\' => id.push('\\'),
                '\n' => id.push('\n'),
                'n' => id.push('\n'),
                'r' => id.push('\r'),
                's' => id.push(' '),
                't' => id.push('\t'),
                other => return Err(ParsingError::Invalid(other)),
            }
        }
    }

    fn skip_whitespace(&mut self) -> Result<(), ParsingError> {
        loop {
            match self.reader.peek() {
                Ok(c) => {
                    if !c.is_whitespace() {
                        break;
                    }
                    self.reader.skip();
                }
                Err(ParsingError::EndOfInput) => break,
                Err(msg) => return Err(msg),
            }
        }
        Ok(())
    }

    pub fn read_char(&mut self) -> Result<char, ParsingError> {
        let char = self.reader.peek()?;
        self.reader.skip();
        Ok(char)
    }

    pub fn drain(&mut self) {
        self.reader.drain();
    }
}

fn token_type(token: String) -> Token {
    use Token::*;
    match token.as_str() {
        "\\+" => return Not,
        ":-" => return Implies,
        "?-" => return Question,
        _ => (),
    }
    match precedence(&token) {
        Some((prec, left)) => Op(token, prec, left),
        None => Atom(token),
    }
}

fn is_identifier(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn is_symbol(c: char) -> bool {
    matches!(
        c,
        '+' | '-'
            | '*'
            | '/'
            | '\\'
            | '^'
            | '~'
            | ':'
            | ';'
            | '?'
            | '@'
            | '#'
            | '$'
            | '&'
            | '>'
            | '<'
            | '='
    )
}

fn precedence(op: &str) -> Option<(u16, bool)> {
    // https://www.swi-prolog.org/pldoc/man?section=operators
    let res = match op {
        "-" => (500, true),
        "->" => (1050, false),
        "," => (1000, false),
        ";" => (1100, false),
        ":-" => (1200, true),
        "@<" => (700, true),
        "@=<" => (700, true),
        "@>" => (700, true),
        "@>=" => (700, true),
        "*" => (400, true),
        "**" => (200, true),
        "/" => (400, true),
        "//" => (400, true),
        "\\" => (200, true),
        "\\=" => (700, true),
        "\\==" => (700, true),
        "+" => (500, true),
        "<" => (700, true),
        "=:=" => (700, true),
        "=" => (700, true),
        "=\\=" => (700, true),
        "=<" => (700, true),
        "==" => (700, true),
        ">" => (700, true),
        ">=" => (700, true),
        "div" => (400, true),
        "is" => (700, true),
        "mod" => (400, true),
        "rem" => (400, true),
        _ => return None,
    };
    Some(res)
}

/// Check if the string is recognized as an operator by the parser.
pub fn is_operator(op: &str) -> bool {
    precedence(op).is_some()
}

#[cfg(test)]
mod tests {
    use super::{
        Lexer,
        Token::{self, *},
    };
    use crate::parser::StringReader;
    use test_case::test_case;

    #[test_case(
        "foo.",
        &[Atom("foo".to_string()), Dot];
        "single atom"
    )]
    #[test_case(
        "?- foo, 2 + 2 > 3.",
        &[
            Question,
            Atom("foo".to_string()),
            Comma,
            Number("2".to_string()),
            Op("+".to_string(), 500, true),
            Number("2".to_string()),
            Op(">".to_string(), 700, true),
            Number("3".to_string()),
            Dot,
        ];
        "question"
    )]
    #[test_case(
        "foo(a,b) :- bar(a).",
        &[
            Atom("foo".to_string()),
            Bracket('('),
            Atom("a".to_string()),
            Comma,
            Atom("b".to_string()),
            Bracket(')'),
            Implies,
            Atom("bar".to_string()),
            Bracket('('),
            Atom("a".to_string()),
            Bracket(')'),
            Dot,
        ];
        "rule"
    )]
    #[test_case(
        "'start\\'\\n=\\\"\\\nend'.",
        &[Atom("start'\n=\"\nend".to_string()), Dot];
        "atom uses escape characters"
    )]
    fn lex(input: &str, expected: &[Token]) {
        let reader = &mut StringReader::from(input);
        let mut lex = Lexer::from(reader);
        let mut result = Vec::new();
        while let Ok(token) = lex.peek() {
            result.push(token);
            lex.skip();
        }
        assert_eq!(expected, result)
    }
}
