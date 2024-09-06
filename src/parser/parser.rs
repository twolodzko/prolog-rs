use super::{
    lexer::{Lexer, Token},
    ParsingError,
};
use crate::types::Term::{self, *};

pub fn next(lex: &mut Lexer) -> Result<Term, ParsingError> {
    let expr = match lex.next()? {
        Token::Op(id, _, _) | Token::Atom(id) => {
            let term = read_struct(lex, id)?;
            if let Token::Implies = lex.peek()? {
                lex.skip();
                let body = read_seq(lex)?;
                Rule(Box::new(term), body)
            } else {
                term
            }
        }
        Token::Question => {
            let question = read_seq(lex)?;
            Question(question)
        }
        other => {
            return Err(ParsingError::Unexpected(other.to_string()));
        }
    };
    expect(lex, Token::Dot)?;
    Ok(expr)
}

fn read_struct(lex: &mut Lexer, id: String) -> Result<Term, ParsingError> {
    if let Ok(Token::Bracket('(')) = lex.peek() {
        lex.skip();
        let args = read_brackets(lex)?;
        if !args.is_empty() {
            return Ok(Struct(id, args));
        }
    }
    Ok(Atom(id))
}

fn read_seq(lex: &mut Lexer) -> Result<Vec<Term>, ParsingError> {
    let mut seq = Vec::new();
    if is_boundary(&lex.peek()?) {
        return Ok(seq);
    }
    loop {
        let term = read_operation(lex)?;
        seq.push(term);

        match lex.peek()? {
            Token::Comma => lex.skip(),
            ref token if is_boundary(token) => break,
            other => return Err(ParsingError::Unexpected(other.to_string())),
        }
    }
    Ok(seq)
}

fn is_boundary(token: &Token) -> bool {
    matches!(
        token,
        Token::Bracket(')')
            | Token::List(']')
            | Token::Curly('}')
            | Token::List('|')
            | Token::Comma
            | Token::Dot
    )
}

fn read_operation(lex: &mut Lexer) -> Result<Term, ParsingError> {
    pratt_parser(lex, 1200)
}

fn pratt_parser(lex: &mut Lexer, max_bp: u16) -> Result<Term, ParsingError> {
    // Unlike [1],[2],[3], the precedence scores in Prolog's documentation are reversed,
    // so instead of checking for smaller precedence when doing the check in the loop,
    // we need to check for higher precedence.
    //
    // [1]: https://martin.janiczek.cz/2023/07/03/demystifying-pratt-parsers.html
    // [2]: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    // [3]: https://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/

    let mut lhs = read_term(lex)?;
    loop {
        match lex.peek()? {
            ref token if is_boundary(token) => break,
            Token::Op(op, mut prec, left) => {
                if prec >= max_bp {
                    break;
                }
                lex.skip();

                if !left {
                    prec += 1;
                }
                let rhs = pratt_parser(lex, prec)?;

                lhs = Struct(op, vec![lhs, rhs])
            }
            other => return Err(ParsingError::Unexpected(other.to_string())),
        }
    }
    Ok(lhs)
}

fn read_term(lex: &mut Lexer) -> Result<Term, ParsingError> {
    let term = match lex.next()? {
        Token::Atom(id) => read_struct(lex, id)?,
        Token::Variable(id) => {
            if id == "_" {
                Any
            } else {
                Variable(id, 0)
            }
        }
        Token::Number(val) => {
            let num = val.parse().unwrap();
            Number(num)
        }
        op @ Token::Not => {
            let expr = read_operation(lex)?;
            Struct(op.to_string(), vec![expr])
        }
        Token::Op(ref op, _, _) if op == "+" || op == "-" => match lex.peek()? {
            // a struct
            Token::Bracket('(') => {
                lex.skip();
                let args = read_brackets(lex)?;
                Struct(op.to_string(), args)
            }
            // prefix operator
            _ => match read_term(lex)? {
                Number(val) => Number(-val),
                term => Struct(op.to_string(), vec![term]),
            },
        },
        Token::Op(ref op, _, _) => {
            expect(lex, Token::Bracket('('))?;
            let args = read_brackets(lex)?;
            Struct(op.to_string(), args)
        }
        Token::Bracket('(') => {
            lex.skip();
            let body = read_brackets(lex)?;
            match body.len() {
                0 => Atom("()".to_string()),
                1 => body[0].clone(),
                _ => Struct(",".to_string(), body),
            }
        }
        Token::Curly('{') => {
            lex.skip();
            let body = read_seq(lex)?;
            expect(lex, Token::Curly('}'))?;
            Struct(
                "{}".to_string(),
                match to_and(body) {
                    Some(term) => vec![term],
                    None => Vec::new(),
                },
            )
        }
        Token::List('[') => {
            lex.skip();
            read_list(lex)?
        }
        other => return Err(ParsingError::Unexpected(other.to_string())),
    };
    Ok(term)
}

fn read_brackets(lex: &mut Lexer) -> Result<Vec<Term>, ParsingError> {
    let body = read_seq(lex)?;
    expect(lex, Token::Bracket(')'))?;
    Ok(body)
}

fn read_list(lex: &mut Lexer) -> Result<Term, ParsingError> {
    let head = read_seq(lex)?;
    let tail = match lex.next()? {
        Token::List('|') => {
            if head.is_empty() {
                return Err(ParsingError::SyntaxError);
            }
            let term = read_term(lex)?;
            expect(lex, Token::List(']'))?;
            term
        }
        Token::List(']') => {
            if head.is_empty() {
                return Ok(Nil);
            }
            Nil
        }
        other => return Err(ParsingError::Unexpected(other.to_string())),
    };
    // pack it into Cons list
    Ok(make_list(&head, tail))
}

/// Convert [head | tail] to a cons list.
pub(crate) fn make_list(head: &[Term], mut tail: Term) -> Term {
    use Term::Struct;
    for elem in head.iter().rev() {
        tail = Struct(".".to_string(), vec![elem.clone(), tail])
    }
    tail
}

fn to_and(mut seq: Vec<Term>) -> Option<Term> {
    let tail = match seq.pop() {
        Some(rhs) => match seq.pop() {
            Some(lhs) => vec![lhs, rhs],
            None => return Some(rhs),
        },
        None => return None,
    };
    let init = Struct(",".to_string(), tail);
    let term = seq
        .iter()
        .rev()
        .cloned()
        .fold(init, |acc, x| Struct(",".to_string(), vec![x, acc]));
    Some(term)
}

fn expect(lex: &mut Lexer, expected: Token) -> Result<(), ParsingError> {
    let token = lex.next()?;
    if token != expected {
        return Err(ParsingError::Unexpected(token.to_string()));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::{lexer::Lexer, FileReader, StringReader},
        types::Term::{self, *},
        var,
    };
    use test_case::test_case;

    #[test_case(
        "foo.",
        Atom("foo".to_string());
        "simple atom fact"
    )]
    #[test_case(
        "bar(a, 1).",
        Struct("bar".to_string(), vec![Atom("a".to_string()), Number(1)]);
        "simple struct fact"
    )]
    #[test_case(
        "yes(_).",
        Struct("yes".to_string(), vec![Any]);
        "wildcard"
    )]
    #[test_case(
        "baz(a) :- foo(a).",
        Rule(
            Box::new(Struct("baz".to_string(), vec![Atom("a".to_string())])),
            vec![Struct("foo".to_string(), vec![Atom("a".to_string())])]
        );
        "simple rule"
    )]
    #[test_case(
        "bar(a,b,c) :- a,b,c.",
        Rule(
            Box::new(Struct("bar".to_string(), vec![Atom("a".to_string()), Atom("b".to_string()), Atom("c".to_string())])),
            vec![Atom("a".to_string()), Atom("b".to_string()), Atom("c".to_string())]
        );
        "rule with and"
    )]
    #[test_case(
        "fizzbuzz(X) :- X mod 3 =:= 0, X mod 5 =:= 0.",
        Rule(
            Box::new(Struct("fizzbuzz".to_string(), vec![var!("X")])),
            vec![
                Struct("=:=".to_string(), vec![
                    Struct("mod".to_string(), vec![
                        var!("X"),
                        Number(3),
                    ]),
                    Number(0)
                ]),
                Struct("=:=".to_string(), vec![
                    Struct("mod".to_string(), vec![
                        var!("X"),
                        Number(5),
                    ]),
                    Number(0)
                ])
            ]
        );
        "fizbuzz rule"
    )]
    #[test_case(
        "?- foo.",
        Question(vec![Atom("foo".to_string())]);
        "simple question"
    )]
    #[test_case(
        "?- 1+2*3>4.",
        Question(vec![
            Struct(">".to_string(), vec![
                Struct("+".to_string(), vec![
                    Number(1),
                    Struct("*".to_string(), vec![
                        Number(2), Number(3)
                    ]),
                ]),
                Number(4)
            ])
        ]);
        "question with arithmetics"
    )]
    #[test_case(
        "?- (1+2)*3>4,a->b;c.",
        Question(vec![
            Struct(">".to_string(), vec![
                Struct("*".to_string(), vec![
                    Struct("+".to_string(), vec![
                        Number(1), Number(2)
                    ]),
                    Number(3),
                ]),
                Number(4)
            ]),
            Struct(";".to_string(), vec![
                Struct("->".to_string(), vec![
                    Atom("a".to_string()), Atom("b".to_string())
                ]),
                Atom("c".to_string())
            ])
        ]);
        "question with operator precedence"
    )]
    #[test_case(
        "?- 1+2-3+4.",
        Question(vec![
            Struct("+".to_string(), vec![
                Struct("-".to_string(), vec![
                    Struct("+".to_string(), vec![
                        Number(1), Number(2),
                    ]),
                    Number(3)
                ]),
                Number(4)
            ])
        ]);
        "question left associative"
    )]
    #[test_case(
        "?- a;b;c;d.",
        Question(vec![
            Struct(";".to_string(), vec![
                Atom("a".to_string()),
                Struct(";".to_string(), vec![
                    Atom("b".to_string()),
                    Struct(";".to_string(), vec![
                        Atom("c".to_string()), Atom("d".to_string()),
                    ]),
                ]),
            ])
        ]);
        "question right associative"
    )]
    #[test_case(
        "?- [] = [].",
        Question(vec![
            Struct("=".to_string(), vec![
                Nil, Nil,
            ])
        ]);
        "question empty list"
    )]
    #[test_case(
        "?- [1,2] = [1,2|[]].",
        Question(vec![
            Struct("=".to_string(), vec![
                Struct(".".to_string(), vec![Number(1), Struct(".".to_string(), vec![Number(2), Nil])]),
                Struct(".".to_string(), vec![Number(1), Struct(".".to_string(), vec![Number(2), Nil])]),
            ])
        ]);
        "question non-empty list"
    )]
    #[test_case(
        "?- [1,2,[3]|[]] = [1,2|[3]],[].",
        Question(vec![
            Struct("=".to_string(), vec![
                Struct(".".to_string(), vec![
                    Number(1),
                    Struct(".".to_string(), vec![
                        Number(2),
                        Struct(".".to_string(), vec![
                            Struct(".".to_string(), vec![Number(3), Nil]),
                            Nil,
                        ])
                    ])
                ]),
                Struct(".".to_string(), vec![
                    Number(1),
                    Struct(".".to_string(), vec![
                        Number(2),
                        Struct(".".to_string(), vec![Number(3), Nil]),
                    ]),
                ])
            ]),
            Nil
        ]);
        "question more lists"
    )]
    #[test_case(
        "?- +(1, 2).",
        Question(vec![Struct("+".to_string(), vec![Number(1), Number(2)])]);
        "operator in front"
    )]
    #[test_case(
        "?- [1|X] = [1,2].",
        Question(vec![
            Struct("=".to_string(), vec![
                Struct(".".to_string(), vec![Number(1), var!("X")]),
                Struct(".".to_string(), vec![Number(1), Struct(".".to_string(), vec![Number(2), Nil])]),
            ])
        ]);
        "two lists"
    )]
    #[test_case(
        "max2(X,Y,Max) :- (X >= Y, !, Max = X) ; Max = Y.",
        Rule(
            Box::new(Struct("max2".to_string(), vec![
                Variable("X".to_string(), 0),
                Variable("Y".to_string(), 0),
                Variable("Max".to_string(), 0),
            ])),
            vec![Struct(";".to_string(), vec![
                Struct(",".to_string(), vec![
                    Struct(">=".to_string(), vec![
                        Variable("X".to_string(), 0),
                        Variable("Y".to_string(), 0),
                    ]),
                    Atom("!".to_string()),
                    Struct("=".to_string(), vec![
                        Variable("Max".to_string(), 0),
                        Variable("X".to_string(), 0),
                    ])
                ]),
                Struct("=".to_string(), vec![
                    Variable("Max".to_string(), 0),
                    Variable("Y".to_string(), 0),
                ])
            ])]
        );
        "max2 rule"
    )]
    fn parser(input: &str, expected: Term) {
        let reader = &mut StringReader::from(input);
        let mut lex = Lexer::from(reader);
        assert_eq!(Ok(expected), super::next(&mut lex));
    }

    #[test_case("'foo'(1, 2).", "foo(1, 2)."; "quoted atoms")]
    #[test_case("?- [a].", "?- [a|[]]."; "short lists")]
    #[test_case("?- [1,2,3,4].", "?- [1|[2|[3|[4]]]]."; "long lists")]
    #[test_case("?- 1 + 2 * 3.", "?- +(1, *(2, 3))."; "operation precedence")]
    #[test_case("?- (1 + 2) * 3.", "?- +(1, 2) * 3."; "quotes")]
    #[test_case("?- is(X, 2+2).", "?- X is 2+2."; "is operator")]
    #[test_case("?- -(5).", "?- -(((5)))."; "skip redundant brackets")]
    #[test_case("?- 1 + 2 div 3 * 6.", "?- 1 + *(2 div 3, 6)."; "mixed operators and structs")]
    #[test_case("?- \\+ foo.", "?- \\+ (foo)."; "negation and brackets")]
    // this is inconsistent with prolog, but this is how it works now
    #[test_case("?- A, B, (C ; D), E, F.", "?- A, B, C ; D, E, F."; "or with brackets")]
    fn parsing_equalities(lhs: &str, rhs: &str) {
        let reader = &mut StringReader::from(lhs);
        let mut lex = Lexer::from(reader);
        let lhs_parsed = super::next(&mut lex);

        let reader = &mut StringReader::from(rhs);
        let mut lex = Lexer::from(reader);
        let rhs_parsed = super::next(&mut lex);

        assert_eq!(lhs_parsed, rhs_parsed);
    }

    #[test]
    fn read_file() {
        let reader = &mut FileReader::from("examples/mortal.pl").unwrap();
        let mut lex = Lexer::from(reader);

        let mut facts = 0;
        let mut rules = 0;
        let mut questions = 0;

        while let Ok(term) = super::next(&mut lex) {
            match term {
                Atom(_) | Struct(_, _) => facts += 1,
                Rule(_, _) => rules += 1,
                Question(_) => questions += 1,
                other => panic!("unexpected: {:?}", other),
            }
        }

        assert_eq!(facts, 4);
        assert_eq!(rules, 1);
        assert_eq!(questions, 3);
    }
}
