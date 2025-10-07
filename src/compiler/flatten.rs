use crate::types::Term;
use std::collections::VecDeque;

fn flatten(term: &Term) -> Vec<Term> {
    let mut reg = Vec::new();
    let mut queue = VecDeque::new();
    queue.push_front(term);

    while let Some(term) = queue.pop_front() {
        if !reg.contains(term) {
            reg.push(term.clone());
        }
        if let Term::Functor(_, args) = term {
            args.iter().for_each(|a| queue.push_back(a));
        }
    }

    for i in 0..reg.len() {
        if let Term::Functor(ref name, ref args) = &reg[i] {
            let args = args
                .iter()
                .map(|a| Term::Reference(reg.iter().position(|t| t == a).unwrap()))
                .collect();
            reg[i] = Term::Functor(name.to_string(), args);
        }
    }

    reg
}

#[cfg(test)]
mod tests {
    use super::flatten;
    use crate::{
        parser::{self, Lexer, StringReader},
        types::Term::{self},
    };
    use test_case::test_case;

    #[test_case(
        "?- p(Z, h(Z,W), f(W)).",
        "[p/3(#1, #2, #3), Z, h/2(#1, #4), f/1(#4), W]";
        "wam book example 1"
    )]
    #[test_case(
        "?- p(f(X), h(Y, f(a)), Y).",
        "[p/3(#1, #2, #3), f/1(#4), h/2(#3, #5), Y, X, f/1(#6), a/0]";
        "wam book example 2"
    )]
    fn flattening(input: &str, expected: &str) {
        let reader = &mut StringReader::from(input);
        let lex = &mut Lexer::from(reader);
        let term = parser::next(lex).unwrap();
        let Term::Question(question) = term else {
            unreachable!()
        };
        let result = flatten(&question[0]);
        assert_eq!(format!("{:?}", result), expected)
    }
}
