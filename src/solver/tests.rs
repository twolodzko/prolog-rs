use super::eval_file;
use crate::{
    atom,
    database::Database,
    errors::Error,
    parser::{self, Lexer, ParsingError, StringReader},
    solver::{
        byrd::ByrdBox,
        eval_expr, unify,
        vars::{self, Vars},
    },
    structure,
    types::Term::{self, *},
};
use test_case::test_case;

macro_rules! init_var {
    ( $id:expr ) => {
        Variable($id.to_string(), vars::BASE)
    };
}

#[test_case(
    Any,
    Any,
    true;
    "wildcards"
)]
#[test_case(
    atom!("foo"),
    atom!("foo"),
    true;
    "atoms"
)]
#[test_case(
    structure!("foo", Number(1), Number(2)),
    structure!("foo", Number(1), Number(2)),
    true;
    "structs"
)]
#[test_case(
    Nil,
    Nil,
    true;
    "empty lists"
)]
#[test_case(
    Any,
    structure!("foo", Number(1), Number(2)),
    true;
    "wildcard vs struct"
)]
fn unify_terms(lhs: Term, rhs: Term, expected: bool) {
    let mut vars = Vars::default();
    let result = unify(&lhs, &rhs, &mut vars);
    assert_eq!(result, expected);
}

#[test]
fn unify_variable() {
    let mut vars = Vars::default();

    let var = init_var!("X");
    let val = structure!("foo", Number(1), Number(2));

    let result = unify(&var, &val, &mut vars);
    assert!(result);
    vars.prune();
    assert_eq!(vars.get(&var), Some(&val));
}

#[test]
fn unify_lists() {
    let mut vars = Vars::default();

    // [1|X]
    // [1,2|Y]
    let lhs = structure!(".", Number(1), init_var!("X"));
    let rhs = structure!(".", Number(1), structure!(".", Number(2), init_var!("Y")));

    let result = unify(&lhs, &rhs, &mut vars);
    assert!(result);
}

#[test_case(
    "?- fail.",
    None;
    "eval fail"
)]
#[test_case(
    "foo.
    ?- foo.",
    Some(Ok(Vars::default()));
    "simple atom"
)]
#[test_case(
    "foo(a).
    ?- foo(a).",
    Some(Ok(Vars::default()));
    "simple struct"
)]
#[test_case(
    "foo(a).
    foo(b).
    foo(X) :- X = c.
    foo(d).
    ?- foo(other).",
    None;
    "nothing matches"
)]
#[test_case(
    "foo(a).
    foo(b).
    foo(c).
    ?- foo(c).",
    Some(Ok(Vars::default()));
    "not first clause passes"
)]
#[test_case(
    "foo(a).
    bar(X) :- foo(X).
    ?- bar(a).",
    Some(Ok(Vars::default()));
    "simple rule"
)]
#[test_case(
    "failing(X) :- fail.
    ?- failing(Y).",
    None;
    "failing rule"
)]
#[test_case(
    "foo(X) :- fail.
    foo(X) :- X = 1.
    ?- foo(Y).",
    Some(Ok(Vars::from([(init_var!("Y"), Number(1))])));
    "fail and backtrack"
)]
#[test_case(
    "?- \\+ 1=2.",
    Some(Ok(Vars::default()));
    "not false"
)]
#[test_case(
    "true.
    ?- \\+ true.",
    None;
    "not true"
)]
#[test_case(
    "?- \\+ \\+ 1=2.",
    None;
    "double negation"
)]
#[test_case(
    "?- \\+ \\+ \\+ 1=2.",
    Some(Ok(Vars::default()));
    "triple negation"
)]
#[test_case(
    "?- X = X.",
    Some(Ok(Vars::default()));
    "tautology with variables"
)]
#[test_case(
    "?- 1 = 1.",
    Some(Ok(Vars::default()));
    "tautology with numbers"
)]
#[test_case(
    "?- 1 = 2.",
    None;
    "constants that don't unify"
)]
#[test_case(
    "?- [1,2,3] = [1|[2,3]].",
    Some(Ok(Vars::default()));
    "unify function"
)]
#[test_case(
    "person(socrates).
    mortal(Who) :- person(Who).
    ?- mortal(socrates).",
    Some(Ok(Vars::default()));
    "mortal positive"
)]
#[test_case(
    "person(socrates).
    mortal(Who) :- person(Who).
    ?- \\+ mortal(zeus).",
    Some(Ok(Vars::default()));
    "mortal negative"
)]
#[test_case(
    "person(socrates).
    mortal(Who) :- person(Who).
    ?- mortal(Who).",
    Some(Ok(Vars::from([(init_var!("Who"), atom!("socrates"))])));
    "mortal search"
)]
#[test_case(
    "?- X = 1, X = X.",
    Some(Ok(Vars::from([(init_var!("X"), Number(1))])));
    "unify with self"
)]
#[test_case(
    "?- Y = X, X = 1.",
    Some(Ok(Vars::from([
        (init_var!("Y"), init_var!("X")),
        (init_var!("X"), Number(1))
    ])));
    "unify X with Y"
)]
#[test_case(
    "?- X = Y, X = 1.",
    Some(Ok(Vars::from([
        (init_var!("X"), init_var!("Y")),
        (init_var!("Y"), Number(1))
    ])));
    "unify Y with X"
)]
#[test_case(
    "?- X=Y, Y=Z, Z=2, X=2.",
    Some(Ok(Vars::from([
        (init_var!("X"), init_var!("Y")),
        (init_var!("Y"), init_var!("Z")),
        (init_var!("Z"), Number(2))
    ])));
    "unification is transitive"
)]
#[test_case(
    "foo(X, Y) :- X = 1, Y = 2.
    bar(Y, X) :- foo(Y, X).
    ?- bar(1, 2).",
    Some(Ok(Vars::default()));
    "variable name clash"
)]
#[test_case(
    "foo(1, 2, 3).
    foo(1, 1, X).
    ?- foo(X, X, 3).",
    Some(Ok(Vars::from([(init_var!("X"), Number(1))])));
    "duplicated variable names"
)]
#[test_case(
    "foo(X).
    ?- foo(X), X = a.",
    Some(Ok(Vars::from([(init_var!("X"), atom!("a"))])));
    "two goals sharing variable"
)]
#[test_case(
    "append([],X,X).
    ?- append(X,Y,[a,b,c]), X = [], Y = [a,b,c].",
    Some(Ok(Vars::from([
        (init_var!("X"), Nil),
        (init_var!("Y"), parser::make_list(&[atom!("a"), atom!("b"), atom!("c")], Nil))
    ])));
    "dependent rules"
)]
#[test_case(
    "append([],X,X).
    append([A|B],C,[A|D]) :- append(B,C,D).
    ?- append(X,Y,[a,b,c]), X = [], Y = [a,b,c].",
    Some(Ok(Vars::from([
        (init_var!("X"), Nil),
        (init_var!("Y"), parser::make_list(&[atom!("a"), atom!("b"), atom!("c")], Nil))
    ])));
    "backtracking example"
)]
#[test_case(
    "append([],X,X).
    append([A|B],C,[A|D]) :- append(B,C,D).
    ?- append([1,2,3],[a,b,c],[1,2,3,a,b,c]).",
    Some(Ok(Vars::default()));
    "simple append"
)]
#[test_case(
    "append([],X,X).
    append([A|B],C,[A|D]) :- append(B,C,D).
    ?- append([],[a,b,c],[a,b,c]).",
    Some(Ok(Vars::default()));
    "append empty"
)]
#[test_case(
    "append([],X,X).
    append([A|B],C,[A|D]) :- append(B,C,D).
    ?- append([a,b,c],[],[a,b,c]).",
    Some(Ok(Vars::default()));
    "append to empty"
)]
#[test_case(
    "append([],X,X).
    append([A|B],C,[A|D]) :- append(B,C,D).
    ?- append([],[],[]).",
    Some(Ok(Vars::default()));
    "append two empty lists"
)]
#[test_case(
    "p(a).
    p(b).
    q(b).
    q(c).
    ?- p(X), p(Y), q(X), Y=a.",
    Some(Ok(Vars::from([
        (init_var!("X"), atom!("b")),
        (init_var!("Y"), atom!("a"))
    ])));
    "backtracking on series of goals"
)]
#[test_case(
    "foo(X).
    ?- foo(Y), fail.",
    None;
    "ends with fail"
)]
#[test_case(
    "foo(X) :- X = 1, fail.
    foo(X) :- X = 2.
    ?- foo(Y).",
    Some(Ok(Vars::from([(init_var!("Y"), Number(2))])));
    "try next clause after failure"
)]
#[test_case(
    "one(a).
    two(X) :- one(X).
    three(Y) :- two(Y).
    four(Z) :- three(Z).
    ?- four(X).",
    Some(Ok(Vars::from([(init_var!("X"), atom!("a"))])));
    "nested rules"
)]
#[test_case(
    "foo(a).
    bar(X) :- foo(X).
    baz(Y) :- bar(Y).
    ?- A=B, B=C, baz(C), B=b.",
    None;
    "indirection"
)]
#[test_case(
    "foo(X, Y).
    ?- foo(a, b), X = c.",
    Some(Ok(Vars::from([(init_var!("X"), atom!("c"))])));
    "masking names"
)]
#[test_case(
    "?- p(X, Y) = p(Y, X).",
    Some(Ok(Vars::from([(init_var!("X"), init_var!("Y"))])));
    "example 1 from Norvig"
)]
#[test_case(
    "?- p(X, Y, a) = p(Y, X, X).",
    Some(Ok(Vars::from([
        (init_var!("X"), init_var!("Y")),
        (init_var!("Y"), atom!("a")),
    ])));
    "example 2 from Norvig"
)]
#[test_case(
    "p(a).
    q(b).
    ?- p(X), \\+ q(X), X=a.",
    Some(Ok(Vars::from([(init_var!("X"), atom!("a"))])));
    "negation between other goal"
)]
#[test_case(
    "?- 1=1 ; 1=2.",
    Some(Ok(Vars::default()));
    "true or false"
)]
#[test_case(
    "?- 1=2 ; 1=1.",
    Some(Ok(Vars::default()));
    "false or true"
)]
#[test_case(
    "?- 1=2 ; 2=1.",
    None;
    "false or false"
)]
#[test_case(
    "?- 1=1, 1=2 ; 1=1.",
    Some(Ok(Vars::default()));
    "and or precedence 1"
)]
#[test_case(
    "?- 1=1 ; 1=2, 1=1.",
    Some(Ok(Vars::default()));
    "and or precedence 2"
)]
#[test_case(
    "true.
    ?- (X=1,Y=2), (X=Y;true).",
    Some(Ok(Vars::from([
        (init_var!("X"), Number(1)),
        (init_var!("Y"), Number(2)),
    ])));
    "braces and or"
)]
#[test_case(
    "?- ((X=1,Y=2) ; fail), X=1.",
    Some(Ok(Vars::from([
        (init_var!("X"), Number(1)),
        (init_var!("Y"), Number(2)),
    ])));
    "nested and in or"
)]
#[test_case(
    "foo(a).
    bar(Y).
    ?- foo(X), ( X = other ; bar(X) ).",
    Some(Ok(Vars::from([
        (init_var!("X"), atom!("a"))
    ])));
    "or in a question"
)]
#[test_case(
    "?- 5 is 5.",
    Some(Ok(Vars::default()));
    "trivial is"
)]
#[test_case(
    "?- 5 =:= 5.",
    Some(Ok(Vars::default()));
    "trivial equality"
)]
#[test_case(
    ">(A, B) :- B < A.
    ?- X=5, Y=4, X>Y.",
    Some(Ok(Vars::from([
        (init_var!("X"), Number(5)),
        (init_var!("Y"), Number(4)),
    ])));
    "variables, arithmetic, and inequality"
)]
#[test_case(
    "?- -2 =:= 2-4.",
    Some(Ok(Vars::default()));
    "basic math"
)]
#[test_case(
    // The implementation is different, but it is consistent with
    // the requirement of being consistent with mod/2.
    // https://www.swi-prolog.org/pldoc/man?function=div%2f2
    "?- X = 73,
        Y = 12,
        Q is div(X, Y),
        M is mod(X, Y),
        X =:= Y*Q+M.",
    Some(Ok(Vars::from([
        (init_var!("X"), Number(73)),
        (init_var!("Y"), Number(12)),
        (init_var!("Q"), Number(6)),
        (init_var!("M"), Number(1)),
    ])));
    "div is consistent with mod"
)]
#[test_case(
    "cons([]).
    cons([H | T]) :- cons(T).
    ?- cons([]).",
    Some(Ok(Vars::default()));
    "cons for nil"
)]
#[test_case(
    "cons([]).
    cons([H | T]) :- cons(T).
    ?- cons([1]).",
    Some(Ok(Vars::default()));
    "cons for single element list"
)]
#[test_case(
    "cons([]).
    cons([H | T]) :- cons(T).
    ?- cons([1,2|[3|[]]]).",
    Some(Ok(Vars::default()));
    "cons for complex list"
)]
#[test_case(
    "cons([]).
    cons([H | T]) :- cons(T).
    ?- cons([incorrect|wrong]).",
    None;
    "cons for dotted pair"
)]
#[test_case(
    "?- [A|B] = [1].",
    Some(Ok(Vars::from([
        (init_var!("A"), Number(1)),
        (init_var!("B"), Nil),
    ])));
    "unify cons list"
)]
#[test_case(
    "last([H|[]], Res) :- Res = H.
    last([_ | T], Res) :- last(T, Res).
    ?- last([1,2,3], X).",
    Some(Ok(Vars::from([(init_var!("X"), Number(3))])));
    "last"
)]
#[test_case(
    "foo(a).
    foo(b).
    bar(X) :- foo(X), X = b.
    ?- bar(Y).",
    Some(Ok(Vars::from([(init_var!("Y"), atom!("b"))])));
    "simple backtracking"
)]
#[test_case(
    "?- X=1, [H|T] = [X,2].",
    Some(Ok(Vars::from([
        (init_var!("X"), Number(1)),
        (init_var!("H"), Number(1)), // this might be just H=X
        (init_var!("T"), structure!(".", Number(2), Nil)),
    ])));
    "var replacement in lists"
)]
#[test_case{
    "is_list(X) :- var(X), !, fail.
    is_list([]).
    is_list([_|T]) :- is_list(T).
    ?- is_list(no_its_not).",
    None;
    "is_list for a non-list"
}]
#[test_case{
    "is_list(X) :- var(X), !, fail.
    is_list([]).
    is_list([_|T]) :- is_list(T).
    ?- is_list([]).",
    Some(Ok(Vars::default()));
    "is_list for a nil"
}]
#[test_case{
    "is_list(X) :- var(X), !, fail.
    is_list([]).
    is_list([_|T]) :- is_list(T).
    ?- is_list([a,b,c]).",
    Some(Ok(Vars::default()));
    "is_list for a list"
}]
#[test_case{
    "is_list(X) :- var(X), !, fail.
    is_list([]).
    is_list([_|T]) :- is_list(T).
    ?- is_list([a|b]).",
    None;
    "is_list for a dotted pair"
}]
#[test_case{
    "?- consult('lib/stdlib.pl').
    ?- true.",
    Some(Ok(Vars::default()));
    "consult"
}]
#[test_case{
    "?- var(X).",
    Some(Ok(Vars::default()));
    "var for non-initialized variable"
}]
#[test_case{
    "?- X=1, var(X).",
    None;
    "var for initialized variable"
}]
#[test_case{
    "?- var([]).",
    None;
    "var for non-variable"
}]
#[test_case{
    "ident(X) :- X.
    ?- ident(1=2).",
    None;
    "identity failing"
}]
#[test_case{
    "ident(X) :- X.
    ?- ident(1=1).",
    Some(Ok(Vars::default()));
    "identity succeeding"
}]
#[test_case{
    "?- X = 2 + 2, Y is X + 1.",
    Some(Ok(Vars::from([
        (init_var!("X"), structure!("+", Number(2), Number(2))),
        (init_var!("Y"), Number(5)),
    ])));
    "evaluate variables 1"
}]
#[test_case{
    "run(X) :- X.
    ?- run(Y is 2 + 2).",
    Some(Ok(Vars::from([(init_var!("Y"), Number(4))])));
    "evaluate variables 2"
}]
#[test_case{
    "run(A, B) :- A, B.
    ?- run(X is 2 + 2, X =:= +(2,+(1,1))).",
    Some(Ok(Vars::from([(init_var!("X"), Number(4))])));
    "evaluate variables 3"
}]
#[test_case(
    ">(A, B) :- B < A.
    =<(A, B) :- \\+ (A > B).
    is_prime(N) :- integer(N), N > 1, \\+ has_factor(N, 2).
    has_factor(N, K) :- K * K =< N, N mod K =:= 0.
    has_factor(N, K) :- K * K =< N, K_ is K + 1, has_factor(N, K_).
    ?- is_prime(4).",
    None;
    "is prime for 4"
)]
#[test_case(
    ">(A, B) :- B < A.
    =<(A, B) :- \\+ (A > B).
    is_prime(N) :- integer(N), N > 1, \\+ has_factor(N, 2).
    has_factor(N, K) :- K * K =< N, N mod K =:= 0.
    has_factor(N, K) :- K * K =< N, K_ is K + 1, has_factor(N, K_).
    ?- is_prime(7).",
    Some(Ok(Vars::default()));
    "is prime for 7"
)]
#[test_case(
    "?- {_, X} = {1=2,2=2}, X.",
    Some(Ok(Vars::from([
        (init_var!("X"), Struct("=".to_string(), vec![Number(2), Number(2)]))
    ])));
    "curly brackets extract"
)]
#[test_case(
    "?- {X} = {1=1,2=2}, X.",
    Some(Ok(Vars::from([
        (init_var!("X"), Struct(",".to_string(), vec![
            Struct("=".to_string(), vec![Number(1), Number(1)]),
            Struct("=".to_string(), vec![Number(2), Number(2)])
        ]))
    ])));
    "curly brackets with succeeding and"
)]
#[test_case(
    "?- {X} = {1=2,2=2}, X.",
    None;
    "curly brackets with failing and for first arg"
)]
#[test_case(
    "?- {X} = {1=1,2=1}, X.",
    None;
    "curly brackets with failing and for second arg"
)]
#[test_case(
    "?- X @< 1.",
    Some(Ok(Vars::default()));
    "variable is smaller than number"
)]
#[test_case(
    "?- 1 @< a.",
    Some(Ok(Vars::default()));
    "number is smaller than atom"
)]
#[test_case(
    "?- a @< a(b).",
    Some(Ok(Vars::default()));
    "atom is smaller than struct"
)]
#[test_case(
    "?- a @< [].",
    Some(Ok(Vars::default()));
    "atom is smaller than nil"
)]
#[test_case(
    "?- X @< x.",
    Some(Ok(Vars::default()));
    "variable is smaller than atom"
)]
#[test_case(
    "?- 1 @< 2.",
    Some(Ok(Vars::default()));
    "numbers are compared by value"
)]
#[test_case(
    "?- aaa @< bbb.",
    Some(Ok(Vars::default()));
    "atoms are compared alphabetically"
)]
#[test_case(
    "?- foo(a, b) @< bar(a, b, c).",
    Some(Ok(Vars::default()));
    "compound terms compared by arity"
)]
#[test_case(
    "?- bar(a, b) @< foo(a, b).",
    Some(Ok(Vars::default()));
    "compound terms compared by name"
)]
#[test_case(
    "?- foo(1, 2) @< foo(1, []).",
    Some(Ok(Vars::default()));
    "compound terms compared recursively"
)]
#[test_case(
    "?- a == a.",
    Some(Ok(Vars::default()));
    "same atoms"
)]
#[test_case(
    "?- 42 == 42.",
    Some(Ok(Vars::default()));
    "same numbers"
)]
#[test_case(
    "?- [] == [].",
    Some(Ok(Vars::default()));
    "same nils"
)]
#[test_case(
    "?- [a,b,c] == [a,b,c].",
    Some(Ok(Vars::default()));
    "same lists"
)]
#[test_case(
    "?- [a,b,c] == [a,b,c,d].",
    None;
    "shorter vs longer list"
)]
#[test_case(
    "?- [a,b,c,d] == [a,b,c].",
    None;
    "longer vs shorter list"
)]
fn parse_and_eval(input: &str, expected: Option<Result<Vars, Error>>) {
    unsafe {
        super::TRACE = true;
    }

    let db = Database::new();
    let mut reader = StringReader::from(input);
    let lex = &mut Lexer::from(&mut reader);

    loop {
        match parser::next(lex) {
            Ok(ref expr) => {
                if let Some(mut solver) = eval_expr(expr, db.clone()).unwrap() {
                    let result = solver.next();
                    if result != expected {
                        panic!(
                            "failed test case:\n\t{}\nexpected: {:?}\ngot:      {:?}",
                            input, expected, result
                        )
                    }
                    println!("ok")
                }
            }
            Err(ParsingError::EndOfInput) => return,
            Err(msg) => panic!("{}", msg),
        }
    }
}

// FIXME: this test passes, just it throws error at different stage
// #[test_case(
//     "",
//     "?- im_not_there.",
//     Err(Error::Unknown(atom!("im_not_there")));
//     "non-existent atom"
// )]
#[test_case(
    "foo(a).
    foo(b).
    foo(c).",
    "?- foo(X).",
    Ok(vec![
        Vars::from([(init_var!("X"), atom!("a"))]),
        Vars::from([(init_var!("X"), atom!("b"))]),
        Vars::from([(init_var!("X"), atom!("c"))]),
    ]);
    "multiple facts"
)]
#[test_case(
    "",
    "?- 1 = 1.",
    Ok(vec![Vars::default()]);
    "tautology"
)]
#[test_case(
    "",
    "?- 1 = 2.",
    Ok(vec![]);
    "trivially false"
)]
#[test_case(
    "a.",
    "?- a.",
    Ok(vec![Vars::default()]);
    "single fact"
)]
#[test_case(
    "bar(1).
    foo(X) :- bar(X).",
    "?- foo(Y).",
    Ok(vec![Vars::from([(init_var!("Y"), Number(1))])]);
    "rule with 1 clause"
)]
#[test_case(
    "",
    "?- \\+ 1=2.",
    Ok(vec![Vars::default()]);
    "not false"
)]
#[test_case(
    "",
    "?- X = 1 ; Y = 2.",
    Ok(vec![
        Vars::from([(init_var!("X"), Number(1))]),
        Vars::from([(init_var!("Y"), Number(2))]),
    ]);
    "or"
)]
#[test_case(
    "",
    "?- (X = 1 ; Y = 2), (X = 3 ; Y = 4).",
    Ok(vec![
        Vars::from([(init_var!("X"), Number(1)), (init_var!("Y"), Number(4))]),
        Vars::from([(init_var!("Y"), Number(2)), (init_var!("X"), Number(3))]),
    ]);
    "two ors"
)]
#[test_case(
    "p(a).
    p(b).
    q(c).
    q(d).",
    "?- p(X) ; q(X).",
    Ok(vec![
        Vars::from([(init_var!("X"), atom!("a"))]),
        Vars::from([(init_var!("X"), atom!("b"))]),
        Vars::from([(init_var!("X"), atom!("c"))]),
        Vars::from([(init_var!("X"), atom!("d"))]),
    ]);
    "or with branches"
)]
#[test_case(
    "p(a).
    p(b).
    q(c).
    q(d).",
    "?- (p(X), !) ; q(X).",
    Ok(vec![
        Vars::from([(init_var!("X"), atom!("a"))])
    ]);
    "or with branches and cut"
)]
#[test_case(
    "num(1).
    num(2).",
    "?- num(X), Y is X+1.",
    Ok(vec![
        Vars::from([(init_var!("X"), Number(1)), (init_var!("Y"), Number(2))]),
        Vars::from([(init_var!("X"), Number(2)), (init_var!("Y"), Number(3))]),
    ]);
    "backtracking and math"
)]
#[test_case(
    "foo(a).
    foo(b).
    foo(c).",
    "?- foo(X).",
    Ok(vec![
        Vars::from([(init_var!("X"), atom!("a"))]),
        Vars::from([(init_var!("X"), atom!("b"))]),
        Vars::from([(init_var!("X"), atom!("c"))]),
    ]);
    "several facts"
)]
#[test_case(
    "foo(a).
    foo(b).
    foo(c).",
    "?- foo(X), foo(Y).",
    Ok(vec![
        Vars::from([(init_var!("X"), atom!("a")), (init_var!("Y"), atom!("a"))]),
        Vars::from([(init_var!("X"), atom!("a")), (init_var!("Y"), atom!("b"))]),
        Vars::from([(init_var!("X"), atom!("a")), (init_var!("Y"), atom!("c"))]),
        Vars::from([(init_var!("X"), atom!("b")), (init_var!("Y"), atom!("a"))]),
        Vars::from([(init_var!("X"), atom!("b")), (init_var!("Y"), atom!("b"))]),
        Vars::from([(init_var!("X"), atom!("b")), (init_var!("Y"), atom!("c"))]),
        Vars::from([(init_var!("X"), atom!("c")), (init_var!("Y"), atom!("a"))]),
        Vars::from([(init_var!("X"), atom!("c")), (init_var!("Y"), atom!("b"))]),
        Vars::from([(init_var!("X"), atom!("c")), (init_var!("Y"), atom!("c"))]),
    ]);
    "combinations"
)]
#[test_case(
    "bar(1).
    bar(2).
    bar(3).
    bar(4).
    bar(5).
    foo(X) :- bar(X).",
    "?- foo(Y).",
    Ok(vec![
        Vars::from([(init_var!("Y"), Number(1))]),
        Vars::from([(init_var!("Y"), Number(2))]),
        Vars::from([(init_var!("Y"), Number(3))]),
        Vars::from([(init_var!("Y"), Number(4))]),
        Vars::from([(init_var!("Y"), Number(5))]),
    ]);
    "rule with 5 clauses"
)]
#[test_case(
    "bar(b).
    bar(c).
    foo(a).
    foo(X) :- bar(X).",
    "?- foo(Y).",
    Ok(vec![
        Vars::from([(init_var!("Y"), atom!("a"))]),
        Vars::from([(init_var!("Y"), atom!("b"))]),
        Vars::from([(init_var!("Y"), atom!("c"))]),
    ]);
    "two stage unification"
)]
#[test_case(
    "my_append([],X,X).
    my_append([A|B],C,[A|D]) :- my_append(B,C,D).",
    "?- my_append(X,Y,[a,b,c]).",
    Ok(vec![
        Vars::from([
            (init_var!("X"), Nil),
            (init_var!("Y"), parser::make_list(&[atom!("a"), atom!("b"), atom!("c")], Nil)),
        ]),
        Vars::from([
            (init_var!("X"), structure!(".", atom!("a"), Nil)),
            (init_var!("Y"), parser::make_list(&[atom!("b"), atom!("c")], Nil)),
        ]),
        Vars::from([
            (init_var!("X"), parser::make_list(&[atom!("a"), atom!("b")], Nil)),
            (init_var!("Y"), structure!(".", atom!("c"), Nil)),
        ]),
        Vars::from([
            (init_var!("X"), parser::make_list(&[atom!("a"), atom!("b"), atom!("c")], Nil)),
            (init_var!("Y"), Nil),
        ]),
    ]);
    "recursive backtracking"
)]
#[test_case(
    "",
    "?- !.",
    Ok(vec![Vars::default()]);
    "trivial cut"
)]
#[test_case(
    "p(a).
    p(b) :- !.
    p(c).",
    "?- p(X).",
    Ok(vec![
        Vars::from([(init_var!("X"), atom!("a"))]),
        Vars::from([(init_var!("X"), atom!("b"))]),
    ]);
    "cut to skip branches"
)]
#[test_case(
    "foo(a).
    foo(b).
    foo(c).
    bar(X, Y) :- foo(X), !, foo(Y).",
    "?- bar(A, B).",
    Ok(vec![
        Vars::from([
            (init_var!("A"), atom!("a")),
            (init_var!("B"), atom!("a"))
        ]),
        Vars::from([
            (init_var!("A"), atom!("a")),
            (init_var!("B"), atom!("b"))
        ]),
        Vars::from([
            (init_var!("A"), atom!("a")),
            (init_var!("B"), atom!("c"))
        ]),
    ]);
    "cut for combinations"
)]
#[test_case(
    "foo(a).
    foo(b).
    foo(c).
    bar(X, Y, Z) :- foo(X), foo(Y), !, foo(Z).",
    "?- bar(A, B, C).",
    Ok(vec![
        Vars::from([
            (init_var!("A"), atom!("a")),
            (init_var!("B"), atom!("a")),
            (init_var!("C"), atom!("a"))
        ]),
        Vars::from([
            (init_var!("A"), atom!("a")),
            (init_var!("B"), atom!("a")),
            (init_var!("C"), atom!("b"))
        ]),
        Vars::from([
            (init_var!("A"), atom!("a")),
            (init_var!("B"), atom!("a")),
            (init_var!("C"), atom!("c"))
        ]),
    ]);
    "cut for combinations 2"
)]
#[test_case(
    // see: https://www.tutorialspoint.com/prolog/prolog_examples_of_cuts.htm
    "list_member(X,[X|_]) :- !.
    list_member(X,[_|TAIL]) :- list_member(X,TAIL).
    list_append(A,T,T) :- list_member(A,T), !.
    list_append(A,T,[A|T]).",
    "?- list_append(a, [a,b,c], L).",
    Ok(vec![Vars::from([
        (init_var!("L"), parser::make_list(&[atom!("a"), atom!("b"), atom!("c")], Nil)),
    ])]);
    "list append using cut"
)]
#[test_case(
    "max(X, Y, Max) :- X >= Y, !, Max = X.
    max(X, Y, Max) :- Max = Y.",
    "?- max(1, 2, N).",
    Ok(vec![Vars::from([
        (init_var!("N"), Number(2)),
    ])]);
    "max using cut lhs"
)]
#[test_case(
    "max2(X,Y,Max) :- (X >= Y, !, Max = X) ; Max = Y.",
    "?- max2(1, 2, N).",
    Ok(vec![Vars::from([
        (init_var!("N"), Number(2)),
    ])]);
    "max2 using cut lhs"
)]
#[test_case(
    "max2(X,Y,Max) :- (X >= Y, !, Max = X) ; Max = Y.",
    "?- max2(5, 2, N).",
    Ok(vec![Vars::from([
        (init_var!("N"), Number(5)),
    ])]);
    "max2 using cut rhs"
)]
#[test_case(
    "foo(A,B) :- member(A-B,[a-1,b-2,c-3,a-4]).
    foo(A,B) :- member(A-B,[d-5,e-6,a-7,f-8]).",
    "?- foo(a,B).",
    Ok(vec![
        Vars::from([(init_var!("B"), Number(1))]),
        Vars::from([(init_var!("B"), Number(4))]),
        Vars::from([(init_var!("B"), Number(7))]),
    ]);
    "member"
)]
#[test_case(
    "foo(A,B) :- member(A-B,[a-1,b-2,c-3,a-4]), !.
    foo(A,B) :- member(A-B,[d-5,e-6,a-7,f-8]).",
    "?- foo(a,B).",
    Ok(vec![
        Vars::from([(init_var!("B"), Number(1))]),
    ]);
    "member and cut"
)]
#[test_case(
    "foo(A,B) :- once(member(A-B,[a-1,b-2,c-3,a-4])).
    foo(A,B) :- member(A-B,[d-5,e-6,a-7,f-8]).",
    "?- foo(a,B).",
    Ok(vec![
        Vars::from([(init_var!("B"), Number(1))]),
        Vars::from([(init_var!("B"), Number(7))]),
    ]);
    "member and once"
)]
#[test_case(
    // see: https://pages.cs.wisc.edu/~fischer/cs538.s02/prolog/A13CUT.HTM
    "data(one).
    data(two).
    data(three).

    cut_test_a(X) :- data(X).
    cut_test_a('last clause').",
    "?- cut_test_a(X).",
    Ok(vec![
        Vars::from([(init_var!("X"), atom!("one"))]),
        Vars::from([(init_var!("X"), atom!("two"))]),
        Vars::from([(init_var!("X"), atom!("three"))]),
        Vars::from([(init_var!("X"), atom!("last clause"))]),
    ]);
    "cut_test_a"
)]
#[test_case(
    // see: https://pages.cs.wisc.edu/~fischer/cs538.s02/prolog/A13CUT.HTM
    "data(one).
    data(two).
    data(three).
    cut_test_b(X) :- data(X), !.
    cut_test_b('last clause').",
    "?- cut_test_b(X).",
    Ok(vec![
        Vars::from([(init_var!("X"), atom!("one"))]),
    ]);
    "cut_test_b"
)]
#[test_case(
    // see: https://pages.cs.wisc.edu/~fischer/cs538.s02/prolog/A13CUT.HTM
    "data(one).
    data(two).
    data(three).
    cut_test_c(X,Y) :- data(X), !, data(Y).",
    "?- cut_test_c(X, Y).",
    Ok(vec![
        Vars::from([
            (init_var!("X"), atom!("one")),
            (init_var!("Y"), atom!("one"))
        ]),
        Vars::from([
            (init_var!("X"), atom!("one")),
            (init_var!("Y"), atom!("two"))
        ]),
        Vars::from([
            (init_var!("X"), atom!("one")),
            (init_var!("Y"), atom!("three"))
        ]),
    ]);
    "cut_test_c"
)]
#[test_case(
    "data(one).
    data(two).
    data(three).
    foo(X) :- once(data(X)).
    foo(X) :- once(data(X)).",
    "?- foo(X).",
    Ok(vec![
        Vars::from([(init_var!("X"), atom!("one"))]),
        Vars::from([(init_var!("X"), atom!("one"))]),
    ]);
    "once twice"
)]
#[test_case(
    "foo(a).
    foo(b).
    foo(c).
    bar(1).
    bar(2).
    baz(X,Y) :- foo(X), once(bar(Y)).",
    "?- baz(X, Y).",
    Ok(vec![
        Vars::from([
            (init_var!("X"), atom!("a")),
            (init_var!("Y"), Number(1))
        ]),
        Vars::from([
            (init_var!("X"), atom!("b")),
            (init_var!("Y"), Number(1))
        ]),
        Vars::from([
            (init_var!("X"), atom!("c")),
            (init_var!("Y"), Number(1))
        ]),
    ]);
    "once example"
)]
#[test_case(
    // https://stackoverflow.com/a/18338089/3986320
    "between(N, M, K) :- N =< M, K = N.
    between(N, M, K) :- N < M, N1 is N+1, between(N1, M, K).",
    "?- between(1,5,X).",
    Ok(vec![
        Vars::from([(init_var!("X"), Number(1))]),
        Vars::from([(init_var!("X"), Number(2))]),
        Vars::from([(init_var!("X"), Number(3))]),
        Vars::from([(init_var!("X"), Number(4))]),
        Vars::from([(init_var!("X"), Number(5))]),
    ]);
    "between"
)]
#[test_case(
    // example from: https://cliplab.org/logalg/slides/3_prolog_language.pdf
    "qsort([],[]).
    qsort([X|L],S) :-          % X=5, L=[2,1,3,7,6]
        partition(L,X,LS,LB),  % LS=[2,1,3], LB=[7,6]
        qsort(LS,LSS),         % LSS=[1,2,3]
        qsort(LB,LBS),         % LBS=[6,7]
        append(LSS,[X|LBS],S). % call: append ([1,2,3],[5,6,7],S)
                               % S=[1,2,3,5,6,7]
    partition ([],_P,[],[]).
    partition ([E|R],P,[E|Smalls],Bigs) :-
        E < P,
        partition(R,P,Smalls,Bigs).
    partition([E|R],P,Smalls,[E|Bigs]) :-
        E >= P,
        partition(R,P,Smalls,Bigs).",
    "?- qsort([5,2,1,3,7,6],SL).",
    Ok(vec![
        Vars::from([
            (
                init_var!("SL"),
                parser::make_list(
                    &[Number(1), Number(2), Number(3), Number(5), Number(6), Number(7)],
                    Nil,
                ),
            ),
        ]),
    ]);
    "qsort example"
)]
#[test_case(
    // see: https://www.swi-prolog.org/pldoc/man?predicate=!%2f0
    "a(a0).
    a(a1).
    a(a2).
    b(b0).
    b(b1).
    b(b2).
    c(c0).
    c(c1).
    c(c2).
    t0(A,B) :- a(A), !, b(B).
    t0(x,x).",
    "?- t0(X, Y).",
    Ok(vec![
        Vars::from([
            (init_var!("X"), atom!("a0")),
            (init_var!("Y"), atom!("b0")),
        ]),
        Vars::from([
            (init_var!("X"), atom!("a0")),
            (init_var!("Y"), atom!("b1")),
        ]),
        Vars::from([
            (init_var!("X"), atom!("a0")),
            (init_var!("Y"), atom!("b2")),
        ]),
    ]);
    "t0 cut example"
)]
#[test_case(
    // see: https://www.swi-prolog.org/pldoc/man?predicate=!%2f0
    "a(a0).
    a(a1).
    a(a2).
    b(b0).
    b(b1).
    b(b2).
    c(c0).
    c(c1).
    c(c2).
    t1(A,B) :- (a(A), !, fail) ; b(B).
    t1(x,x).",
    "?- t1(X, Y).",
    Ok(vec![]);
    "t1 cut example"
)]
#[test_case(
    // see: https://www.swi-prolog.org/pldoc/man?predicate=!%2f0
    "a(a0).
    a(a1).
    a(a2).
    b(b0).
    b(b1).
    b(b2).
    c(c0).
    c(c1).
    c(c2).
    t2(A,B,C) :- (a(A) -> (b(B), !)) ; c(C).
    t2(x,x,x).",
    "?- t2(X, Y, Z).",
    Ok(vec![
        Vars::from([
            (init_var!("X"), atom!("a0")),
            (init_var!("Y"), atom!("b0")),
        ]),
    ]);
    "t2 cut example"
)]
#[test_case(
    // see: https://www.swi-prolog.org/pldoc/man?predicate=!%2f0
    "a(a0).
    a(a1).
    a(a2).
    b(b0).
    b(b1).
    b(b2).
    c(c0).
    c(c1).
    c(c2).
    t3(A,B) :- call(((a(A), !, fail) ; b(B))).
    t3(x,x).",
    "?- t3(X, Y).",
    Ok(vec![
        Vars::from([
            (init_var!("X"), atom!("x")),
            (init_var!("Y"), atom!("x")),
        ]),
    ]);
    "t3 cut example"
)]
#[test_case(
    // see: https://www.swi-prolog.org/pldoc/man?predicate=!%2f0
    "a(a0).
    a(a1).
    a(a2).
    b(b0).
    b(b1).
    b(b2).
    c(c0).
    c(c1).
    c(c2).
    t4(A) :- \\+ (a(A), !, fail).",
    "?- t4(X).",
    Ok(vec![
        Vars::from([
            (init_var!("X"), atom!("a0")),
        ]),
    ]);
    "t4 cut example"
)]
// The tests below come from the test suite by Jonathan Hodgson
// testing of conformance to the ISO Prolog standard
// see: http://www.deransart.fr/prolog/suites.html
#[test_case(
    "",
    "?- ','(X=1, var(X)).",
    Ok(vec![]);
    "iso - and 1"
)]
#[test_case(
    "",
    "?- ','(var(X), X=1).",
    Ok(vec![
        Vars::from([
            (init_var!("X"), Number(1)),
        ])
    ]);
    "iso - and 2"
)]
#[test_case(
    "",
    "?- ','(fail, call(3)).",
    Ok(vec![]);
    "iso - and 3"
)]
#[test_case(
    "",
    "?- ','(X = true, call(X)).",
    Ok(vec![
        Vars::from([
            (init_var!("X"), atom!("true")),
        ])
    ]);
    "iso - and 5"
)]
#[test_case(
    "",
    "?- '=\\\\='(0,1).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - arith diff 1"
)]
#[test_case(
    "",
    "?- '=\\\\='(3 * 2,7 - 1).",
    Ok(vec![]);
    "iso - arith diff 3"
)]
#[test_case(
    "",
    "?- '=\\\\='(N,5).",
    Err(Error::UnsetVar("N".to_string()));
    "iso - arith diff 4"
)]
#[test_case(
    "",
    "?- '=\\\\='(floot(1),5).",
    Err(Error::TypeError(structure!("floot", Number(1))));
    "iso - arith diff 5"
)]
#[test_case(
    "",
    "?- '=:='(0,1).",
    Ok(vec![]);
    "iso - arith eq 1"
)]
#[test_case(
    "",
    "?- '=:='(3 * 2,7 - 1).",
    Ok(vec![
         Vars::default(),
    ]);
    "iso - arith eq 3"
)]
#[test_case(
    "",
    "?- '=:='(N,5).",
    Err(Error::UnsetVar("N".to_string()));
    "iso - arith eq 4"
)]
#[test_case(
    "",
    "?- '>'(0,1).",
    Ok(vec![]);
    "iso - arith gt 1"
)]
#[test_case(
    "",
    "?- '>'(3 * 2,7 - 1).",
    Ok(vec![]);
    "iso - arith gt 3"
)]
#[test_case(
    "",
    "?- '>'(N,5).",
    Err(Error::UnsetVar("N".to_string()));
    "iso - arith gt 4"
)]
#[test_case(
    "",
    "?- '>'(2 + floot(1),5).",
    Err(Error::TypeError(structure!("floot", Number(1))));
    "iso - arith gt 5"
)]
#[test_case(
    "",
    "?- '>='(0,1).",
    Ok(vec![]);
    "iso - arith gte 1"
)]
#[test_case(
    "",
    "?- '>='(3 * 2,7 - 1).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - arith gte 3"
)]
#[test_case(
    "",
    "?- '>='(N,5).",
    Err(Error::UnsetVar("N".to_string()));
    "iso - arith gte 4"
)]
#[test_case(
    "",
    "?- '>='(2 + floot(1),5).",
    Err(Error::TypeError(structure!("floot", Number(1))));
    "iso - arith gte 5"
)]
#[test_case(
    "",
    "?- '<'(0,1).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - arith lt 1"
)]
#[test_case(
    "",
    "?- '<'(3 * 2,7 - 1).",
    Ok(vec![]);
    "iso - arith lt 3"
)]
#[test_case(
    "",
    "?- '<'(N,5).",
    Err(Error::UnsetVar("N".to_string()));
    "iso - arith lt 4"
)]
#[test_case(
    "",
    "?- '<'(2 + floot(1),5).",
    Err(Error::TypeError(structure!("floot", Number(1))));
    "iso - arith lt 5"
)]
#[test_case(
    "",
    "?- '=<'(0,1).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - arith lte 1"
)]
#[test_case(
    "",
    "?- '=<'(3 * 2,7 - 1).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - arith lte 3"
)]
#[test_case(
    "",
    "?- '=<'(N,5).",
    Err(Error::UnsetVar("N".to_string()));
    "iso - arith lte 4"
)]
#[test_case(
    "",
    "?- '=<'(2 + floot(1),5).",
    Err(Error::TypeError(structure!("floot", Number(1))));
    "iso - arith lte 5"
)]
#[test_case(
    "",
    "?- atom(atom).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - atom 1"
)]
#[test_case(
    "",
    "?- atom('string').",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - atom 2"
)]
#[test_case(
    "",
    "?- atom(a(b)).",
    Ok(vec![]);
    "iso - atom 3"
)]
#[test_case(
    "",
    "?- atom(Var).",
    Ok(vec![]);
    "iso - atom 4"
)]
#[test_case(
    "",
    "?- atom([]).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - atom 5"
)]
#[test_case(
    "",
    "?- atom(6).",
    Ok(vec![]);
    "iso - atom 6"
)]
#[test_case(
    "",
    "?- integer(3).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - integer 1"
)]
#[test_case(
    "",
    "?- integer(-3).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - integer 2"
)]
#[test_case(
    "",
    "?- integer(X).",
    Ok(vec![]);
    "iso - integer 3"
)]
#[test_case(
    "",
    "?- integer(atom).",
    Ok(vec![]);
    "iso - integer 4"
)]
#[test_case(
    "",
    "?- number(3).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - number 1"
)]
#[test_case(
    "",
    "?- number(-3).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - number 2"
)]
#[test_case(
    "",
    "?- number(X).",
    Ok(vec![]);
    "iso - number 3"
)]
#[test_case(
    "",
    "?- number(atom).",
    Ok(vec![]);
    "iso - number 4"
)]
#[test_case(
    "",
    "?- nonvar(foo).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - nonvar 1"
)]
#[test_case(
    "",
    "?- nonvar(Foo).",
    Ok(vec![]);
    "iso - nonvar 2"
)]
#[test_case(
    "",
    "?- foo=Foo,nonvar(Foo).",
    Ok(vec![
        Vars::from([
            (init_var!("Foo"), atom!("foo")),
        ]),
    ]);
    "iso - nonvar 3"
)]
#[test_case(
    "",
    "?- nonvar(_).",
    Ok(vec![]);
    "iso - nonvar 4"
)]
#[test_case(
    "",
    "?- nonvar(a(b)).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - nonvar 5"
)]
#[test_case(
    "",
    "?- true.",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - true"
)]
#[test_case(
    "",
    "?- fail.",
    Ok(vec![]);
    "iso - fail"
)]
#[test_case(
    "",
    "?- (repeat,!,fail).",
    Ok(vec![]);
    "iso - repeat"
)]
#[test_case(
    "",
    "?- !.",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - cut 1"
)]
#[test_case(
    "",
    "?- ((!,fail);true).", // added brackets
    Ok(vec![]);
    "iso - cut 2"
)]
#[test_case(
    "",
    "?- (call(!),fail;true).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - cut 3"
)]
#[test_case(
    "",
    "?- call(!).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - call 1"
)]
#[test_case(
    "",
    "?- call(fail).",
    Ok(vec![]);
    "iso - call 2"
)]
#[test_case(
    "",
    "?- call((fail, X)).",
    Ok(vec![]);
    "iso - call 3"
)]
#[test_case(
    "",
    "?- call((fail, call(1))).",
    Ok(vec![]);
    "iso - call 4"
)]
#[test_case(
    "",
    "?- call((write(3), X)).",
    Err(Error::UnsetVar("X".to_string()));
    "iso - call 5"
)]
#[test_case(
    "",
    "?- call((write(3), call(1))).",
    Err(Error::NotCallable(Number(1)));
    "iso - call 6"
)]
#[test_case(
    "",
    "?- call(X).",
    Err(Error::UnsetVar("X".to_string()));
    "iso - call 7"
)]
#[test_case(
    "",
    "?- call(1).",
    Err(Error::NotCallable(Number(1)));
    "iso - call 8"
)]
#[test_case(
    "",
    "?- call((fail, 1)).",
    Err(Error::NotCallable(Number(1)));
    "iso - call 9"
)]
#[test_case(
    "",
    "?- call((write(3), 1)).",
    Err(Error::NotCallable(Number(1)));
    "iso - call 10"
)]
#[test_case(
    "",
    "?- call((1; true)).",
    Err(Error::NotCallable(Number(1)));
    "iso - call 11"
)]
#[test_case(
    "",
    "?- 'is'(Result,3 + 11).", // using int instead of float
    Ok(vec![
        Vars::from([
            (init_var!("Result"), Number(14)),
        ])
    ]);
    "iso - is 1"
)]
#[test_case(
    "",
    "?- X = 1 + 2, 'is'(Y, X * 3).",
    Ok(vec![
        Vars::from([
            (init_var!("X"), structure!("+", Number(1), Number(2))),
            (init_var!("Y"), Number(9)),
        ])
    ]);
    "iso - is 2"
)]
#[test_case(
    "",
    "?- 'is'(foo,77).",
    Err(Error::TypeError(atom!("foo")));
    "iso - is 3"
)]
#[test_case(
    "",
    "?- 'is'(77, N).",
    Err(Error::UnsetVar("N".to_string()));
    "iso - is 4"
)]
#[test_case(
    "",
    "?- 'is'(77, foo).",
    Err(Error::ArithError(atom!("foo")));
    "iso - is 5"
)]
#[test_case(
    "",
    "?- '\\\\=='(1,1).",
    Ok(vec![]);
    "iso - term diff 1"
)]
#[test_case(
    "",
    "?- '\\\\=='(X,X).",
    Ok(vec![]);
    "iso - term diff 2"
)]
#[test_case(
    "",
    "?- '\\\\=='(1,2).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - term diff 3"
)]
#[test_case(
    "",
    "?- '\\\\=='(X,1).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - term diff 4"
)]
#[test_case(
    "",
    "?- '\\\\=='(X,Y).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - term diff 5"
)]
#[test_case(
    "",
    "?- '\\\\=='(_,_).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - term diff 6"
)]
#[test_case(
    "",
    "?- '\\\\=='(X,a(X)).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - term diff 7"
)]
#[test_case(
    "",
    "?- '\\\\=='(f(a),f(a)).",
    Ok(vec![]);
    "iso - term diff 8"
)]
#[test_case(
    "",
    "?- '=='(1,1).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - term eq 1"
)]
#[test_case(
    "",
    "?- '=='(X,X).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - term eq 2"
)]
#[test_case(
    "",
    "?- '=='(1,2).",
    Ok(vec![]);
    "iso - term eq 3"
)]
#[test_case(
    "",
    "?- '=='(X,1).",
    Ok(vec![]);
    "iso - term eq 4"
)]
#[test_case(
    "",
    "?- '=='(X,Y).",
    Ok(vec![]);
    "iso - term eq 5"
)]
#[test_case(
    "",
    "?- '=='(_,_).",
    Ok(vec![
]);
    "iso - term eq 6"
)]
#[test_case(
    "",
    "?- '=='(X,a(X)).",
    Ok(vec![]);
    "iso - term eq 7"
)]
#[test_case(
    "",
    "?- '=='(f(a),f(a)).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - term eq 8"
)]
#[test_case(
    "",
    "?- '@<'(aardvark,zebra).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - term lt 1"
)]
#[test_case(
    "",
    "?- '@<'(short,short).",
    Ok(vec![]);
    "iso - term lt 2"
)]
#[test_case(
    "",
    "?- '@<'(short,shorter).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - term lt 3"
)]
#[test_case(
    "",
    "?- '@<'(foo(b),foo(a)).",
    Ok(vec![]);
    "iso - term lt 4"
)]
#[test_case(
    "",
    "?- '@<'(X,X).",
    Ok(vec![]);
    "iso - term lt 5"
)]
#[test_case(
    "",
    "?- '@<'(foo(a,X),foo(b,Y)).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - term lt 6"
)]
#[test_case(
    "",
    "?- '@=<'(aardvark,zebra).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - term lte 1"
)]
#[test_case(
    "",
    "?- '@=<'(short,short).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - term lte 2"
)]
#[test_case(
    "",
    "?- '@=<'(short,shorter).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - term lte 3"
)]
#[test_case(
    "",
    "?- '@=<'(foo(b),foo(a)).",
    Ok(vec![]);
    "iso - term lte 4"
)]
#[test_case(
    "",
    "?- '@=<'(X,X).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - term lte 5"
)]
#[test_case(
    "",
    "?- '@=<'(foo(a,X),foo(b,Y)).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - term lte 6"
)]
#[test_case(
    "",
    "?- '@>'(aardvark,zebra).",
    Ok(vec![]);
    "iso - term gt 1"
)]
#[test_case(
    "",
    "?- '@>'(short,short).",
    Ok(vec![]);
    "iso - term gt 2"
)]
#[test_case(
    "",
    "?- '@>'(short,shorter).",
    Ok(vec![]);
    "iso - term gt 3"
)]
#[test_case(
    "",
    "?- '@>'(foo(b),foo(a)).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - term gt 4"
)]
#[test_case(
    "",
    "?- '@>'(X,X).",
    Ok(vec![]);
    "iso - term gt 5"
)]
#[test_case(
    "",
    "?- '@>'(foo(a,X),foo(b,Y)).",
    Ok(vec![]);
    "iso - term gt 6"
)]
#[test_case(
    "",
    "?- '@>='(aardvark,zebra).",
    Ok(vec![]);
    "iso - term gte 1"
)]
#[test_case(
    "",
    "?- '@>='(short,short).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - term gte 2"
)]
#[test_case(
    "",
    "?- '@>='(short,shorter).",
    Ok(vec![]);
    "iso - term gte 3"
)]
#[test_case(
    "",
    "?- '@>='(foo(b),foo(a)).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - term gte 4"
)]
#[test_case(
    "",
    "?- '@>='(X,X).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - term gte 5"
)]
#[test_case(
    "",
    "?- '@>='(foo(a,X),foo(b,Y)).",
    Ok(vec![]);
    "iso - term gte 6"
)]
#[test_case(
    "",
    "?- ';'(true, fail).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - or 1"
)]
#[test_case(
    "",
    "?- ';'((!, fail), true).",
    Ok(vec![]);
    "iso - or 2"
)]
#[test_case(
    "",
    "?- ';'(!, call(3)).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - or 3"
)]
#[test_case(
    "",
    "?- ';'((X=1, !), X=2).",
    Ok(vec![
        Vars::from([
            (init_var!("X"), Number(1)),
        ]),
    ]);
    "iso - or 4"
)]
#[test_case(
    "",
    "?- ';'(X=1, X=2).",
    Ok(vec![
        Vars::from([
            (init_var!("X"), Number(1)),
        ]),
        Vars::from([
            (init_var!("X"), Number(2)),
        ]),
    ]);
    "iso - or 5"
)]
#[test_case(
    "",
    "?- '->'(true, true).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - if-then 1"
)]
#[test_case(
    "",
    "?- '->'(true, fail).",
    Ok(vec![]);
    "iso - if-then 2"
)]
#[test_case(
    "",
    "?- '->'(fail, true).",
    Ok(vec![]);
    "iso - if-then 3"
)]
#[test_case(
    "",
    "?- '->'(true, X=1).",
    Ok(vec![
        Vars::from([
            (init_var!("X"), Number(1)),
        ])
    ]);
    "iso - if-then 4"
)]
#[test_case(
    "",
    "?- '->'(';'(X=1, X=2), true).",
    Ok(vec![
        Vars::from([
            (init_var!("X"), Number(1)),
        ])
    ]);
    "iso - if-then 5"
)]
#[test_case(
    "",
    "?- '->'(true, ';'(X=1, X=2)).",
    Ok(vec![
        Vars::from([
            (init_var!("X"), Number(1)),
        ]),
        Vars::from([
            (init_var!("X"), Number(2)),
        ])
    ]);
    "iso - if-then 6"
)]
#[test_case(
    "",
    "?- ';'('->'(true, true), fail).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - if-then-else 1"
)]
#[test_case(
    "",
    "?- ';'('->'(fail, true), true).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - if-then-else 2"
)]
#[test_case(
    "",
    "?- ';'('->'(true, fail), fail).",
    Ok(vec![]);
    "iso - if-then-else 3"
)]
#[test_case(
    "",
    "?- ';'('->'(fail, true), fail).",
    Ok(vec![]);
    "iso - if-then-else 4"
)]
#[test_case(
    "",
    "?- ';'('->'(true, X=1), X=2).",
    Ok(vec![
        Vars::from([
            (init_var!("X"), Number(1)),
        ])
    ]);
    "iso - if-then-else 5"
)]
#[test_case(
    "",
    "?- ';'('->'(fail, X=1), X=2).",
    Ok(vec![
        Vars::from([
            (init_var!("X"), Number(2)),
        ])
    ]);
    "iso - if-then-else 6"
)]
#[test_case(
    "",
    "?- ';'('->'(true, ';'(X=1, X=2)), true).",
    Ok(vec![
        Vars::from([
            (init_var!("X"), Number(1)),
        ]),
        Vars::from([
            (init_var!("X"), Number(2)),
        ]),
    ]);
    "iso - if-then-else 7"
)]
#[test_case(
    "",
    "?- ';'('->'(';'(X=1, X=2), true), true).",
    Ok(vec![
        Vars::from([
            (init_var!("X"), Number(1)),
        ])
    ]);
    "iso - if-then-else 8"
)]
#[test_case(
    "",
    "?- once(!).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - once 1"
)]
#[test_case(
    "",
    "?- once(!), (X=1; X=2).",
    Ok(vec![
        Vars::from([
            (init_var!("X"), Number(1)),
        ]),
                Vars::from([
            (init_var!("X"), Number(2)),
        ]),
    ]);
    "iso - once 2"
)]
#[test_case(
    "",
    "?- once(repeat).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - once 3"
)]
#[test_case(
    "",
    "?- once(fail).",
    Ok(vec![]);
    "iso - once 4"
)]
#[test_case(
    "",
    "?- once(3).",
    Err(Error::NotCallable(Number(3)));
    "iso - once 5"
)]
#[test_case(
    "",
    "?- once(X).",
    Err(Error::UnsetVar("X".to_string()));
    "iso - once 6"
)]
#[test_case(
    "",
    "?- \\+(true).",
    Ok(vec![]);
    "iso - not provable 1"
)]
#[test_case(
    "",
    "?- \\+(!).",
    Ok(vec![]);
    "iso - not provable 2"
)]
#[test_case(
    "",
    "?- \\+((!,fail)).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - not provable 3"
)]
#[test_case(
    "",
    "?- (X=1;X=2), \\+((!,fail)).",
    Ok(vec![
        Vars::from([
            (init_var!("X"), Number(1)),
        ]),
        Vars::from([
            (init_var!("X"), Number(2)),
        ]),
    ]);
    "iso - not provable 4"
)]
#[test_case(
    "",
    "?- \\+(4 = 5).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - not provable 5"
)]
// TODO: this test passes, but we check for errors at execution, not init.
// #[test_case(
//     "",
//     "?- \\+(3).",
//     Err(Error::NotCallable(Number(3)));
//     "iso - not provable 6"
// )]
#[test_case(
    "",
    "?- \\+(X).",
    Err(Error::UnsetVar("X".to_string()));
    "iso - not provable 7"
)]
#[test_case(
    "",
    "?- '='(1,1).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - unify 1"
)]
#[test_case(
    "",
    "?- '='(X,1).",
    Ok(vec![
        Vars::from([
            (init_var!("X"), Number(1)),
        ]),
    ]);
    "iso - unify 2"
)]
#[test_case(
    "",
    "?- '='(X,Y).",
    Ok(vec![
        Vars::from([
            (init_var!("X"), init_var!("Y")),
        ]),
    ]);
    "iso - unify 3"
)]
#[test_case(
    "",
    "?- '='(X,Y),'='(X,abc).",
    Ok(vec![
        Vars::from([
            (init_var!("X"), init_var!("Y")),
            (init_var!("Y"), atom!("abc")),
        ]),
    ]);
    "iso - unify 4"
)]
#[test_case(
    "",
    "?- '='(f(X,def),f(def,Y)).",
    Ok(vec![
        Vars::from([
            (init_var!("X"), atom!("def")),
            (init_var!("Y"), atom!("def")),
        ]),
    ]);
    "iso - unify 5"
)]
#[test_case(
    "",
    "?- '='(1,2).",
    Ok(vec![]);
    "iso - unify 6"
)]
#[test_case(
    "",
    "?- '='(g(X),f(f(X))).",
    Ok(vec![]);
    "iso - unify 7"
)]
#[test_case(
    "",
    "?- '='(f(X,1),f(a(X))).",
    Ok(vec![]);
    "iso - unify 8"
)]
#[test_case(
    "",
    "?- '='(f(X,Y,X),f(a(X),a(Y),Y,2)).",
    Ok(vec![]);
    "iso - unify 9"
)]
#[test_case(
    "",
    "?- '='(f(A,B,C),f(g(B,B),g(C,C),g(D,D))).",
    Ok(vec![
        Vars::from([
            (init_var!("A"), structure!("g",
                                structure!("g",
                                    structure!("g", init_var!("D"), init_var!("D")),
                                    structure!("g", init_var!("D"), init_var!("D"))),
                                structure!("g",
                                    structure!("g", init_var!("D"), init_var!("D")),
                                    structure!("g", init_var!("D"), init_var!("D"))))),
            (init_var!("B"), structure!("g",
                                structure!("g", init_var!("D"), init_var!("D")),
                                structure!("g", init_var!("D"), init_var!("D")))),
            (init_var!("C"), structure!("g", init_var!("D"), init_var!("D"))),
        ])
    ]);
    "iso - unify 10"
)]
#[test_case(
    "",
    "?- '\\\\='(1,1).",
    Ok(vec![]);
    "iso - not unify 1"
)]
#[test_case(
    "",
    "?- '\\\\='(X,1).",
    Ok(vec![]);
    "iso - not unify 2"
)]
#[test_case(
    "",
    "?- '\\\\='(X,Y).",
    Ok(vec![]);
    "iso - not unify 3"
)]
#[test_case(
    "",
    "?- '\\\\='(X,Y),'\\\\='(X,abc).",
    Ok(vec![]);
    "iso - not unify 4"
)]
#[test_case(
    "",
    "?- '\\\\='(f(X,def),f(def,Y)).",
    Ok(vec![]);
    "iso - not unify 5"
)]
#[test_case(
    "",
    "?- '\\\\='(1,2).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - not unify 6"
)]
#[test_case(
    "",
    "?- '\\\\='(g(X),f(f(X))).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - not unify 7"
)]
#[test_case(
    "",
    "?- '\\\\='(f(X,1),f(a(X))).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - not unify 8"
)]
#[test_case(
    "",
    "?- '\\\\='(f(X,Y,X),f(a(X),a(Y),Y,2)).",
    Ok(vec![
        Vars::default(),
    ]);
    "iso - not unify 9"
)]
fn all_solutions(knowledge: &str, query: &str, expected: Result<Vec<Vars>, Error>) {
    let mut db = Database::new();

    let mut reader = StringReader::from(knowledge);
    let mut lex = Lexer::from(&mut reader);

    unsafe {
        super::TRACE = true;
    }

    // load standard library
    eval_file("lib/stdlib.pl", db.clone()).unwrap();

    loop {
        match parser::next(&mut lex) {
            Err(parser::ParsingError::EndOfInput) => break,
            Err(msg) => panic!("{}", msg),
            Ok(ref expr) => match expr {
                Question(_) => unreachable!(),
                term => db.assert(term).unwrap(),
            },
        }
    }

    let mut reader = StringReader::from(query);
    let mut lex = Lexer::from(&mut reader);
    let solver = match parser::next(&mut lex).unwrap() {
        Question(ref goals) => ByrdBox::from(goals, db.clone()).unwrap(),
        _ => unreachable!(),
    };
    let result: Result<Vec<Vars>, Error> = solver.iter().collect();
    assert_eq!(result, expected);
}

#[test_case("examples/99-prolog-problems/p1_lists.pl")]
#[test_case("examples/99-prolog-problems/p2_arithmetic.pl")]
#[test_case("examples/99-prolog-problems/p3_logic_and_codes.pl")]
#[test_case("examples/99-prolog-problems/p4_binary_trees.pl")]
#[test_case("examples/99-prolog-problems/p5_multiway_trees.pl")]
fn integration_test(path: &str) {
    let db = Database::new();
    // load standard library
    eval_file("lib/stdlib.pl", db.clone()).unwrap();
    if let Err(err) = eval_file(path, db) {
        panic!("Unexpected error: {}", err)
    }
}

#[test]
fn queens_test() {
    let db = Database::new();
    // load standard library
    eval_file("lib/stdlib.pl", db.clone()).unwrap();
    eval_file("examples/eight_queens.pl", db.clone()).unwrap();

    // using 6 queens to make it faster
    let solver = eval_expr(
        &Term::Question(vec![structure!("queens", Number(6), init_var!("Qs"))]),
        db,
    )
    .unwrap();

    let result = solver.unwrap().collect::<Result<Vec<_>, Error>>().unwrap();
    let expected = vec![
        Vars::from([(
            init_var!("Qs"),
            parser::make_list(
                &[
                    Number(5),
                    Number(3),
                    Number(1),
                    Number(6),
                    Number(4),
                    Number(2),
                ],
                Nil,
            ),
        )]),
        Vars::from([(
            init_var!("Qs"),
            parser::make_list(
                &[
                    Number(4),
                    Number(1),
                    Number(5),
                    Number(2),
                    Number(6),
                    Number(3),
                ],
                Nil,
            ),
        )]),
        Vars::from([(
            init_var!("Qs"),
            parser::make_list(
                &[
                    Number(3),
                    Number(6),
                    Number(2),
                    Number(5),
                    Number(1),
                    Number(4),
                ],
                Nil,
            ),
        )]),
        Vars::from([(
            init_var!("Qs"),
            parser::make_list(
                &[
                    Number(2),
                    Number(4),
                    Number(6),
                    Number(1),
                    Number(3),
                    Number(5),
                ],
                Nil,
            ),
        )]),
    ];
    assert_eq!(result, expected)
}
