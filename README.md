# Prolog.rs

This is a minimal Prolog interpreter implemented in Rust.
The implementation covers only a subset of Prolog features,
is not intended to be fast, or optimal in any sense.
It is a learning project that helped me to understand Prolog better.

The implementation is tested using 350+ unit tests, including
running some code solutions for the ["99 Prolog problems"],
and unit tests checking for ISO Prolog consistency. There are
some differences from other Prolog implementations though,
as described [below](#limitations-and-differences-from-other-implementations).

## Usage

To run the Prolog interpreter you can use the [Justfile] commands:

```shell
$ just test      # runs tests
$ just build     # builds the standalone binary ./prolog
$ just run FILE  # evaluate the FILE and start REPL
$ just repl      # start REPL
```

To install it, move the `prolog` binary together with the `lib/` directory to some
directory of your choice.

## Data types

The units of data in Prolog are called *terms*.

* Atoms are name-only datatypes, for example, `foo`.
  Their names need to start with a lowercase letter.
* Integers are the only supported numerical type.
* Variables have no specific value, but they can be initialized
  with a value during unification (see below). Their names
  need to start with uppercase letters or `_`.
* There exist also compound terms:
  * Structures like `foo(a, b)` have name `foo` and
    arguments (`c` and `b`).
  * Lists like `[1, 2, 3]` can contain multiple terms.

In Prolog everything is a struct, so atom `foo` is the same as `foo()`, operation like
`2 + 2` is `+(2, 2)`, the "and" operator in `a , b` is `,(a, b)`
(don't be confused with the comma separating the arguments), etc.

Lists in Prolog are also structs, so `[1, 2, 3]` is represented as `.(1, .(2, .(3, [])))`, where
`[]` stands for an empty list. Prolog allows you to create dotted pairs (in [lisp] terms), for example
`[1 | 2]` is represented as `.(1, 2)`.

There are no booleans. Terms are evaluated by unification. The term `fail` always fails the unification.

```prolog
?- 1=1.
yes
?- \+ 1=1.
no
?- fail.
no
?- \+ fail.
yes
```

## Facts, rules, and questions

Prolog programs are defined in terms of three kinds of expressions:

* Facts, like `foo.` or `bar(a,b,32).` state what is "true".
* Rules, like `mortal(Who) :- person(Who).` define logical implications.
* Questions, like `?- mortal(socrates).` validate if the question is true.

A very simple Prolog program may solve the classical logical question:

```prolog
% fact
person(socrates).

% rule
mortal(Who) :-
    person(Who).

% question
?- mortal(socrates).
```

## Unification

Prolog extensively uses pattern matching. When you ask a *question*, it searches its
database if any of the recorded *facts* and *rules* that match the goals in the question.
This is nicely explained in the *[Adventure in Prolog]* book by Dennis Merritt:

> Prolog's pattern matching is called **unification**. In the case where the logicbase
> contains only facts, unification succeeds if the following three conditions hold.
>
> * The predicate named in the goal and logicbase are the same.
> * Both predicates have the same arity.
> * All of the arguments are the same.

When variable is unified with the value, the variable becomes equivalent to the value.
If two free variables are unified, they become each other's aliases.

There are also procedures using special evaluation rules instead of unification, for example:

```prolog
?- X is 1+2.
X = 3
?- 1+2 < 5.
yes
?- writeln('hello, world!').
hello, world!
yes
```

## Features

By design, this interpreter covers only a subset of Prolog's features. Those include:

* `fail` is a goal that always fails.
* `a , b` means that we want to satisfy both *a* and *b*, while `a ; b` means *a* or *b*.
* `\+` can be used to negate a goal.
* `!` is the [cut operator]. It prevents backtracking for the goals preceding it.
* `->` is the if-else operator, `Cond -> Then` tries to satisfy `Cond`, if it succeeds,
  then attempts to satisfy `Then`, otherwise it fails. Underneath, it is a syntactic
  sugar for expressing `Cond, !, Then`.
* `=` is a unification operator, it is equivalent to `=(A, A)`.
* `_` is a wildcard variable that unifies with anything but never holds any value.
* `is` operator, as in `X is 2 + 2`, evaluates the right-hand-side and if left-hand-side is
  a free variable, assigns the result to it, otherwise compares the result to it's value
  (so `4 is 2 + 2` evaluates to "yes").
* The supported mathematical operators and functions are:
  * unary operators `+`, `-`,
  * binary operators `+`, `-`, `*`, `/`, `//` (last two are synonyms), and `rem`,
  * `div` and `mod` (using [`i32::div_euclid`][i32] and [`i32::rem_euclid`][i32]),
  * `abs` and `sign` functions.
  
  Those operators can be used together with procedures with special evaluation rules
  like `is`, `=:=`, `<`, etc. Outside of those procedures, they will create structs,
  for example `2 + 3` would become `+(2,3)`.
* `=:=`, `<` are the numerical comparison operators that evaluate both sides
  and compare them, e.g. `2 + 1 < 7 - 2`.
  The operators `=\=`, `=<`, `>`, `>=` are available through `lib/stdlib.pl`.
* `==`, `@<` are comparison operators checking the [standard order of terms]
  (see [below](#limitations-and-differences-from-other-implementations)).
  The operators `\==`, `@=<`, `@>`, `@>=` are available through `lib/stdlib.pl`.
* `consult('path/to/file.pl')` loads and evaluates the file. If the file contains a question (`?-`)
  which cannot be satisfied, it will fail with an error. It takes as an argument an atom with
  path to the file, or a list of such atoms.
* `write(X)` prints *X* and `nl` prints a newline.
* The `trace` and `untrace` commands can be used to turn the tracing logging on and off.
* `{a, b, c}` is a syntactic sugar for writing `{}(,(a, ,(b, c)))`. It has no special meaning.
* `atom(X)`, `integer(X)`, `number(X)` are type checkers. `var(X)` checks if *X* is a free variable.

More functionalities are implemented in the standard library available through `lib/stdlib.pl`.

## Limitations and differences from other implementations

* Only a subset of Prolog's functionalities are implemented. Features such as strings or floats types, DCG's, defining
  custom operators, etc are not available.
* The precedence of `;` and `,` operators is reversed, so `a , b ; c, d` is parsed the same as `a , (b ; c) , d`.
  Use brackets to assure the correct precedence.
* In quoted atom names only a subset of escape characters are allowed, including: `\n`, `\t`, `\s`, `\\`, `\'`. `\"`,
  or `\NEWLINE`.
* Under the [standard order of terms] variables should be sorted by their memory addresses. Since in this
  implementation variables don't get memory addresses until they are initialized, such ordering is not possible.
* In Prolog `,/2` is an operator such that `a , b` tries to satisfy `a` and `b`. In this implementation instead
  of using linked lists, structs are based on Rust `Vec`'s of any size, so `a , b , c` would become `,(a, b, c)`
  rather than `,(a, ,(b, c))`.
* Arithmetic `div` and `mod` use [Rust's `i32::div_euclid` and `i32::rem_euclid`][i32] which are defined
  [differently to Prolog][swipl-div], but meet the requirement of being consistent with each other.
* The implementation is not tail-call optimized, so can easily overflow when satisfying complex goals.
* Tracing is simplified and limited as compared to the other implementations.


[antlr]: https://github.com/antlr/grammars-v4/blob/master/prolog/prolog.g4
[Adventure in Prolog]: https://www.amzi.com/AdventureInProlog/index.php
[standard order of terms]: https://www.swi-prolog.org/pldoc/man?section=standardorder
[cut operator]: https://pages.cs.wisc.edu/~fischer/cs538.s02/prolog/A13CUT.HTM
[i32]: https://doc.rust-lang.org/std/primitive.i32.html
[lisp]: https://web.mit.edu/scheme_v9.2/doc/mit-scheme-ref/Lists.html#Lists
["99 Prolog problems"]: https://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/
[swipl-div]: https://www.swi-prolog.org/pldoc/man?function=div%2f2
[Justfile]: https://github.com/casey/just
