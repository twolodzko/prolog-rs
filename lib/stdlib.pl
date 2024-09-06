
true.
false :- fail.

\=(X, Y) :- \+ (X = Y).
=\=(X, Y) :- \+ (X =:= Y).
\==(A, B) :- \+ (A == B).

>(A, B) :- B < A.
=<(A, B) :- \+ (B < A).
>=(A, B) :- \+ (A < B).

@>(A, B) :- B @< A.
@=<(A, B) :- \+ (B @< A).
@>=(A, B) :- \+ (A @< B).

forall(Cond, Action) :- \+ (Cond, \+ Action).

is_list(X) :- var(X), !, fail.
is_list([]).
is_list([_|T]) :- is_list(T).

append([], X, X).
append([A|B], C, [A|D]) :- append(B, C, D).

member(X, [X|_]).
member(X, [_|Xs]) :- member(X, Xs).

memberchk(Elem, List) :- once(member(Elem, List)).

length([], 0).
length([_|T], L) :- length(T, L_), L is L_ + 1.

reverse(X, R) :- reverse(X, R, []).
reverse([], R, R).
reverse([X|Xs], R, Acc) :- reverse(Xs, R, [X|Acc]).

nonvar(X) :- \+ var(X).

atomic(X) :- atom(X).
atomic(X) :- number(X).

compound(X) :- \+ atomic(X).

writeln(X) :- write(X), nl.
display(X) :- writeln(X).

call(X) :- X.

once(X) :- X, !.

ignore(X) :- X, !.
ignore(_).

repeat.
repeat :- repeat.
