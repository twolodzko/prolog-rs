
person(socrates).
person(plato).
person(zeno).
person(aristotle).

mortal(Who) :-
    person(Who).

?- mortal(socrates).
?- \+ mortal(zeus).

% this would print all by backtracking
?- mortal(X),
    writeln(X),
    fail.
