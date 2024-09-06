
p(a).
p(b).
q(c).
q(d).

% this should run only the first p example, fail, and jump to q examples
?- trace.
?- (\+ (p(X), ! )) ; q(X).

% this should run only the first p example, succeed, and jump to q examples
once(X) :- X, !.
?- once(p(X)) ; q(X).
