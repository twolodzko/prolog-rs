whole_number(0).
whole_number(X) :- whole_number(Y), X is Y + 1.

% For an infinite loop call:
% ?- whole_number(X).
