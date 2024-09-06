% 1.01 (*) Find the last element of a list.

% Example:
% ?- my_last(X, [a, b, c, d]).
% X = d

my_last(X, [X]).
my_last(X, [_|T]) :- my_last(X, T).

?- my_last(X, [a, b, c, d]), nonvar(X), X = d, writeln(ok).


% 1.02 (*) Find the last but one element of a list.

% Example:
% ?- last_but_one(X, [a, b, c, d]).
% X = c

last_but_one(X, [X,_]).
last_but_one(X, [_|T]) :- last_but_one(X, T).

?- last_but_one(X, [a, b, c, d]), nonvar(X), X = c, writeln(ok).


% 1.03 (*) Find the Kth element of a list.
% The first element in the list is number 1.

% Example:
% ?- element_at(X, [a, b, c, d], 3).
% X = c

element_at(X, [X|_], 1).
element_at(X, [_|T], K) :- K > 1, K_ is K-1, element_at(X, T, K_).

?- element_at(X, [a, b, c, d], 3), nonvar(X), X = c, writeln(ok).


% 1.04 (*) Find the number of elements of a list.

% Example:
% ?- my_length(X, [a, b, c]).
% X = 3

my_length([], 0).
my_length([_|T], L) :- my_length(T, L_), L is L_ + 1.

?- my_length([a, b, c], X), nonvar(X), X = 3, writeln(ok).


% 1.05 (*) Reverse a list.

% Example:
% ?- reverse([a, b, c, d], X).
% X = [d, c, b, a]

my_reverse(X, R) :- my_reverse_(X, R, []).

my_reverse_([], R, R).
my_reverse_([X|Xs], R, Acc) :- my_reverse_(Xs, R, [X|Acc]).

?- my_reverse([a, b, c, d], X), nonvar(X), X = [d, c, b, a], writeln(ok).


% 1.06 (*) Find out whether a list is a palindrome.

% Example:
% ?- is_palindrome([x, a, m, a, x]).
% true.

is_palindrome(X) :- my_reverse(X, X).

?- is_palindrome([x, a, m, a, x]), writeln(ok).


% 1.07 (**) Flatten a nested list structure.

% Example:
% ?- my_flatten([a, [b, [c, d], e]], X).
% X = [a, b, c, d, e]

my_flatten(X, [X]) :- \+ is_list(X).
my_flatten([], []).
my_flatten([X|Xs], Y) :-
  my_flatten(X, X_), my_flatten(Xs, Xs_), append(X_, Xs_, Y).

?- my_flatten([a, [b, [c, d], e]], X), nonvar(X), X = [a, b, c, d, e], writeln(ok).


% 1.08 (**) Eliminate consecutive duplicates of list elements.

% Example:
% ?- compress([a, a, a, a, b, c, c, a, a, d, e, e, e, e], X).
% X = [a, b, c, d, e]

compress([], []).
compress([X], [X]).
compress([X,X|Xs], Y) :- compress([X|Xs], Y).
compress([X,Z|Xs], [X|Y]) :- X \= Z, compress([Z|Xs], Y).

?- compress([a, a, a, a, b, c, c, d, e, e, e, e], X),
  nonvar(X),
  X = [a, b, c, d, e],
  writeln(ok).


% 1.09 (**) Pack consecutive duplicates of list elements into sublists.

% Example:
% ?- pack([a, a, a, a, b, c, c, a, a, d, e, e, e, e], X).
% X = [[a, a, a, a], [b], [c, c], [a, a], [d], [e, e, e, e]]

pack([], []).
pack([X], [[X]]).
pack([X|Xs], [[X,X|P]|T]) :- pack(Xs, [[X|P]|T]).
pack([X|Xs], [[X],[Y|P]|T]) :- pack(Xs, [[Y|P]|T]), X \= Y.

?- pack([a, a, a, a, b, c, c, a, a, d, e, e, e, e], X),
  nonvar(X),
  X = [[a, a, a, a], [b], [c, c], [a, a], [d], [e, e, e, e]],
  writeln(ok).


% 1.10 (*) Run-length encoding of a list.

% Example:
% ?- encode([a, a, a, a, b, c, c, a, a, d, e, e, e, e], X).
% X = [[4, a], [1, b], [2, c], [2, a], [1, d], [4, e]]

encode([], []).
encode([X], [[1, X]]).
encode([X|Xs], [[N_, X]|E]) :- encode(Xs, [[N, X]|E]), N_ is N + 1.
encode([X|Xs], [[1, X], [N, Y]|E]) :- encode(Xs, [[N, Y]|E]), X \= Y.

?- encode([a, a, a, a, b, c, c, a, a, d, e, e, e, e], X),
  nonvar(X),
  X = [[4, a], [1, b], [2, c], [2, a], [1, d], [4, e]],
  writeln(ok).


% 1.11 (*) Modified run-length encoding.
% Modify the result of problem 1.10 in such a way that if an element has no
% duplicates it is simply copied into the result list.

% Example:
% ?- encode_modified([a, a, a, a, b, c, c, a, a, d, e, e, e, e], X).
% X = [[4, a], b, [2, c], [2, a], d, [4, e]]

encode_modified(X, Y) :- encode(X, Z), modify(Z, Y).

modify([], []).
modify([[1,X]|Xs], [X|Ys]) :- modify(Xs, Ys).
modify([[N,X]|Xs], [[N,X]|Ys]) :- N \= 1, modify(Xs, Ys).

?- encode_modified([a, a, a, a, b, c, c, a, a, d, e, e, e, e], X),
  nonvar(X),
  X = [[4, a], b, [2, c], [2, a], d, [4, e]],
  writeln(ok).


% 1.12 (**) Decode a run-length encoded list.

% Example:
% ?- decode([[4, a], [1, b], [2, c], [2, a], [1, d], [4, e]], X).
% X = [a, a, a, a, b, c, c, a, a, d, e, e, e, e]

decode([], []).
decode([[N,X]|Xs], Y) :- repeat(X, N, A), decode(Xs, B), append(A, B, Y).

repeat(_, 0, []).
repeat(X, N, [X|Xs]) :- N_ is N-1, repeat(X, N_, Xs).

?- decode([[4, a], [1, b], [2, c], [2, a], [1, d], [4, e]], X),
  nonvar(X),
  X = [a, a, a, a, b, c, c, a, a, d, e, e, e, e],
  writeln(ok).


% 1.13 (**) Run-length encoding of a list (direct solution).
% Implement the so-called run-length encoding data compression method directly.
% I.e. don't explicitly create the sublists containing the duplicates, only
% count them. As in problem 1.11, simplify the result list by replacing the
% singleton terms [1, X] by X.

% Example:
% ?- encode_direct([a, a, a, a, b, c, c, a, a, d, e, e, e, e], X).
% X = [[4, a], b, [2, c], [2, a], d, [4, e]]

encode_direct([], []).
encode_direct([X], [X]).
encode_direct([X|Xs], [[2,X]|T]) :- encode_direct(Xs, [X|T]).
encode_direct([X|Xs], [[N_,X]|T]) :- encode_direct(Xs, [[N,X]|T]), N_ is N+1.
encode_direct([X|Xs], [X,Y|T]) :- encode_direct(Xs, [Y|T]), Y \= X, Y \= [_, X].

?- encode_direct([a, a, a, a, b, c, c, a, a, d, e, e, e, e], X),
  nonvar(X),
  X = [[4, a], b, [2, c], [2, a], d, [4, e]],
  writeln(ok).


% 1.14 (*) Duplicate the elements of a list.

% Example:
% ?- dupli([a, b, c, c, d], X).
% X = [a, a, b, b, c, c, c, c, d, d]

dupli([], []).
dupli([X|Xs], [X,X|Y]) :- dupli(Xs, Y).

?- dupli([a, b, c, c, d], X),
  nonvar(X),
  X = [a, a, b, b, c, c, c, c, d, d],
  writeln(ok).


% 1.15 ()*) Duplicate the elements of a list a given number of times.

% Example:
% ?- dupli([a, b, c], 3, X).
% X = [a, a, a, b, b, b, c, c, c]

dupli([], _, []).
dupli([X|Xs], N, Y) :- repeat(X, N, D), dupli(Xs, N, T), append(D, T, Y).

?- dupli([a, b, c], 3, X),
  nonvar(X),
  X = [a, a, a, b, b, b, c, c, c],
  writeln(ok).


% 1.16 (**) Drop every Nth element from a list.

% Example:
% ?- drop_every([a, b, c, d, e, f, g, h, i, k], 3, X).
% X = [a, b, d, e, g, h, k]

drop_every([], _, []).
drop_every(X, N, Y) :-
  X \= [],
  N_ is N-1,
  take(N_, X, S),
  drop(N, X, E),
  drop_every(E, N, E_),
  append(S, E_, Y).

take(_, [], []).
take(0, _, []).
take(N, [X|Xs], [X|Y]) :- N > 0, N_ is N-1, take(N_, Xs, Y).

drop(_, [], []).
drop(0, X, X).
drop(N, [_|Xs], Y) :- N > 0, N_ is N-1, drop(N_, Xs, Y).

?- drop_every([a, b, c, d, e, f, g, h, i, k], 3, X),
  nonvar(X),
  X = [a, b, d, e, g, h, k],
  writeln(ok).


% 1.17 (*) Split a list into two parts; the length of the first part is given.

% Example:
% ?- split([a, b, c, d, e, f, g], 3, L1, L2)
% L1 = [a, b, c]
% L2 = [d, e, f, g]

split(X, N, L1, L2) :- take(N, X, L1), drop(N, X, L2).

?- split([a, b, c, d, e, f, g], 3, L1, L2),
  nonvar(L1),
  nonvar(L2),
  L1 = [a, b, c],
  L2 = [d, e, f, g],
  writeln(ok).


% 1.18 (**) Extract a slice from a list.
% Given two indices, I and K, the slice is the list containing the elements
% between the Ith and Kth element of the original list (both limits included).
% Start counting the elements with 1.

% Example:
% ?- slice([a, b, c, d, e, f, g, h, i, k], 3, 7, L)
% L = [c, d, e, f, g]

slice(X, I, K, L) :- take(K, X, S), I_ is I-1, drop(I_, S, L).

?- slice([a, b, c, d, e, f, g, h, i, k], 3, 7, L),
  nonvar(L),
  L = [c, d, e, f, g],
  writeln(ok).


% 1.19 (**) Rotate a list N places to the left.

% Examples:
% ?- rotate([a, b, c, d, e, f, g, h], 3, X).
% X = [d, e, f, g, h, a, b, c]
%
% ?- rotate([a, b, c, d, e, f, g, h], -2, X).
% X = [g, h, a, b, c, d, e, f]

rotate(X, N, Y) :-
  length(X, L), N_ is N mod L, split(X, N_, L1, L2), append(L2, L1, Y).

?- rotate([a, b, c, d, e, f, g, h], 3, X),
  nonvar(X),
  X = [d, e, f, g, h, a, b, c],
  writeln(ok).
?- rotate([a, b, c, d, e, f, g, h], -2, X),
  nonvar(X),
  X = [g, h, a, b, c, d, e, f],
  writeln(ok).


% 1.20 (*) Remove the Kth element from a list.

% Example:
% ?- remove_at(X, [a, b, c, d], 2, R).
% X = b
% R = [a, c, d]

remove_at(X, L, N, R) :- N_ is N-1, split(L, N_, S, [X|E]), append(S, E, R).

?- remove_at(X, [a, b, c, d], 2, R),
  nonvar(X),
  nonvar(R),
  X = b,
  R = [a, c, d],
  writeln(ok).


% 1.21 (*) Insert an element at a given position into a list.

% Example:
% ?- insert_at(alfa,[a,b,c,d],2,L).
% L = [a,alfa,b,c,d]

insert_at(X, L, N, R) :- N_ is N-1, split(L, N_, L1, L2), append(L1, [X|L2], R).

?- insert_at(alfa,[a,b,c,d],2,L), nonvar(L), L = [a,alfa,b,c,d], writeln(ok).


% 1.22 (*) Create a list containing all integers within a given range.

% Example:
% ?- range(4, 9, L).
% L = [4, 5, 6, 7, 8, 9]

range(N, N, [N]).
range(A, B, [A|R]) :- A \= B, A_ is A + sign(B-A), range(A_, B, R).

?- range(4, 9, L),
  nonvar(L),
  L = [4, 5, 6, 7, 8, 9],
  writeln(ok).


% % Skip: there's no random function
% % 1.23 (**) Extract a given number of randomly selected elements from a list.

% % Example:
% % ?- rnd_select([a,b,c,d,e,f,g,h],3,L).
% % L = [e,d,a]


% % 1.24 (*) Lotto: Draw N different random numbers from the set 1..M

% % Example:
% % ?- lotto(6,49,L).
% % L = [23,1,17,33,21,37]


% % 1.25 (*) Generate a random permutation of the elements of a list.

% % Example:
% % ?- rnd_permu([a,b,c,d,e,f],L).
% % L = [b,a,d,c,e,f]


% 1.26 (**) Generate the combinations of K distinct objects chosen from the N
% elements of a list.

% Example:
% ?- combination(3,[a,b,c,d,e,f],L).
% L = [a,b,c] ;
% L = [a,b,d] ;
% L = [a,b,e] ;
% ...

combination(0, _, []).
combination(N, X, [H|R]) :-
  0 < N, tails(X, [H|T]), N_ is N-1, combination(N_, T, R).

tails(X, X).
tails([_|Xs], T) :- tails(Xs, T).

?- combination(3,[a,b,c,d,e,f],[a,c,e]), writeln(ok).
?- combination(3,[a,b,c,d,e,f],[d,e,f]), writeln(ok).


% 1.27 (**) Group the elements of a set into disjoint subsets.
% a) Generate via backtracking all the ways a group of 9 people can work in 3
%    disjoint subgroups of 2, 3, and 4 persons.
%
%    Example:
%    ?- group3([aldo,beat,carla,david,evi,flip,gary,hugo,ida],G1,G2,G3).
%    G1 = [aldo,beat], G2 = [carla,david,evi], G3 = [flip,gary,hugo,ida]
%    ...


% b) Generalize the above predicate in a way that we can specify a list of
%    group sizes and the predicate will return a list of groups.
%
%    Example:
%    ?- group([aldo,beat,carla,david,evi,flip,gary,hugo,ida],[2,2,5],Gs).
%    Gs = [[aldo,beat],[carla,david],[evi,flip,gary,hugo,ida]]
%    ...


% 1.28 (**) Sorting a list of lists according to length of sublists.
% a) Suppose that a list contains elements that are lists themselves. The
%    objective is to sort the elements of InList according to their length.
%
%    Example:
%    ?- lsort([[a,b,c],[d,e],[f,g,h],[d,e],[i,j,k,l],[m,n],[o]],L).
%    L = [[o], [d, e], [d, e], [m, n], [a, b, c], [f, g, h], [i, j, k, l]]


% b) This time, sort the list by their length frequency, i.e. lists with rare
%    lengths are placed first, others with a more frequent length come later.
%
%    Example:
%    ?- lfsort([[a,b,c],[d,e],[f,g,h],[d,e],[i,j,k,l],[m,n],[o]],L).
%    L = [[i, j, k, l], [o], [a, b, c], [f, g, h], [d, e], [d, e], [m, n]]
