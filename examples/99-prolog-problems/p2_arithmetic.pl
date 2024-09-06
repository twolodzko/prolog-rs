% 2.01 (**) Determine whether a given number is prime.

% Example:
% ?- is_prime(7).
% true

is_prime(N) :- integer(N), N > 1, \+ has_factor(N, 2).

has_factor(N, K) :- K * K =< N, N mod K =:= 0.
has_factor(N, K) :- K * K =< N, K_ is K + 1, has_factor(N, K_).

?- \+ is_prime(4), writeln(ok).
?- is_prime(7), writeln(ok).


% 2.02 (**) Determine the prime factors of a given positive integer.
% Construct a flat list containing the prime factors in ascending order.

% Example:
% ?- prime_factors(315, L).
% L = [3, 3, 5, 7]

prime_factors(N, Fs) :- N > 1, prime_factors(N, 2, Fs).

prime_factors(1, _, []) :- !.
prime_factors(N, P, Fs) :-
  P =< N, N mod P =\= 0, !, next_prime(P, P_), prime_factors(N, P_, Fs).
prime_factors(N, P, [P|Fs]) :-
  P =< N, N_ is N / P, prime_factors(N_, P, Fs).

next_prime(P, P_) :- P_ is P + 1, is_prime(P_), !.
next_prime(P, N) :- P_ is P + 1, next_prime(P_, N).

?- prime_factors(315, L),
  nonvar(L),
  L = [3, 3, 5, 7],
  writeln(ok).


% 2.03 (**) Determine the prime factors of a given positive integer (2).
% Construct a list containing the prime factors and their multiplicity.

% Example:
% ?- prime_factors_mult(315, L).
% L = [[3, 2], [5, 1], [7, 1]]

prime_factors_mult(N, FMs) :- prime_factors(N, Fs), group_count(Fs, FMs).

group_count([], []).
group_count([X], [[X, 1]]).
group_count([X|Xs], [[X, N_]|E]) :- group_count(Xs, [[X, N]|E]), N_ is N + 1.
group_count([X|Xs], [[X, 1], [Y, N]|E]) :- group_count(Xs, [[Y, N]|E]), X \= Y.

?- prime_factors_mult(315, L),
  nonvar(L),
  L = [[3, 2], [5, 1], [7, 1]],
  writeln(ok).


% 2.04 (*) A list of prime numbers.
% Given a range of integer by its lower and upper limit, construct a list of all
% prime numbers in that range.

prime_list(A,B,L) :- A =< 2, !, p_list(2,B,L).
prime_list(A,B,L) :- A1 is (A // 2) * 2 + 1, p_list(A1,B,L).

p_list(A,B,[]) :- A > B, !.
p_list(A,B,[A|L]) :- is_prime(A), !,
   next(A,A1), p_list(A1,B,L).
p_list(A,B,L) :-
   next(A,A1), p_list(A1,B,L).

next(2,3) :- !.
next(A,A1) :- A1 is A + 2.


% 2.05 (**) Goldbach's conjecture.
% Goldbach's conjecture says that every positive even number greater than 2 is
% the sum of two prime numbers. It is one of the most famous facts in number
% theory that has not been proven to be correct in the general case, but has
% been numerically confirmed up to very large numbers (much larger than is
% possible in Prolog). Write a predicate to find the two prime numbers that sum
% to a given even number.

% Example:
% ?- goldbach(28, L).
% L = [5, 23]

goldbach(4,[2,2]) :- !.
goldbach(N,L) :- N mod 2 =:= 0, N > 4, goldbach(N,L,3).

goldbach(N,[P,Q],P) :- Q is N - P, is_prime(Q), !.
goldbach(N,L,P) :- P < N, next_prime(P,P1), goldbach(N,L,P1).

next_prime(P,P1) :- P1 is P + 2, is_prime(P1), !.
next_prime(P,P1) :- P2 is P + 2, next_prime(P2,P1).

member(X, [X|_]).
member(X, [_|Xs]) :- member(X, Xs).

?- goldbach(28, L),
  nonvar(L),
  L = [5, 23],
  writeln(ok).


% 2.06 (**) A list of Goldbach compositions.
% a) Given a range of integers by its lower and upper limit, print a list of all
%    even numbers and their Goldbach composition.
%
%    Example:
%    ?- goldbach_list(9,20).
%    10 = 3 + 7
%    12 = 5 + 7
%    14 = 3 + 11
%    16 = 3 + 13
%    18 = 5 + 13
%    20 = 3 + 17


% b) In most cases, if an even number is written as the sum of two prime
%    numbers, one of them is very small. Very rarely, the primes are both
%    bigger than say 50. Try to find out how many such cases there are in the
%    range 2..3000.
%
%    Example (for a print limit of 50):
%    ?- goldbach_list(1,2000,50).
%    992 = 73 + 919
%    1382 = 61 + 1321
%    1856 = 67 + 1789
%    1928 = 61 + 1867


% 2.07 (**) Determine the greatest common divisor of two positive integer
% numbers. Use Euclid's algorithm.

% Example:
% ?- gcd(36, 63, G).
% G = 9

% Define gcd as an arithmetic function, so you can use it like this:
% ?- G is gcd(36, 63).
% G = 9

gcd(N1, 0, N1).
gcd(N1, N2, D) :- N2 > 0, R is N1 mod N2, gcd(N2, R, D).

?- gcd(36, 63, G),
  nonvar(G),
  G = 9,
  writeln(ok).


% 2.08 (*) Determine whether two positive integer numbers are coprime.
% Two numbers are coprime if their greatest common divisor equals 1.

% Example:
% ?- coprime(35, 64).
% true

coprime(A, B) :- gcd(A, B, 1).

?- coprime(35, 64), writeln(ok).


% 2.09 (**) Calculate Euler's totient function phi(m).
% Euler's so-called totient function phi(m) is defined as the number of positive
% integers r (1 <= r < m) that are coprime to m.
% E.g. m = 10, r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
%
% Find out what the value of phi(m) is if m is a prime number. Euler's totient
% function plays an important role in one of the most widely used public key
% cryptography methods (RSA). In this exercise, use the most primitive method to
% calculate this function. There is a smarter way that we shall use in 2.10.

% Example:
% ?- Phi is totient_phi(10).
% Phi = 4

totient_phi(N, R) :- totient_phi(1, N, R).

totient_phi(_, 1, 1) :- !.
totient_phi(U, U, 0) :- !.
totient_phi(L, U, R) :-
  L < U, coprime(L, U), !, L_ is L + 1, totient_phi(L_, U, R_), R is R_ + 1.
totient_phi(L, U, R) :- L < U, L_ is L + 1, totient_phi(L_, U, R).

?- totient_phi(10, Phi),
  nonvar(Phi),
  Phi = 4,
  writeln(ok).



% 2.10 (**) Calculate Euler's totient function phi(m) (2).
% See problem 2.09 for the definition of Euler's totient function. If the list
% of the prime factors of a number m is known in the form of problem 2.03 then
% the function phi(m) can be efficiently calculated as follows: Let [[p1, m1],
% [p2, m2], [p3, m3], ...] be the list of prime factors (and their
% multiplicities) of a given number m. Then phi(m) can be calculated with the
% following formula:
% phi(m) = (p1 - 1) * p1**(m1 - 1)
%        * (p2 - 1) * p2**(m2 - 1)
%        * (p3 - 1) * p3**(m3 - 1)
%        * ...


% 2.11 (*) Compare the two methods of calculating Euler's totient function.
% Use the solutions of problems 2.09 and 2.10 to compare the algorithms. Take
% the number of logical inferences as a measure for efficiency. Try to calculate
% phi(10090) as an example.
