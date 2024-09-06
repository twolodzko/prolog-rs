% 5.01 (*) Check whether a given term represents a multiway tree.
% Write a predicate istree/1 which succeeds if and only if its argument is a
% Prolog term representing a multiway tree.

% Example:
% ?- istree(t(a,[t(f,[t(g,[])]),t(c,[]),t(b,[t(d,[]),t(e,[])])])).
% Yes

is_multitree(mt(_, [])) :- !.
is_multitree(mt(_, [M|Ms])) :-
    is_multitree(M),
    is_multitree(mt(_, Ms)).

?- is_multitree(mt(a,[mt(f,[mt(g,[])]),mt(c,[]),mt(b,[mt(d,[]),mt(e,[])])])),
    writeln(ok).

% 5.02 (*) Count the nodes of a multiway tree.
% Write a predicate nnodes/1 which counts the nodes of a given multiway tree.

% Example:
% ?- nnodes(t(a,[t(f,[])]),N).
% N = 2

% Write another version of the predicate that allows for a flow pattern (o,i).

nnodes(mt(_, []), 1) :- !.
nnodes(mt(_, [M|Ms]), N) :-
    nnodes(M, N1),
    nnodes(mt(_, Ms), N2),
    N is N1+N2, !.

?- nnodes(mt(a,[mt(f,[])]),2), writeln(ok).
?- nnodes(mt(a,[mt(f,[mt(g,[])]),mt(c,[]),mt(b,[mt(d,[]),mt(e,[])])]),7), writeln(ok).


% 5.03 (**) Tree construction from a node string.
% We suppose that the nodes of a multiway tree contain single characters. In the
% depth-first order sequence of its nodes, a special character ^ has been
% inserted whenever, during the tree traversal, the move is a backtrack to the
% previous level.

% By this rule, the tree in the figure at:
% https://sites.google.com/site/prologsite/prolog-problems/5/p70.gif
% is represented as: afg^^c^bd^e^^^

% Define the syntax of the string and write a predicate tree(String,Tree) to
% construct the Tree when the String is given. Work with atoms (instead of
% strings). Make your predicate work in both directions.


% 5.04 (*) Determine the internal path length of a tree.
% We define the internal path length of a multiway tree as the total sum of the
% path lengths from the root to all nodes of the tree. By this definition, the
% tree in the figure of problem 5.03 has an internal path length of 9.

% Write a predicate ipl(Tree,IPL) for the flow pattern (+,-).

ipl(mt(_, Ms), N) :-
    ipl_acc(Ms, 0, 0, N).

ipl_acc([], Acc1, Acc2, N) :-
    N is Acc1 + Acc2.

ipl_acc([mt(_, Ms1)|Ms2], Acc1, Acc2, Res) :-
    Acc3 is Acc1+1,
    ipl_acc(Ms1, Acc3, Acc2, Res1),
    ipl_acc(Ms2, Acc1, Res1, Res).

?- ipl(mt(a,[mt(f,[mt(g,[])]),mt(c,[]),mt(b,[mt(d,[]),mt(e,[])])]), 9),
    writeln(ok).


 % 5.05 (*) Construct the bottom-up order sequence of the tree nodes.
 % Write a predicate bottom_up(Tree,Seq) which constructs the bottom-up sequence
 % of the nodes of the multiway tree Tree. Seq should be a Prolog list.

 % What happens if you run your predicate backwards?

 bottom_up(Ms, Res) :-
    bottom_up_acc(Ms, [], Res1),
    reverse(Res1, Res).

bottom_up_acc([], Acc, Acc) :- !.
bottom_up_acc(mt(X, []), Acc, [X|Acc]) :- !.
bottom_up_acc(mt(X, [M|Ms]), Acc, Res) :-
    bottom_up_acc(M, Acc, Acc1),
    bottom_up_acc(mt(X, Ms), Acc1, Res).

?- bottom_up(
        mt(a,[mt(f,[mt(g,[])]),mt(c,[]),mt(b,[mt(d,[]),mt(e,[])])]),
        [g, f, c, d, e, b, a]
    ), writeln(ok).


% 5.06 (**) Lisp-like tree representation.
% There is a particular notation for multiway trees in Lisp. Lisp is a prominent
% functional programming language, which is used primarily for artificial
% intelligence problems. As such it is one of the main competitors of Prolog. In
% Lisp almost everything is a list, just as in Prolog everything is a term.

% The picture at:
% https://sites.google.com/site/prologsite/prolog-problems/5/p73.png?attredirects=0
% shows how multiway tree structures are represented in Lisp.

% Note that in the "lispy" notation a node with successors (children) in the
% tree is always the first element in a list, followed by its children. The
% "lispy" representation of a multiway tree is a sequence of atoms and
% parentheses '(' and ')', which we shall collectively call "tokens". We can
% represent this sequence of tokens as a Prolog list; e.g. the lispy expression
% (a (b c)) could be represented as the Prolog list
% ['(', a, '(', b, c, ')', ')']. Write a predicate tree_ltl(T,LTL) which
% constructs the "lispy token list" LTL if the tree is given as term T in the
% usual Prolog notation.

% Example:
% ?- tree_ltl(t(a, [t(b, [t(c, [])])]), LTL).
% LTL = ['(', a, '(', b, c, ')', ')']

% As a second, even more interesting exercise try to rewrite tree_ltl/2 in a way
% that the inverse conversion is also possible: Given the list LTL, construct
% the Prolog tree T. Use difference lists.
