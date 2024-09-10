https://amzi.com/AdventureInProlog/a3simple.php#Chapter3

Prolog queries work by pattern matching. The query pattern is called a goal. If there is a fact that matches the goal,
then the query succeeds and the listener responds with 'yes.' If there is no matching fact, then the query fails
and the listener responds with 'no.'

Prolog's pattern matching is called unification. In the case where the logicbase contains only facts, unification
succeeds if the following three conditions hold.

 * The predicate named in the goal and logicbase are the same.
 * Both predicates have the same arity.
 * All of the arguments are the same.

https://amzi.com/AdventureInProlog/a4comqry.php#Chapter4

Simple goals can be combined to form compound queries. For example, we might want to know if there is anything good
to eat in the kitchen. In Prolog we might ask

    ?- location(X, kitchen), edible(X).

Whereas a simple query had a single goal, the compound query has a conjunction of goals. The comma separating the
goals is read as "and."

https://amzi.com/AdventureInProlog/a5rules.php

We said earlier a predicate is defined by clauses, which may be facts or rules. A rule is no more than a stored query.
Its syntax is

    head :- body.

where

`head` a predicate definition (just like a fact)

`:-` the neck symbol, sometimes read as "if"

`body` one or more goals (a query)

With rules, Prolog unifies the goal pattern with the head of the clause. If unification succeeds, then Prolog
initiates a new query using the goals in the body of the clause.

Rules, in effect, give us multiple levels of queries. The first level is composed of the original goals. The next
level is a new query composed of goals found in the body of a clause from the first level.

Each level can create even deeper levels. Theoretically, this could continue forever. In practice it can continue
until the listener runs out of space.

## Unification

 * https://github.com/dtonhofer/prolog_notes/tree/master/other_notes/about_byrd_box_model
 * https://preserve.lehigh.edu/lehigh-scholarship/graduate-publications-theses-dissertations/theses-dissertations/design-71?article=5506&context=etd
 * https://www.cs.cornell.edu/courses/cs3110/2011sp/Lectures/lec26-type-inference/type-inference.htm#4
 * http://www.cs.trincoll.edu/~ram/cpsc352/notes/unification.html
 * https://www.cs.bham.ac.uk//research/projects/poplog/paradigms_lectures/lecture20.html#representing
 * https://norvig.com/unify-bug.pdf
 * https://staff.um.edu.mt/mcam1/Files/Dissertation.pdf
 * https://lpn.swi-prolog.org/lpnpage.php?pagetype=html&pageid=lpn-htmlch2
 * https://www.amzi.com/articles/prolog_under_the_hood.htm
 * https://homes.cs.washington.edu/~bodik/ucb/cs164/sp13/lectures/07-implementing-prolog-sp13.pdf

## Backtracking

 * https://mmalmsten.medium.com/a-practical-introduction-to-backtracking-in-prolog-b9cfaee0eb6a
 * https://en.wikipedia.org/wiki/Depth-first_search
 * https://en.wikipedia.org/wiki/Backtracking
 * http://jeffe.cs.illinois.edu/teaching/algorithms/book/02-backtracking.pdf
 * https://web.stanford.edu/class/archive/cs/cs106b/cs106b.1188/lectures/Lecture11/Lecture11.pdf
 * https://www.cs.toronto.edu/~hojjat/384w09/Lectures/Lecture-04-Backtracking-Search.pdf

## Warren's Abstract Machine

* https://cs.franklin.edu/~brownc/640/wam-slides.pdf
* https://www.complang.tuwien.ac.at/adrian/forthlog/literatur/wambook.pdf
* https://cliplab.org/logalg/slides/8_wam.pdf
* https://www.sri.com/wp-content/uploads/2021/12/641.pdf

## Other

 * https://www.cs.jhu.edu/~jason/325/PDFSlides/14prolog.pdf
