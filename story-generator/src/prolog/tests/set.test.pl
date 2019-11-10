:- begin_tests(set).

:- use_module('../utils/set.pl').

% isSet(++Set:list) is semidet
test(isSet) :- isSet([]).
test(isSet) :- isSet([a, b, c]).
test(isSet, [fail]) :- isSet([a, b, a]).

% elementOf(++Element, ++Set:list) is nondet
test(elementOf, [nondet]) :- elementOf(b, [a, b, c]).
test(elementOf, [fail]) :- elementOf(b, []).
test(elementOf, [fail]) :- elementOf(d, [a, b, c]).
% elementOf(+Element, ++Set:list) is nondet
test(elementOf, [all(X=[a,b,c])]) :- elementOf(f(X), [f(a), f(b), f(c)]).
test(elementOf, [all(X=[])]) :- elementOf(f(X), []).
% elementOf(++Element, +Set) is nondet
test(elementOf, [all(X = [b])]) :- elementOf(b, [a, X, c]).
test(elementOf, [all(X = [c, _])]) :- elementOf(c, [a, X, c]).
% elementOf(?Element, ++Set) is nondet
test(elementOf, [all(X = [a,b,c])]) :- elementOf(X, [a, b, c]).
test(elementOf, [all(X=[])]) :- elementOf(X, []).
% elementOf(?Element, +Set) is nondet
test(elementOf, [all([X, Y] = [[a,_],[_,_],[c,_]])]) :- elementOf(X, [a, Y, c]).

test(setDiff, [true(X = [])]) :- setDiff([], [], X).
test(setDiff, [true(X = [a,b,c])]) :- setDiff([a, b, c], [], X).
test(setDiff, [all(X = [[b,c]])]) :- setDiff([a, b, c], [a], X).
test(setDiff, [all(X = [[a,c]])]) :- setDiff([a, b, c], [b], X).
test(setDiff, [all(X = [[b]])]) :- setDiff([a, b, c], [a, c], X).
test(setDiff, [all(X = [[]])]) :- setDiff([a, b, c], [a, b, c], X).
test(setDiff, [all(X = [a, b, _])]) :- setDiff([a, b], [X], _).

/*
test(subsetOf) :- subsetOf([], []).
test(subsetOf) :- subsetOf([], [a, b, c]).
test(subsetOf, [nondet]) :- subsetOf([a, c], [a, b, c]).
test(subsetOf, [nondet]) :- subsetOf([c, a], [a, b, c]).
test(subsetOf, [nondet]) :- subsetOf([b], [a, b, c]).
test(subsetOf, [all(X = [[],[a],[a,b],[b],[b,a]])]) :- subsetOf(X, [a, b]).
test(subsetOf, [all(X=[a,b])]) :- subsetOf([c, X], [a, b, c]).
test(subsetOf, [all(X=[b])]) :- subsetOf([c, b], [a, X, c]).
test(subsetOf, [all(X=[b])]) :- subsetOf([c, a], [a, X, c]).

test(equivalentTo) :- equivalentTo([], []).
test(equivalentTo, [fail]) :- equivalentTo([], [a]).
test(equivalentTo, [fail]) :- equivalentTo([a], []).
test(equivalentTo, [nondet]) :- equivalentTo([a, b, c], [a, b, c]).
test(equivalentTo, [nondet]) :- equivalentTo([a, b, c], [a, c, b]).
test(equivalentTo, [nondet]) :- equivalentTo([a, b, c], [b, c, a]).
test(equivalentTo, [nondet]) :- equivalentTo([a, b, c], [b, c, a]).
*/
:- end_tests(set).