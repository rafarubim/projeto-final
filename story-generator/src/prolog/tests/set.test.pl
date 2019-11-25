:- begin_tests(set).

:- use_module('../utils/set.pl').

/**
 * <module> Test Set
 * 
 * This module tests the set module.
 * 
 * @author Rafael Rubim Cabral
 * @version 0.1.0
 * 
 * @see Docs model: https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/pldoc.html%27)
 **/

% isSet(++Set:list) is semidet
test(isSet) :- isSet([]).
test(isSet) :- isSet([a, b, c]).
test(isSet, [fail]) :- isSet([a, b, a]).

% elementOf(++Element:any, ++Set:list) is nondet
test(elementOf, [nondet]) :- elementOf(b, [a, b, c]).
test(elementOf, [fail]) :- elementOf(b, []).
test(elementOf, [fail]) :- elementOf(d, [a, b, c]).
% elementOf( +Element:any, ++Set:list) is nondet
test(elementOf, [all(X=[a,b,c])]) :- elementOf(f(X), [f(a), f(b), f(c)]).
test(elementOf, [fail]) :- elementOf(f(_), []).
% elementOf( ?Element:any, ++Set:list) is nondet
test(elementOf, [all(X = [a,b,c])]) :- elementOf(X, [a, b, c]).
test(elementOf, [fail]) :- elementOf(_, []).

% setDiff(++setA:list, ++setB:list, -Diff:list) is multi
test(setDiff, [all(X = [[]])]) :- setDiff([], [], X).
test(setDiff, [all(X = [[a,b,c]])]) :- setDiff([a, b, c], [], X).
test(setDiff, [all(X = [[b,c]])]) :- setDiff([a, b, c], [a], X).
test(setDiff, [all(X = [[a,c]])]) :- setDiff([a, b, c], [b], X).
test(setDiff, [all(X = [[b]])]) :- setDiff([a, b, c], [a, c], X).
test(setDiff, [all(X = [[]])]) :- setDiff([a, b, c], [a, b, c], X).

% setUnion(++setA:list, ++setB:list, -Union:list) is det
test(setUnion, [all(X = [[]])]) :- setUnion([], [], X).
test(setUnion, [all(X = [[c,b,a]])]) :- setUnion([a, b, c], [], X).
test(setUnion, [all(X = [[a,b,c]])]) :- setUnion([], [a, b, c], X).
test(setUnion, [all(X = [[c,b,a]])]) :- setUnion([a, b, c], [a], X).
test(setUnion, [all(X = [[c,a,b]])]) :- setUnion([a, b, c], [b], X).
test(setUnion, [all(X = [[a,b,c]])]) :- setUnion([b], [a, b, c], X).
test(setUnion, [all(X = [[b,a,c]])]) :- setUnion([a, b, c], [a, c], X).
test(setUnion, [all(X = [[a,b,c]])]) :- setUnion([a, b, c], [a, b, c], X).

% subsetOf(++Subset:list, ++Set:list) is nondet
test(subsetOf, [nondet]) :- subsetOf([], []).
test(subsetOf, [fail]) :- subsetOf([a], []).
test(subsetOf, [fail]) :- subsetOf([d], [a, b, c]).
test(subsetOf, [nondet]) :- subsetOf([], [a, b, c]).
test(subsetOf, [nondet]) :- subsetOf([a, c], [a, b, c]).
test(subsetOf, [nondet]) :- subsetOf([c, a], [a, b, c]).
test(subsetOf, [nondet]) :- subsetOf([b], [a, b, c]).
test(subsetOf, [fail]) :- subsetOf([a, d], [a, b, c]).
test(subsetOf, [fail]) :- subsetOf([d, a], [a, b, c]).
% subsetOf( +Subset:list, ++Set:list) is nondet
test(subsetOf, [fail]) :- subsetOf([_], []).
test(subsetOf, [fail]) :- subsetOf([a|_], []).
test(subsetOf, [all(X=[[],[b]])]) :- subsetOf([a|X], [a, b]).
test(subsetOf, [all(X=[[],[a]])]) :- subsetOf([b|X], [a, b]).
test(subsetOf, [all(X=[a,b,c])]) :- subsetOf([X], [a, b, c]).
test(subsetOf, [all(X=[b,c])]) :- subsetOf([a, X], [a, b, c]).
test(subsetOf, [all(X=[a,b])]) :- subsetOf([c, X], [a, b, c]).
test(subsetOf, [all(X=[[],[a],[a,c],[c],[c,a]])]) :- subsetOf([b|X], [a, b, c]).
test(subsetOf, [all(X=[[],[c]])]) :- subsetOf([a, b|X], [a, b, c]).
test(subsetOf, [all(X=[[],[c]])]) :- subsetOf([b, a|X], [a, b, c]).
test(subsetOf, [all(X=[[]])]) :- subsetOf([a, b, c|X], [a, b, c]).
test(subsetOf, [all(X=[[]])]) :- subsetOf([c, a, b|X], [a, b, c]).
test(subsetOf, [fail]) :- subsetOf([_, d], [a, b, c]).
test(subsetOf, [fail]) :- subsetOf([d, _], [a, b, c]).
test(subsetOf, [fail]) :- subsetOf([a, d|_], [a, b, c]).
test(subsetOf, [fail]) :- subsetOf([d, a|_], [a, b, c]).
% subsetOf( ?Subset:list, ++Set:list) is nondet
test(subsetOf, [all(X=[[]])]) :- subsetOf(X, []).
test(subsetOf, [all(X = [[],[a],[a,b],[b],[b,a]])]) :- subsetOf(X, [a, b]).

% equivalentTo(++SetA:list, ++SetB:list) is nondet
test(equivalentTo, [nondet]) :- equivalentTo([], []).
test(equivalentTo, [fail]) :- equivalentTo([], [a]).
test(equivalentTo, [fail]) :- equivalentTo([a], []).
test(equivalentTo, [nondet]) :- equivalentTo([a, b, c], [a, b, c]).
test(equivalentTo, [nondet]) :- equivalentTo([a, b, c], [a, c, b]).
test(equivalentTo, [nondet]) :- equivalentTo([a, b, c], [b, c, a]).
test(equivalentTo, [nondet]) :- equivalentTo([a, b, c], [b, c, a]).
% equivalentTo( +SetA:list, ++SetB:list) is nondet
test(equivalentTo, [fail]) :- equivalentTo([_], []).
test(equivalentTo, [all(X=[a])]) :- equivalentTo([X], [a]).
test(equivalentTo, [all(X=[b])]) :- equivalentTo([a, X, c], [a, b, c]).
test(equivalentTo, [fail]) :- equivalentTo([a, _, d], [a, b, c]).
test(equivalentTo, [all([X,Y]=[[a,b],[b,a]])]) :- equivalentTo([X, Y, c], [a, c, b]).
test(equivalentTo, [all(X=[[a,c],[c,a]])]) :- equivalentTo([b|X], [a, b, c]).
test(equivalentTo, [fail]) :- equivalentTo([d|_], [a, b, c]).
% equivalentTo( ?SetA:list, ++SetB:list) is nondet
test(equivalentTo, [all(X=[[]])]) :- equivalentTo(X, []).
test(equivalentTo, [all(X=[[a]])]) :- equivalentTo(X, [a]).
test(equivalentTo, [all(X=[[a,b],[b,a]])]) :- equivalentTo(X, [a, b]).

% setIsOneOf(++Set:list, ++SetList:list) is nondet
test(setIsOneOf, [fail]) :- setIsOneOf([], []).
test(setIsOneOf, [nondet]) :- setIsOneOf([], [[]]).
test(setIsOneOf, [nondet]) :- setIsOneOf([b,a], [[a,b]]).
test(setIsOneOf, [fail]) :- setIsOneOf([], [[a,b], [c,d], ['e',f]]).
test(setIsOneOf, [nondet]) :- setIsOneOf([], [[a,b], [], ['e',f]]).
test(setIsOneOf, [nondet]) :- setIsOneOf([d,c], [[a,b], [c,d], ['e',f]]).
test(setIsOneOf, [fail]) :- setIsOneOf([d,c], [[a,b], [c], ['e',f]]).
% setIsOneOf( +Set:list, ++SetList:list) is nondet
test(setIsOneOf, [fail]) :- setIsOneOf([_], []).
test(setIsOneOf, [fail]) :- setIsOneOf([_], [[]]).
test(setIsOneOf, [fail]) :- setIsOneOf([_], [[a,b], [c,d], ['e',f]]).
test(setIsOneOf, [all(X=[a])]) :- setIsOneOf([X], [[a]]).
test(setIsOneOf, [all(X=[c])]) :- setIsOneOf([X], [[a,b], [c], ['e',f]]).
test(setIsOneOf, [all(X=[b])]) :- setIsOneOf([X,a], [[a,b]]).
test(setIsOneOf, [all(X=[d])]) :- setIsOneOf([X,c], [[a,b], [c,d], ['e',f]]).
test(setIsOneOf, [all([X,Y]=[[a,b],[b,a],[c,d],[d,c]])]) :- setIsOneOf([X, Y], [[a,b], [c,d]]).
% setIsOneOf( ?Set:list, ++SetList:list) is nondet
test(setIsOneOf, [fail]) :- setIsOneOf(_, []).
test(setIsOneOf, [all(X=[[]])]) :- setIsOneOf(X, [[]]).
test(setIsOneOf, [all(X=[[a,b],[b,a]])]) :- setIsOneOf(X, [[a,b]]).
test(setIsOneOf, [all(X=[[a,b],[b,a],[c,d],[d,c]])]) :- setIsOneOf(X, [[a,b], [c,d]]).

% hasIntersection(++SetA:list, ++SetB:list) is semidet
test(hasIntersection, [fail]) :- hasIntersection([], []).
test(hasIntersection, [fail]) :- hasIntersection([a, b], []).
test(hasIntersection, [fail]) :- hasIntersection([], [a, b]).
test(hasIntersection, [fail]) :- hasIntersection([a, b], [c, d]).
test(hasIntersection) :- hasIntersection([a, b], [b, c]).
test(hasIntersection) :- hasIntersection([a, b], [c, b]).
% hasIntersection(+SetA:list, ++SetB:list) is semidet
test(hasIntersection, [fail]) :- hasIntersection([_], []).
test(hasIntersection, [fail]) :- hasIntersection([a, _], []).
test(hasIntersection, [true(X=a)]) :- hasIntersection([X], [a, b]).
test(hasIntersection, [true(X=b)]) :- hasIntersection([a, X], [b, c]).
test(hasIntersection) :- hasIntersection([a, _], [a, b]).

:- end_tests(set).