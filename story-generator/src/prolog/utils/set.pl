:-  module(set, [isSet/1, elementOf/2, subsetOf/2, equivalentTo/2, setIsOneOf/2, setDiff/3, setUnion/3]).

/**
 * <module> Set
 * 
 * This module declares predicates to deal with the set data structure.
 * A set is represented by a prolog list which doesn't have repeated elements.
 * The order of the elements in the list doesn't matter, which means [a, b] and
 * [b, a] are equivalent.
 * 
 * @author Rafael Rubim Cabral
 * @version 0.1.0
 **/

% -------------------- Public predicates

%! isSet(++Set:list) is semidet
% True if Set does not contain repeated elements
isSet([]).
isSet([H|T]) :-
  not(elementOf(H, T)),
  isSet(T).

%! elementOf(++Element:any, ++Set:list) is nondet
%! elementOf( +Element:any, ++Set:list) is nondet
%! elementOf(++Element:any,  +Set:list) is nondet
%! elementOf( ?Element:any, ++Set:list) is nondet
%! elementOf( ?Element:any,  +Set:list) is nondet
elementOf(H, [H|_]).
elementOf(X, [_|T]) :-
  elementOf(X, T).

%! setDiff(++setA, ++setB, -Diff) is multi
setDiff(S, [], S) :- !. % green cut
setDiff(S, [H|T], Z) :-
  setWithout(S, H, Y),
  setDiff(Y, T, Z).

% setUnion(setA, setB, Union)
setUnion([], S, S).
setUnion([H|T], S, Z) :-
  setWith(S, H, Y),
  setUnion(T, Y, Z).

% subsetOf(Subset, set)
subsetOf([], _).
subsetOf([H|T], X) :-
  elementOf(H, X),
  setWithout(X, H, Z),
  subsetOf(T, Z).

% equivalentTo(setA, setB)
equivalentTo(X, Y) :-
  subsetOf(X, Y),
  subsetOf(Y, X).

% setIsOneOf(set, setList)
setIsOneOf(S, [H|_]) :-
  equivalentTo(S, H), !. % red cut
setIsOneOf(S, [_,T]) :-
  setIsOneOf(S, T).

% -------------------- Private predicates

% setWith(set, element, SetWithElement)
setWith(S, X, S) :-
  elementOf(X, S), !. % red cut
setWith(T, H, [H|T]).

% setWithout(set, Element, SetWithoutElement)
setWithout([], _, []).
setWithout([H|T], H, T).
setWithout([H|T], X, [H|Z]) :-
  setWithout(T, X, Z),
  H \== X.