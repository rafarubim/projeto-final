:-  module(set, [isSet/1, elementOf/2, subsetOf/2, equivalentTo/2, setIsOneOf/2, setDiff/3, setUnion/3]).

% -------------------- Public predicates

% isSet(set)
isSet([]).
isSet([H|T]) :-
  not(elementOf(H, T)),
  isSet(T).

% elementOf(Element, set)
elementOf(H, [H|_]).
elementOf(X, [_|T]) :-
  elementOf(X, T).

% subsetOf(subset, set)
subsetOf([], _).
subsetOf([H|T], X) :-
  elementOf(H, X),
  subsetOf(T, X).

% equivalentTo(setA, setB)
equivalentTo(X, Y) :-
  subsetOf(X, Y),
  subsetOf(Y, X).

% setIsOneOf(set, setList)
setIsOneOf(S, [H|_]) :-
  equivalentTo(S, H), !. % red cut
setIsOneOf(S, [_,T]) :-
  setIsOneOf(S, T). 

% setDiff(setA, setB, Diff)
setDiff([], S, S).
setDiff([H|T], S, Z) :-
  setWithout(S, H, Y),
  setDiff(T, Y, Z).

% setUnion(setA, setB, Union)
setUnion([], S, S).
setUnion([H|T], S, Z) :-
  setWith(S, H, Y),
  setUnion(T, Y, Z).

% -------------------- Private predicates

% setWith(set, element, SetWithElement)
setWith(S, X, S) :-
  elementOf(X, S), !. % red cut
setWith(T, H, [H|T]).

% setWithout(set, element, SetWithoutElement)
setWithout([], _, []).
setWithout([H|T], H, T) :- !. % red cut
setWithout([H|T], X, [H|Z]) :-
  setWithout(T, X, Z).