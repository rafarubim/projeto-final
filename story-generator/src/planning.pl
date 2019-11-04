facts([
  onPos(monkey, a),
  onPos(stairs, b),
  onPos(banana, c),
  onGround(monkey)
  ]).

elementOf(H, [H|_]).
elementOf(X, [_|T]) :-
  elementOf(X, T).

isSet([]).
isSet([H|T]) :-
  \+ elementOf(H, T),
  isSet(T).

subsetOf([], _).
subsetOf([H|T], X) :-
  elementOf(H, X),
  subsetOf(T, X).

equivalentTo(X, Y) :-
  subsetOf(X, Y),
  subsetOf(Y, X).

setWithout([], _, []).
setWithout([H|T], H, T) :- !.
setWithout([H|T], X, [H|Z]) :-
  setWithout(T, X, Z).

setDiff(S, [], S).
setDiff(S, [H|T], Z) :-
  setWithout(S, H, Y),
  setDiff(Y, T, Z).

setWith(S, X, S) :-
  elementOf(X, S), !.
setWith(T, H, [H|T]).

setUnion(S, [], S).
setUnion(S, [H|T], Z) :-
  setWith(S, H, Y),
  setUnion(Y, T, Z).

updatedFacts(Facts, RemovedFacts, AddedFacts, NewFacts) :-
  setDiff(Facts, RemovedFacts, DiffFacts),
  setUnion(DiffFacts, AddedFacts, NewFacts).

actionExecution(Action, Facts, NewFacts) :-
  actionSpecification(Action, Conditions, RemovedFacts, AddedFacts),
  subsetOf(Conditions, Facts),
  updatedFacts(Facts, RemovedFacts, AddedFacts, NewFacts).

actionSpecification(move(Y), [onPos(monkey, X)], [onPos(monkey, X)], [onPos(monkey, Y)]).
%actionSpecification(carry(Y), [onPos(monkey, X), onPos(stairs, X)], [onPos(monkey, X), onPos(stairs, X)], [onPos(monkey, Y), onPos(stairs, Y)]).
%actionSpecification(win, [onPos(monkey, X), onPos(banana, X), \+ onGround(mokey)], [], [win]).