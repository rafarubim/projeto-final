:- module(monkey, [monkeyPlan/1]).

:- use_module("utils/planning").
% PLANNER EXAMPLE

:- beginDomainDefinition(monkeyDomain).

type(place).
type(character).
type(object).

% actionSpecification(action, typeSpecs, prologConditions, conditions, removedFacts, addedFacts, prologSideEffects).
actionSpec(move(X, Y), [place(X), place(Y)], [X \== Y], [onPos(monkey, X), onGround(monkey)], [onPos(monkey, X)], [onPos(monkey, Y)], []).
actionSpec(carry(X, Y), [place(X), place(Y)], [X \== Y], [onPos(monkey, X), onPos(stairs, X), onGround(monkey)], [onPos(monkey, X), onPos(stairs, X)], [onPos(monkey, Y), onPos(stairs, Y)], []).
actionSpec(climb(X), [place(X)], [], [onPos(monkey, X), onPos(stairs, X), onGround(monkey)], [onGround(monkey)], [], []).
actionSpec(win(X), [place(X)], [], [onPos(monkey, X), onPos(banana, X), \+ onGround(monkey)], [], [win], []).

printClause(Clause) :-
  write(Clause), nl.

:- endDomainDefinition(monkeyDomain).

:- beginProblemDefinition(monkeyDomain).

% Type facts
place(a).
place(b).
place(c).

character(monkey).

object(stairs).
object(banana).

:- endProblemDefinition(monkeyDomain).

% State facts
facts([
  onPos(monkey, a),
  onPos(stairs, b),
  onPos(banana, c),
  onGround(monkey)
  ]).

monkeyPlan(Plan) :- facts(X), planExecution(monkeyDomain, X, [win], Plan).

% The following query retrieves the plan:
% monkeyPlan(Plan).