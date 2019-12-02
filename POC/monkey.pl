:- module(monkey, [monkeyAStarPlan/1]).

:- use_module("utils/planning").
% PLANNER EXAMPLE

:- beginDomainDefinition(monkeyDomain).

type(place).
type(character).
type(object).

% actionSpecification(action, typeSpecs, prologConditions, conditions, removedFacts, addedFacts, prologSideEffects).
actionSpec(move(X, Y), [place(X), place(Y)], [X \== Y], [onPos(monkey, X), onGround(monkey)], [], [], [onPos(monkey, X)], [onPos(monkey, Y)]).
actionSpec(carry(X, Y), [place(X), place(Y)], [X \== Y], [onPos(monkey, X), onPos(stairs, X), onGround(monkey)], [], [], [onPos(monkey, X), onPos(stairs, X)], [onPos(monkey, Y), onPos(stairs, Y)]).
actionSpec(climb(X), [place(X)], [], [onPos(monkey, X), onPos(stairs, X), onGround(monkey), strength(monkey, S)], [S > 0], [], [onGround(monkey)], []).
actionSpec(workOut(S, NewS), [], [], [strength(monkey, S)], [NewS is S + 1], [], [strength(monkey, S)], [strength(monkey, NewS)]).
actionSpec(win(X), [place(X)], [], [onPos(monkey, X), onPos(banana, X), \+ onGround(monkey)], [], [], [], [win]).

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
  onGround(monkey),
  strength(monkey, 0)
  ]).

heuristic(actionExecution(carry(b, c),_), 15) :- !.
heuristic(actionExecution(_,_), 5).

monkeyAStarPlan(Plan) :- facts(X), planAStar(monkeyDomain, X, [win], heuristic, Plan, _, _).

% The following query retrieves the plan:
% monkeyPlan(Plan).