:- module(event, [merlinPlan/3]).

% eventTypeSpec(EventName:compound_term, Entity)
:- use_module('utils/set').
:- use_module(state).
:- use_module('utils/planning').

/*
:- beginDomainDefinition(events).
actionSpec(
  move(Char, Plc1, Plc2, Level, NewLevel),
  [],
  [],
  [isIn(Char, Plc1), tiredLevel(Char, Level)],
  [events:ignoreNonDet(state:respectsSignature(isIn(Char, Plc2), _)), events:ignoreNonDet(state:respectsSignature(tiredLevel(Char, NewLevel), _)), Plc1 \== Plc2, NewLevel is Level + 10],
  [],
  [isIn(Char, Plc1), tiredLevel(Char, Level)],
  [isIn(Char, Plc2), tiredLevel(Char, NewLevel)]
).
:- endDomainDefinition(events).
*/

eventSpec(
  move(Char, Plc1, Plc2, Level, NewLevel),  	    % Event name
  [isIn(Char, Plc1), tiredLevel(Char, Level)],    % State Conditions
  [Plc1 \== Plc2, NewLevel is Level + 10],        % Prolog Conditions
  [],                                             % Trigger Conditions
  0,                                              % Duration
  [isIn(Char, Plc1), tiredLevel(Char, Level)],    % Removed States
  [isIn(Char, Plc2), tiredLevel(Char, NewLevel)]  % Added States
).

ignoreNonDet(Goal) :-
  Goal.
ignoreNonDet(Goal) :-
  \+ Goal.

instancedState(Assertion, events:ignoreNonDet(state:respectsSignature(Assertion, _))).

eventToActionSpec(EventSpecTerm, ActionSpecTerm) :-
  EventSpecTerm =.. [eventSpec, EventName, StateConditions, PrologConditions, _, _, Retractions, Assertions],
  maplist(instancedState, Retractions, InstancedRetractions),
  maplist(instancedState, Assertions, InstancedAssertions),
  append([InstancedRetractions, InstancedAssertions, PrologConditions], MorePrologConditions),
  ActionSpecTerm =.. [actionSpec, EventName, [], [], StateConditions, MorePrologConditions, [], Retractions, Assertions].

createActionSpecs :-
  beginDomainDefinition(events),
  eventToActionSpec(
    eventSpec(
      move(Char, Plc1, Plc2, Level, NewLevel),
      [isIn(Char, Plc1), tiredLevel(Char, Level)],
      [Plc1 \== Plc2, NewLevel is Level + 10],
      [],
      0,
      [isIn(Char, Plc1), tiredLevel(Char, Level)],
      [isIn(Char, Plc2), tiredLevel(Char, NewLevel)]
    ),
    ActionSpecTerm
  ),
  assert(ActionSpecTerm),
  endDomainDefinition(events).

facts([
  isIn(merlin, fields),
  tiredLevel(merlin, 0)
]).

heuristic(_, 5).

merlinPlan(Plan, PlanCost, FinalState) :- facts(X), planAStar(events, X, [isIn(merlin, city)], heuristic, Plan, PlanCost, FinalState).
