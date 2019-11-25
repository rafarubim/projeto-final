:- module(event, [beginEventTypesDefinition/0, endEventTypesDefinition/0, merlinPlan/3]).

% eventTypeSpec(EventName:compound_term, Entity)
:- use_module('utils/set').
:- use_module('utils/planning').
:- use_module(entity).
:- use_module(state).

/*
:- beginDomainDefinition(events).
actionSpec(
  move(Char, Plc1, Plc2, Level, NewLevel),
  [],
  [],
  [isIn(Char, Plc1), tiredLevel(Char, Level)],
  [event:ignoreNonDet(state:respectsSignature(isIn(Char, Plc2), _)), event:ignoreNonDet(state:respectsSignature(tiredLevel(Char, NewLevel), _)), Plc1 \== Plc2, NewLevel is Level + 10],
  [],
  [isIn(Char, Plc1), tiredLevel(Char, Level)],
  [isIn(Char, Plc2), tiredLevel(Char, NewLevel)]
).
:- endDomainDefinition(events).
*/

:- use_module('utils/assertRuntimeTerms').

:- module_transparent([beginEventTypesDefinition/0, endEventTypesDefinition/0]).

:- dynamic eventTypeSpec/7.

beginEventTypesDefinition :-
  beginAssertRuntimeTerms(event, [eventTypeSpec/7]).

endEventTypesDefinition :-
  endAssertRuntimeTerms.

eventTypeSpec(
  move(Char, Plc1, Plc2, Level, NewLevel),  	    % Event name
  [standsIn(Char, Plc1), tiredLevel(Char, Level)],    % State Conditions
  [Plc1 \== Plc2, NewLevel is Level + 10],        % Prolog Conditions
  [],                                             % Trigger Conditions
  0,                                              % Duration
  [standsIn(Char, Plc1), tiredLevel(Char, Level)],    % Removed States
  [standsIn(Char, Plc2), tiredLevel(Char, NewLevel)]  % Added States
).

ignoreNonDet(Goal) :-
  Goal.
ignoreNonDet(Goal) :-
  \+ Goal.

instancedState(Assertion, event:ignoreNonDet(respectsSignature(Assertion))).

eventTypeToActionSpec(EventSpecTerm, ActionSpecTerm) :-
  EventSpecTerm =.. [eventTypeSpec, EventName, StateConditions, PrologConditions, _, _, Retractions, Assertions],
  maplist(instancedState, Retractions, InstancedRetractions),
  maplist(instancedState, Assertions, InstancedAssertions),
  append([InstancedRetractions, InstancedAssertions, PrologConditions], MorePrologConditions),
  ActionSpecTerm =.. [actionSpec, EventName, [], [], StateConditions, MorePrologConditions, [], Retractions, Assertions].

createActionSpecs :-
  beginDomainDefinition(events),
  eventTypeToActionSpec(
    eventTypeSpec(
      move(Char, Plc1, Plc2, Level, NewLevel),
      [standsIn(Char, Plc1), tiredLevel(Char, Level)],
      [Plc1 \== Plc2, NewLevel is Level + 10],
      [],
      0,
      [standsIn(Char, Plc1), tiredLevel(Char, Level)],
      [standsIn(Char, Plc2), tiredLevel(Char, NewLevel)]
    ),
    ActionSpecTerm
  ),
  assert(ActionSpecTerm),
  endDomainDefinition(events).

facts([
  standsIn(merlin, fields),
  tiredLevel(merlin, 0)
]).

heuristic(_, 5).

merlinPlan(Plan, PlanCost, FinalState) :- facts(X), planAStar(events, X, [standsIn(merlin, city)], heuristic, Plan, PlanCost, FinalState).

:- createActionSpecs.

% merlinPlan(Plan,Cost,Final).