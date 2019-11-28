:- module(event, [beginEventTypesDefinition/0, endEventTypesDefinition/0, beginEventsDefinition/0, endEventsDefinition/0, eventTypeToActionSpec/2, eventType/7, eventTypesTriggeredBy/2, createAndExecuteEvent/2]).

eventTypeSpec(
  move(Char, Plc1, Plc2), % Event type name
  [standsIn(Char, Plc1)], % State Conditions
  [Plc1 \== Plc2],        % Prolog Conditions
  [tick],                 % Trigger Conditions
  [tick(0)],              % Effect Triggers
  [standsIn(Char, Plc1)], % Removed States
  [standsIn(Char, Plc2)]  % Added States
).

% eventTypeSpec(?EventTypeName:compound_term, -StateConditions:list, -PrologConditions:list, -TriggerConditions:list, -EffectTriggers:list, -RmStates:list, -AddedStates:list) is nondet
% eventTypeSpec(?EventType:compound_term, ?OcurrenceTime:number, -EffectTriggers:list, -RmStates:list, -AddedStates:list) is nondet

:- use_module('utils/apply').
:- use_module('utils/lists').
:- use_module('utils/assertRuntimeTerms').
:- use_module('utils/set').
:- use_module(state).
:- use_module(trigger).

:- module_transparent([beginEventTypesDefinition/0, endEventTypesDefinition/0, beginEventsDefinition/0, endEventsDefinition/0]).

:- dynamic eventTypeSpec/7.
:- dynamic eventSpec/2.

beginEventTypesDefinition :-
  beginAssertRuntimeTerms(event, [eventTypeSpec/7]).

endEventTypesDefinition :-
  endAssertRuntimeTerms.

beginEventsDefinition :-
  beginAssertRuntimeTerms(event, [eventSpec/2]).

endEventsDefinition :-
  endAssertRuntimeTerms.

event(EventSignature, ) :-
  eventSpec(). 

eventType(Name, StCond, PlCond, TrgCond, TrgEff, RmSt, AddSt) :-
  eventTypeSpec(Name, StCond, PlCond, TrgCond, TrgEff, RmSt, AddSt).

eventTypeToActionSpec(EventSpecTerm, ActionSpecTerm) :-
  EventSpecTerm =.. [eventTypeSpec, EventName, StateConditions, PrologConditions, _, _, Retractions, Assertions],
  maplist(instancedState, Retractions, InstancedRetractions),
  maplist(instancedState, Assertions, InstancedAssertions),
  append([InstancedRetractions, InstancedAssertions, PrologConditions], MorePrologConditions),
  ActionSpecTerm =.. [actionSpec, EventName, [], [], StateConditions, MorePrologConditions, [], Retractions, Assertions].

% Events
eventTypesTriggeredBy(NamesEventTypes, TrgType) :-
  findall(
      EventName,
    (
      eventTypeSpec(EventName, _, _, TrgConds, _, _, _),
      elementOf(TrgType, TrgConds)
    ),
    NamesEventTypes
  ).

createAndExecuteEvent(CurrentTime, EventTypeName) :-
  eventTypeSpec(EventTypeName, _, _, _, EffTrgs, RmStates, AddStates),
  maplist(deltaToAbsTriggerTime(CurrentTime), EffTrgs, NewTrgs),
  addTriggers(NewTrgs),
  removeStates(RmStates),
  addStates(AddStates),
  assert(eventSpec(EventTypeName, CurrentTime)).

deltaToAbsTriggerTime(CurrentTime, DeltaTrg, AbsTrg) :-
  DeltaTrg =.. [Trg, Delta],
  AbsTime is CurrentTime + Delta,
  AbsTrg =.. [Trg, AbsTime].

instancedState(Assertion, event:ignoreNonDet(respectsSignature(Assertion))).

ignoreNonDet(Goal) :-
  Goal.
ignoreNonDet(Goal) :-
  \+ Goal.