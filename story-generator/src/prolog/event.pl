:- module(event, [beginEventTypesDefinition/0, endEventTypesDefinition/0, beginEventsDefinition/0, endEventsDefinition/0, eventTypeToActionSpec/2, eventType/7, eventTypesTriggeredBy/2, createAndExecuteEvent/2, event/2]).

% Event type name
% State Conditions
% Prolog Conditions
% Trigger Conditions
% Effect Triggers
% Removed States
% Added States

eventTypeSpec(
  move(Char, Plc1, Plc2),
  [standsIn(Char, Plc1)],
  [entityClassification(Char, character), Plc1 \== Plc2],
  [tick],
  [tick(1), motion(0)],
  [standsIn(Char, Plc1)],
  [standsIn(Char, Plc2)]
).
eventTypeSpec(
  carry(Char, Thing, Plc1, Plc2),
  [standsIn(Char, Plc2), standsIn(Thing, Plc1), isHolding(Char, Thing)],
  [entityClassification(Char, character), entityClassification(Thing, thing), Plc1 \== Plc2],
  [motion],
  [motion(0)],
  [standsIn(Thing, Plc1)],
  [standsIn(Thing, Plc2)]
).
eventTypeSpec(
  give(Char1, Char2, Thing, Plc),
  [standsIn(Char1, Plc), standsIn(Char2, Plc), standsIn(Thing, Plc), isHolding(Char1, Thing)],
  [entityClassification(Char1, character), entityClassification(Char2, character), entityClassification(Thing, thing), Char1 \== Char2],
  [tick],
  [tick(1)],
  [isHolding(Char1, Thing)],
  [isHolding(Char2, Thing)]
).

% eventTypeSpec(?EventTypeName:compound_term, -StateConditions:list, -PrologConditions:list, -TriggerConditions:list, -EffectTriggers:list, -RmStates:list, -AddedStates:list) is nondet
% eventTypeSpec(?EventType:compound_term, ?OcurrenceTime:number, -EffectTriggers:list, -RmStates:list, -AddedStates:list) is nondet

:- use_module('utils/apply').
:- use_module('utils/lists').
:- use_module('utils/assertRuntimeTerms').
:- use_module('utils/set').
:- use_module(entity).
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

event(EventSignature, OcurrenceTime) :-
  eventSpec(EventSignature, OcurrenceTime). 

eventType(Name, StCond, PlCond, TrgCond, TrgEff, RmSt, AddSt) :-
  eventTypeSpec(Name, StCond, PlCond, TrgCond, TrgEff, RmSt, AddSt).

eventTypeToActionSpec(EventSpecTerm, ActionSpecTerm) :-
  EventSpecTerm =.. [eventTypeSpec, EventName, StateConditions, PrologConditions, _, _, Retractions, Assertions],
  maplist(instancedState, Retractions, InstancedRetractions),
  maplist(instancedState, Assertions, InstancedAssertions),
  maplist(eventContextTerm, PrologConditions, EventContextPrologConditions),
  append([InstancedRetractions, InstancedAssertions, EventContextPrologConditions], MorePrologConditions),
  ActionSpecTerm =.. [actionSpec, EventName, [], [], StateConditions, MorePrologConditions, [], Retractions, Assertions].

eventContextTerm(Term, event:Term).

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