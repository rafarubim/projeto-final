:- module(eventProcesser, [beginPlotDefinition/0, endPlotDefinition/0, beginHeuristicPredicateDefinition/0, endHeuristicPredicateDefinition/0, beginEventProcesser/0, query/3, createAndExecuteEventNow/1]). 

:- use_module('utils/apply').
:- use_module('utils/lists').
:- use_module('utils/assertRuntimeTerms').
:- use_module('utils/planning').
:- use_module(event).
:- use_module(state).
:- use_module(trigger).

:- module_transparent([beginPlotDefinition/0, endPlotDefinition/0, beginHeuristicPredicateDefinition/0, endHeuristicPredicateDefinition/0]).

:- dynamic plotSpec/1.
:- dynamic heuristicPredicateSpec/2.
:- dynamic currentPlotPos/1.
:- dynamic currentTime/1.

beginPlotDefinition :-
  beginAssertRuntimeTerms(eventProcesser, [plotSpec/1]).

endPlotDefinition :-
  endAssertRuntimeTerms.

beginHeuristicPredicateDefinition :-
  beginAssertRuntimeTerms(eventProcesser, [heuristicPredicateSpec/2]).

endHeuristicPredicateDefinition :-
  endAssertRuntimeTerms.

beginEventProcesser :-
  beginDomainDefinition(eventProcesser),
  findall(eventTypeSpec(Name, StCond, PlCond, TrgCond, TrgEff, RmSt, AddSt), eventType(Name, StCond, PlCond, TrgCond, TrgEff, RmSt, AddSt), AllEventTypes),
  maplist(eventTypeToActionSpec, AllEventTypes, AllActionSpecs),
  maplist(assert, AllActionSpecs),
  endDomainDefinition(eventProcesser),
  setCurrentPlotPos(0),
  setCurrentTime(0).

query(CurrentTime, TrgLst, TriggeredEvents) :-
  currentTime(LastTime),
  LastTime < CurrentTime,
  setCurrentTime(CurrentTime),
  addTriggers(TrgLst),
  processTriggers(CurrentTime, TriggeredEvents).

createAndExecuteEventNow(EventTypeName) :-
  currentTime(CurrentTime),
  createAndExecuteEvent(CurrentTime, EventTypeName).

processTriggers(_, []) :-
  \+ nextTrigger(_, _), !. % green cut
processTriggers(MaxTime, []) :-
  nextTrigger(_, Time),
  Time > MaxTime, !. % green cut
processTriggers(MaxTime, TriggeredEvents) :-
  nextTrigger(Trg, Time),
  Time =< MaxTime,
  popTrigger(_,_),
  eventTypesTriggeredBy(TriggeredEventsNames, Trg),
  (
    tryExecuteEventForPlot(Time, TriggeredEventsNames, Trg, TriggeredEvent),
    TriggeredEvents = [TriggeredEvent|NextTriggeredEvents],
    processTriggers(MaxTime, NextTriggeredEvents),
    ! % red cut
  ;
    processTriggers(MaxTime, TriggeredEvents)
  ).
  

tryExecuteEventForPlot(Time, TriggeredEventsNames, TrgName, event(FirstAction, Time)) :-
  allStates(States),
  currentPlotPos(PlotPos),
  plotSpec(Plot),
  nth0(PlotPos, Plot, Goal),
  once(
    planAStar(eventProcesser, States, Goal, eventProcesser:heuristicPredicateSpec, Plan, PlanCost, _, TriggeredEventsNames)
  ),
  (
    Plan = [],
    NewPlotPos is PlotPos + 1,
    setCurrentPlotPos(NewPlotPos),
    tryExecuteEventForPlot(Time, TriggeredEventsNames, TrgName, event(FirstAction, Time))
  ;
    Plan = [FirstAction|_],
    passiveTrigger(TrgName),
    once(
      planAStar(eventProcesser, States, Goal, eventProcesser:heuristicPredicateSpec, _, BestPlanCost, _)
    ),
    PlanCost =< BestPlanCost,
    createAndExecuteEvent(Time, FirstAction)
  ;
    Plan = [FirstAction|_],
    \+ passiveTrigger(TrgName),
    createAndExecuteEvent(Time, FirstAction)
  ).

setCurrentPlotPos(Pos) :-
  retractall(currentPlotPos(_)),
  assert(currentPlotPos(Pos)).

setCurrentTime(Time) :-
  retractall(currentTime(_)),
  assert(currentTime(Time)).