:- module(eventProcesser, [beginPlotDefinition/0, endPlotDefinition/0, beginEventProcesser/0, query/2]). 

:- use_module('utils/apply').
:- use_module('utils/lists').
:- use_module('utils/assertRuntimeTerms').
:- use_module('utils/planning').
:- use_module(event).
:- use_module(state).
:- use_module(trigger).

:- module_transparent([beginPlotDefinition/0, endPlotDefinition/0]).

:- dynamic plotSpec/1.
:- dynamic currentPlotPos/1.
:- dynamic currentTime/1.

beginPlotDefinition :-
  beginAssertRuntimeTerms(eventProcesser, [plotSpec/1]).

endPlotDefinition :-
  endAssertRuntimeTerms.

beginEventProcesser :-
  beginDomainDefinition(eventProcesser),
  findall(eventTypeSpec(Name, StCond, PlCond, TrgCond, TrgEff, RmSt, AddSt), eventType(Name, StCond, PlCond, TrgCond, TrgEff, RmSt, AddSt), AllEventTypes),
  maplist(eventTypeToActionSpec, AllEventTypes, AllActionSpecs),
  maplist(assert, AllActionSpecs),
  endDomainDefinition(eventProcesser),
  setCurrentPlotPos(0),
  setCurrentTime(0).

heuristic(_, 5).

query(CurrentTime, TrgLst) :-
  currentTime(LastTime),
  LastTime < CurrentTime,
  setCurrentTime(CurrentTime),
  addTriggers(TrgLst),
  processTriggers(CurrentTime).

processTriggers(_) :-
  \+ nextTrigger(_, _), !. % green cut
processTriggers(MaxTime) :-
  nextTrigger(_, Time),
  Time > MaxTime, !. % green cut
processTriggers(MaxTime) :-
  nextTrigger(Trg, Time),
  Time =< MaxTime,
  popTrigger(_,_),
  eventTypesTriggeredBy(TriggeredEventsNames, Trg),
  ignore(tryExecuteEventForPlot(Time, TriggeredEventsNames, Trg)),
  processTriggers(MaxTime).
  

tryExecuteEventForPlot(Time, TriggeredEventsNames, TrgName) :-
  allStates(States),
  currentPlotPos(PlotPos),
  plotSpec(Plot),
  nth0(PlotPos, Plot, Goal),
  once(
    planAStar(eventProcesser, States, Goal, heuristic, Plan, PlanCost, _, TriggeredEventsNames)
  ),
  (
    Plan = [],
    NewPlotPos is PlotPos + 1,
    setCurrentPlotPos(NewPlotPos),
    tryExecuteEventForPlot(Time, TriggeredEventsNames, TrgName)
  ;
    Plan = [FirstAction|_],
    passiveTrigger(TrgName),
    once(
      planAStar(eventProcesser, States, Goal, heuristic, _, BestPlanCost, _)
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