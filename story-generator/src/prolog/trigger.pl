:- module(trigger, [beginTriggerTypesDefinition/0, endTriggerTypesDefinition/0, beginTriggersDefinition/0, endTriggersDefinition/0, addTriggers/1, nextTrigger/2, popTrigger/2, triggerTerm/3, passiveTrigger/1]).

% triggerSpec(?TriggerName:atom) is nondet
%
% True if TriggerName is a trigger.

:- use_module('utils/assertRuntimeTerms').

:- module_transparent([beginTriggerTypesDefinition/0, endTriggerTypesDefinition/0, beginTriggersDefinition/0, endTriggersDefinition/0]).

:- dynamic triggerTypeSpec/2.
:- dynamic triggerHeap/1.

:- empty_heap(Heap), assert(triggerHeap(Heap)).

triggerTypeSpec(tick, passive).

beginTriggerTypesDefinition :-
  beginAssertRuntimeTerms(trigger, [triggerTypeSpec/2]).

endTriggerTypesDefinition :-
  endAssertRuntimeTerms.

beginTriggersDefinition :-
  findall(TrgName/1,(trigger:triggerTypeSpec(TrgName, _), (dynamic TrgName/1)),TrgPredicateDescs),
  beginAssertRuntimeTerms(trigger, TrgPredicateDescs).

endTriggersDefinition :-
  endAssertRuntimeTerms,
  findall(
    TrgTerm,
    (
      trigger:triggerTypeSpec(TrgName, _),
      TrgTerm =.. [TrgName, _],
      TrgTerm
    ),
    AllTrgs
  ),
  addTriggers(AllTrgs).

addTriggers(TrgLst) :-
  triggerHeap(Heap), !, % green cut
  addTriggersToHeap(Heap, TrgLst, NewHeap),
  setTriggerHeap(NewHeap), !.

nextTrigger(TrgName, Time) :-
  triggerHeap(Heap),
  min_of_heap(Heap, Time, TrgName).

popTrigger(TrgName, Time) :-
  triggerHeap(Heap),
  get_from_heap(Heap, Time, TrgName, NewHeap),
  setTriggerHeap(NewHeap).

passiveTrigger(TrgName) :-
  triggerTypeSpec(TrgName, passive).

triggerTerm(TrgTerm, TrgName, Time) :-
  TrgTerm =.. [TrgName, Time].

setTriggerHeap(Heap) :-
  retractall(triggerHeap(_)),
  assert(triggerHeap(Heap)).

addTriggersToHeap(Heap, [], Heap).
addTriggersToHeap(Heap, [Trigger|OtherTriggers], NewHeap) :-
  Trigger =.. [TrgName, Time],
  add_to_heap(Heap, Time, TrgName, MidHeap),
  addTriggersToHeap(MidHeap, OtherTriggers, NewHeap).
  