:- use_module(index).

% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Genre Definition

:- beginEnumsDefinition.
:- beginEntityTypesDefinition.
:- beginStateTypesDefinition.
:- beginTriggerTypesDefinition.
:- beginEventTypesDefinition.

% --------------------------------- Enum types

enumSpec(speed, [slow, medium, fast]).

% --------------------------------- Entity types
categorySpec(man, character).
categorySpec(woman, character).
categorySpec(prince, man).
categorySpec(horse, animal).
categorySpec(building, place).

% --------------------------------- State types
stateTypeSpec(builtIn, [
  entityArg(building),
  entityArg(place)
]).
stateTypeSpec(isKnight, [
  entityArg(character)
]).
stateTypeSpec(kidnappedBy, [
  entityArg(character),
  entityArg(character)
]).
stateTypeSpec(defeatedBy, [
  entityArg(character),
  entityArg(character)
]).
stateTypeSpec(savedBy, [
  entityArg(character),
  entityArg(character)
]).

% --------------------------------- Trigger types
triggerTypeSpec(villainActs, passive).
triggerTypeSpec(heroActs, passive).
triggerTypeSpec(fight, active).

% --------------------------------- Event types
eventTypeSpec(
  kidnap(Kidnapper, Kidnapped, Plc),
  [standsIn(Kidnapper, Plc), standsIn(Kidnapped, Plc)],
  [entityClassification(Kidnapper, character), entityClassification(Kidnapped, character), Kidnapper \== Kidnapped, Kidnapper == morgarath],
  [villainActs],
  [tick(1)],
  [],
  [kidnappedBy(Kidnapped, Kidnapper)]
).
eventTypeSpec(
  defeat(Defeater, Defeated, Plc),
  [standsIn(Defeater, Plc), standsIn(Defeated, Plc), \+ kidnappedBy(Defeater, Defeated)],
  [entityClassification(Defeater, character), entityClassification(Defeated, character), Defeater \== Defeated],
  [heroActs],
  [fight(0)],
  [],
  [defeatedBy(Defeated, Defeater)]
).
eventTypeSpec(
  save(Savior, Saved, Plc),
  [standsIn(Savior, Plc), standsIn(Saved, Plc), kidnappedBy(Saved, Kidnapper), defeatedBy(Kidnapper, Savior)],
  [entityClassification(Savior, character), entityClassification(Saved, character), Savior \== Saved, Kidnapper \== Saved, Kidnapper \== Savior],
  [fight],
  [fight(0)],
  [kidnappedBy(Saved, Kidnapper)],
  [savedBy(Saved, Savior)]
).

:- endEnumsDefinition.
:- endEntityTypesDefinition.
:- endStateTypesDefinition.
:- endTriggerTypesDefinition.
:- endEventTypesDefinition.

% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Story definition

:- beginEntitiesDefinition.
:- beginStatesDefinition.
:- beginTriggersDefinition.
:- beginEventsDefinition.
:- beginPlotDefinition.
:- beginHeuristicPredicateDefinition.

% --------------------------------- Entities
entitySpec(cassandra, woman).
entitySpec(horace, prince).
entitySpec(capital, place).
entitySpec(forest, place).
entitySpec(palace, building).
entitySpec(morgarath, man).
entitySpec(crown, thing).

% --------------------------------- States
builtIn(palace, capital).
isKnight(cassandra).
standsIn(horace, palace).
standsIn(cassandra, capital).
standsIn(morgarath, forest).
standsIn(crown, palace).
isHolding(cassandra, crown).

% --------------------------------- Triggers
tick(1).
villainActs(10).
heroActs(20).

% --------------------------------- Plot
plotSpec([
  [
    isHolding(horace, crown)
  ],
  [
    kidnappedBy(horace, Villain)
  ],
  [
    standsIn(cassandra, forest)
  ],
  [
    defeatedBy(Villain, Savior),
    savedBy(horace, Savior)
  ],
  [
    \+ kidnappedBy(_,_),
    defeatedBy(Villain, _),
    savedBy(horace, cassandra)
  ]
]).

% --------------------------------- Heuristic predicate

heuristicPredicateSpec(_, 50).

:- endEntitiesDefinition.
:- endStatesDefinition.
:- endTriggersDefinition.
:- endEventsDefinition.
:- endPlotDefinition.
:- endHeuristicPredicateDefinition.

:- beginEventProcesser.
% allStates(S), member(standsIn(X,Y),S).
% allStates(States), eventProcesser:plotSpec(Plot), nth0(1, Plot, Goal), eventProcesser:planAStar(eventProcesser, States, Goal, eventProcesser:heuristicPredicateSpec, Plan, PlanCost, _).
% allStates(States), eventProcesser:plotSpec(Plot), nth0(1, Plot, Goal), planning:setTempActions(eventProcesser, [kidnap(_,_,_)]), planning:allowedActionExecution(eventProcesser, Action, States, ObtainedFacts), planning:revertTempActions(eventProcesser).
% eventProcesser:allStates(S), eventProcesser:plotSpec(Plot), eventProcesser:currentPlotPos(Pos), eventProcesser:nth0(Pos, Plot, Goal), eventProcesser:planAStar(eventProcesser, S, Goal, eventProcesser:heuristicPredicateSpec, Plan, PlanCost, _, [kidnap(_,_,_)]).