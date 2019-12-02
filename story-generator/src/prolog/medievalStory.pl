:- use_module(index).

% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Genre Definition

:- beginRemoveNativeEntityTypes.
:- beginRemoveNativeStateTypes.
:- beginRemoveNativeEventTypes.
:- beginEnumsDefinition.
:- beginEntityTypesDefinition.
:- beginStateTypesDefinition.
:- beginTriggerTypesDefinition.
:- beginEventTypesDefinition.

% --------------------------------- Remove native types

removeNativeEntityType(animal).

removeNativeStateType(knowsThat).
removeNativeStateType(thinksThat).

% --------------------------------- Enum types



% --------------------------------- Entity types
categorySpec(man, character).
categorySpec(woman, character).
categorySpec(prince, man).
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
triggerTypeSpec(defeat, active).

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
  [defeat(0)],
  [],
  [defeatedBy(Defeated, Defeater)]
).
eventTypeSpec(
  save(Savior, Saved, Plc),
  [standsIn(Savior, Plc), standsIn(Saved, Plc), kidnappedBy(Saved, Kidnapper), defeatedBy(Kidnapper, Savior)],
  [entityClassification(Savior, character), entityClassification(Saved, character), Savior \== Saved, Kidnapper \== Saved, Kidnapper \== Savior],
  [defeat],
  [defeat(0)],
  [kidnappedBy(Saved, Kidnapper)],
  [savedBy(Saved, Savior)]
).

:- endRemoveNativeEntityTypes.
:- endRemoveNativeStateTypes.
:- endRemoveNativeEventTypes.
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
standsIn(crown, capital).
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
