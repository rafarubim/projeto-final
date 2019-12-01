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
triggerTypeSpec(villainStrikes, passive).
triggerTypeSpec(heroActs, passive).
triggerTypeSpec(fight, active).

% --------------------------------- Event types
eventTypeSpec(
  kidnap(Kidnapper, Kidnapped, Plc),
  [standsIn(Kidnapper, Plc), standsIn(Kidnapped, Plc)],
  [entityClassification(Kidnapper, character), entityClassification(Kidnapped, character), Kidnapper \== Kidnapped, Kidnapper \== cassandra],
  [villainStrikes],
  [],
  [],
  [kidnappedBy(Kidnapped, Kidnapper)]
).
eventTypeSpec(
  defeat(Defeater, Defeated, Plc),
  [standsIn(Defeater, Plc), standsIn(Defeated, Plc)],
  [entityClassification(Defeater, character), entityClassification(Defeated, character), Defeater \== Defeated],
  [heroStrikes],
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
tick(0).
villainStrikes(10).

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
