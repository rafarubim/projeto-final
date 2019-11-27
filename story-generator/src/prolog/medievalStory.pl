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
signatureSpec(builtIn, [
  entityArg(building),
  entityArg(place)
]).
signatureSpec(isKnight, [
  entityArg(character)
]).
signatureSpec(kidnappedBy, [
  entityArg(character),
  entityArg(character)
]).
signatureSpec(defeatedBy, [
  entityArg(character),
  entityArg(character)
]).
signatureSpec(savedBy, [
  entityArg(character),
  entityArg(character)
]).

% --------------------------------- Trigger types
triggerTypeSpec(villainStrikes, active).
triggerTypeSpec(heroActs, active).

% --------------------------------- Event types
eventTypeSpec(
  kidnap(Kidnapper, Kidnapped, Plc),
  [standsIn(Kidnapper, Plc), standsIn(Kidnapped, Plc)],
  [Kidnapper \== Kidnapped, Kidnapper \== cassandra],
  [villainStrikes],
  [],
  [],
  [kidnappedBy(Kidnapped, Kidnapper)]
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
entitySpec(palace, building).
entitySpec(morgarath, man).

% --------------------------------- States
builtIn(palace, capital).
isKnight(cassandra).
standsIn(horace, palace).
standsIn(cassandra, palace).
standsIn(morgarath, capital).

% --------------------------------- Triggers
villainStrikes(10).

% --------------------------------- Plot
plotSpec([
  [
    kidnappedBy(horace, Villain)
  ],
  [
    kidnappedBy(cassandra, Villain)
  ],
  [
    \+ kidnappedBy(_,_),
    defeatedBy(Villain, _),
    savedBy(horace, cassandra)
  ]
]).

% --------------------------------- Heuristic predicate

heuristicPredicateSpec(_, 5).

:- endEntitiesDefinition.
:- endStatesDefinition.
:- endTriggersDefinition.
:- endEventsDefinition.
:- endPlotDefinition.
:- endHeuristicPredicateDefinition.

:- beginEventProcesser.