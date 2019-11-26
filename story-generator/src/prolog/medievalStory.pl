:- use_module(entity).
:- use_module(enumeration).
:- use_module(state).
:- use_module(event).
:- use_module(trigger).
:- use_module(eventProcesser).

% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Genre Definition

% --------------------------------- Enum types
:- beginEnumsDefinition.

enumSpec(speed, [slow, medium, fast]).

:- endEnumsDefinition.

% --------------------------------- Entity types
:- beginEntityTypesDefinition.

categorySpec(man, character).
categorySpec(woman, character).
categorySpec(prince, man).
categorySpec(horse, animal).
categorySpec(building, place).

:- endEntityTypesDefinition.

% --------------------------------- State types
:- beginStateTypesDefinition.

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

:- endStateTypesDefinition.

% --------------------------------- Trigger types
:- beginTriggerTypesDefinition.

triggerTypeSpec(villainStrikes, active).
triggerTypeSpec(heroActs, active).

:- endTriggerTypesDefinition.

% --------------------------------- Event types

:- beginEventTypesDefinition.

eventTypeSpec(
  kidnap(Kidnapper, Kidnapped, Plc),
  [standsIn(Kidnapper, Plc), standsIn(Kidnapped, Plc)],
  [Kidnapper \== Kidnapped, Kidnapper \== cassandra],
  [villainStrikes],
  [],
  [],
  [kidnappedBy(Kidnapped, Kidnapper)]
).

:- endEventTypesDefinition.

% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Story definition

% --------------------------------- Entities
:- beginEntitiesDefinition.

entitySpec(cassandra, woman).
entitySpec(horace, prince).
entitySpec(capital, place).
entitySpec(palace, building).
entitySpec(morgarath, man).

:- endEntitiesDefinition.

% --------------------------------- States
:- beginStatesDefinition.

builtIn(palace, capital).
isKnight(cassandra).
standsIn(horace, palace).
standsIn(cassandra, palace).
standsIn(morgarath, capital).

:- endStatesDefinition.

% --------------------------------- Triggers
:- beginTriggersDefinition.

villainStrikes(10).

:- endTriggersDefinition.

% --------------------------------- Plot
:- beginPlotDefinition.

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

:- endPlotDefinition.

:- beginEventProcesser.

% planToPlot(Plan).