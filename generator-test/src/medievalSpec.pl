% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Genre Definition

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
