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
