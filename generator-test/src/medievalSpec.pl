:- use_module(index).

% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Genre Definition

% --------------------------------- Remove native types

removeNativeEntityType(animal).

removeNativeStateType(knowsThat).
removeNativeStateType(thinksThat).

% --------------------------------- Enum types

enumSpec(speed, [slow, medium, fast]).

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
