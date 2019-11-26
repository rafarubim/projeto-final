% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Story definition

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
