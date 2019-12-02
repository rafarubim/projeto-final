% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Story definition

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
