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

heuristicPredicateSpec(actionExecution(move(_,_,_),_), 10) :-
  !.
heuristicPredicateSpec(actionExecution(carry(_,_,_,_),_), Cost) :-
  random(R), Cost is R*10, !.
heuristicPredicateSpec(actionExecution(_,States), 15) :-
  member(isHolding(cassandra, crown), States), !.
heuristicPredicateSpec(_, 50).
