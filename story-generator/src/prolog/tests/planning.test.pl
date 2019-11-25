:- begin_tests(planning).

:- use_module('../utils/planning.pl').

/**
 * <module> Test Planning
 * 
 * This module tests the planning module.
 * 
 * @author Rafael Rubim Cabral
 * @version 0.1.0
 * 
 * @see Docs model: https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/pldoc.html%27)
 **/

test(beginDomainDefinition) :-
  beginDomainDefinition(locationsDomain).

test(endDomainDefinition) :- 
  % Setup
  assert(type(person)),
  assert(type(location)),
  assert(actionSpec(
    move(P, L1, L2),
    [person(P), location(L1), location(L2)],
    [L1 \== L2],
    [onLocation(P, L1)],
    [],
    [],
    [onLocation(P, L1)], 
    [onLocation(P, L2)]
  )),
  assert(actionSpec(
    playOcarina(P),
    [person(P)],
    [],
    [onLocation(P, hyrule)],
    [],
    [],
    [wantsToPlayOcarina(P)], 
    [happy(P)]
  )),
  % Test
  endDomainDefinition(locationsDomain),
  % Teardown
  retractall(type(_)),
  retractall(actionSpec(_,_,_,_,_,_,_)).

test(beginProblemDefinition) :-
  beginProblemDefinition(locationsDomain).

test(endProblemDefinition) :-
  % Setup
  assert(person(jorge)),
  assert(person(klebson)),
  assert(location(hogwarts)),
  assert(location(araluen)),
  assert(location(hyrule)),
  % Test
  endProblemDefinition(locationsDomain),
  % Teardown
  retractall(person(_)),
  retractall(location(_)).

test(plan, [
      all(
        [
          Plan,
          FinalState
        ]=[
          [
            [move(jorge,hogwarts,araluen), move(jorge,araluen,hyrule)],
            [onLocation(jorge, hyrule)]
          ],
          [
            [move(jorge,hogwarts,hyrule)],
            [onLocation(jorge, hyrule)]
          ]
        ]
      )
    ]) :-
  plan(
    locationsDomain,
    [onLocation(jorge, hogwarts)],
    [onLocation(jorge, hyrule)],
    Plan,
    FinalState
  ).

test(planAStar, [
      all(
        [
          Plan,
          PlanCost,
          FinalState
        ]=[
          [
            [move(jorge,hogwarts,araluen),move(jorge,araluen,hyrule)],
            10,
            [onLocation(jorge,hyrule)]
          ]
        ]
      )
    ]) :-
  % Setup
  assert(heuristic(actionExecution(move(_,hogwarts, hyrule),_), 50) :- !),
  assert(heuristic(_, 5)),
  % Test
  planAStar(
    locationsDomain,
    [onLocation(jorge, hogwarts)],
    [onLocation(jorge, hyrule)],
    heuristic,
    Plan,
    PlanCost,
    FinalState
  ),
  % Teardown
  retractall(heuristic(_, _)).

test(plan, [
      all(
        [
          Plan,
          FinalState,
          HappyPerson
        ]=[
          [
            [move(jorge,hogwarts,araluen), move(jorge,araluen,hyrule), playOcarina(jorge)],
            [onLocation(jorge, hyrule), happy(jorge)],
            jorge
          ],
          [
            [move(jorge,hogwarts,hyrule), playOcarina(jorge)],
            [onLocation(jorge, hyrule), happy(jorge)],
            jorge
          ]
        ]
      )
    ]) :-
  plan(
    locationsDomain,
    [onLocation(jorge, hogwarts)],
    [happy(HappyPerson)],
    Plan,
    FinalState
  ).

test(planAStar, [
      true(
        [
          Plan,
          PlanCost,
          FinalState,
          HappyPerson
        ]=[
          [move(jorge,hogwarts,araluen),move(jorge,araluen,hyrule), playOcarina(jorge)],
          15,
          [onLocation(jorge,hyrule), onLocation(klebson, hogwarts), happy(jorge)],
          jorge
        ]
      )
    ]) :-
  % Setup
  assert(heuristic(actionExecution(move(_,hogwarts, hyrule),_), 50) :- !),
  assert(heuristic(_, 5)),
  % Test
  once(
    planAStar(
      locationsDomain,
      [onLocation(jorge, hogwarts), onLocation(klebson, hogwarts)],
      [happy(HappyPerson)],
      heuristic,
      Plan,
      PlanCost,
      FinalState
    )
  ),
  % Teardown
  retractall(heuristic(_, _)).

test(planAStar, [
      true(
        [
          Plan,
          PlanCost,
          FinalState
        ]=[
          [
            move(klebson,hogwarts,araluen),
            move(jorge,hogwarts,araluen),
            move(klebson,araluen,hyrule),
            move(jorge,araluen,hyrule),
            playOcarina(klebson),
            playOcarina(jorge)
          ],
          30,
          [happy(klebson), onLocation(klebson,hyrule), onLocation(jorge,hyrule), happy(jorge)]
        ]
      )
    ]) :-
  % Setup
  assert(heuristic(actionExecution(move(_,hogwarts, hyrule),_), 50) :- !),
  assert(heuristic(_, 5)),
  % Test
  once(
    planAStar(
      locationsDomain,
      [onLocation(jorge, hogwarts), onLocation(klebson, hogwarts), wantsToPlayOcarina(jorge), wantsToPlayOcarina(klebson)],
      [\+ wantsToPlayOcarina(_)],
      heuristic,
      Plan,
      PlanCost,
      FinalState
    )
  ),
  % Teardown
  retractall(heuristic(_, _)).

test(deleteProblem) :-
  deleteProblem(locationsDomain).

test(beginProblemDefinition) :-
  beginProblemDefinition(locationsDomain).

test(endProblemDefinition) :-
  %Setup
  assert(person(merlin)),
  assert(location(pallet_city)),
  assert(location(grand_line)),
  assert(location(alagaesia)),
  % Test
  endProblemDefinition(locationsDomain),
  % Teardown
  retractall(person(_)),
  retractall(location(_)).

test(plan, [
      all(
        [
          Plan,
          FinalState
        ]=[
          [
            [move(merlin,pallet_city,grand_line), move(merlin,grand_line,alagaesia)],
            [onLocation(merlin,alagaesia)]
          ],
          [
            [move(merlin,pallet_city,alagaesia)],
            [onLocation(merlin,alagaesia)]
          ]
        ]
      )
    ]) :-
  plan(
    locationsDomain,
    [onLocation(merlin, pallet_city)],
    [onLocation(merlin, alagaesia)],
    Plan,
    FinalState
  ).

test(planAStar, [
      all(
        [
          Plan,
          PlanCost,
          FinalState
        ]=[
          [
            [move(merlin,pallet_city,alagaesia)],
            5,
            [onLocation(merlin,alagaesia)]
          ]
        ]
      )
    ]) :-
  % Setup
  assert(heuristic(_, 5)),
  % Test
  planAStar(
    locationsDomain,
    [onLocation(merlin, pallet_city)],
    [onLocation(merlin, alagaesia)],
    heuristic,
    Plan,
    PlanCost,
    FinalState
  ),
  % Teardown
  retractall(heuristic(_,_)).

test(deleteDomain) :- deleteDomain(locationsDomain).

:- end_tests(planning).