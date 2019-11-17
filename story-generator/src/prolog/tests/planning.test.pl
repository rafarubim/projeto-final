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

test(beginDomainDefinition) :- beginDomainDefinition(locationsDomain).

test(domainDefinition) :-
  assert(type(person)),
  assert(type(location)),
  assert(actionSpec(
    move(P, L1, L2),
    [person(P), location(L1), location(L2)],
    [L1 \== L2],
    [onLocation(P, L1)],
    [onLocation(P, L1)], 
    [onLocation(P, L2)],  
    []
  )).

test(endDomainDefinition) :- endDomainDefinition(locationsDomain).

test(beginProblemDefinition) :- beginProblemDefinition(locationsDomain).

test(problemDefinition) :-
  assert(person(jorge)),
  assert(location(hogwarts)),
  assert(location(araluen)),
  assert(location(hyrule)).

test(endProblemDefinition) :- endProblemDefinition(locationsDomain).

test(planExecution, [all([Plan,FinalState]=[[move(jorge,hogwarts,hyrule)]])]) :-
  planExecution(
    locationsDomain,
    [onLocation(jorge, hogwarts)],
    [onLocation(jorge, hyrule)],
    Plan,
    FinalState
  ).

test(deleteProblem) :- deleteProblem(locationsDomain).

test(beginProblemDefinition) :- beginProblemDefinition(locationsDomain).

test(problemDefinition) :-
  assert(person(merlin)),
  assert(location(pallet_city)),
  assert(location(grand_line)),
  assert(location(alagaesia)).

test(endProblemDefinition) :- endProblemDefinition(locationsDomain).

test(planExecution, [all([Plan,FinalState]=[[move(merlin,pallet_city,alagaesia)]])]) :-
  planExecution(
    locationsDomain,
    [onLocation(merlin, pallet_city)],
    [onLocation(merlin, alagaesia)],
    Plan,
    FinalState
  ).

test(deleteDomain) :- deleteDomain(locationsDomain).

:- end_tests(planning).