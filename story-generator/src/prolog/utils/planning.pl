:- use_module(set).

% PLANNER IMPLEMENTATION

% satisfiedGoalsList(goalsList)
satisfiedGoalsList(GoalsList) :-
  maplist(call, GoalsList).

% executeSideEffects(sideEffectsList)
executeSideEffects(SideEffectsList) :-
  maplist(ignore, SideEffectsList).

negation(X, not(X)).
negation(X, \+ X).

isNegation(X) :-
  negation(_, X), !. % green cut

emptyIntersection(SetA, SetB) :-
  setDiff(SetA, SetB, SetA).

separateConditions(Conditions, Inclusions, Exclusions) :-
  partition(isNegation, Conditions, Negations, Inclusions),
  maplist(negation, Exclusions, Negations).

% updatedFacts(facts, removedFacts, addedFacts, NewFacts)
updatedFacts(Facts, RemovedFacts, AddedFacts, NewFacts) :-
  setDiff(Facts, RemovedFacts, DiffFacts),
  setUnion(DiffFacts, AddedFacts, NewFacts).

% allowedActionExecution(Action, facts, NewFacts)
allowedActionExecution(Action, Facts, NewFacts) :-
  actionSpecification(Action, TypeSpecifications, PrologConditions, Conditions, RemovedFacts, AddedFacts, PrologSideEffects),
  satisfiedGoalsList(TypeSpecifications),
  satisfiedGoalsList(PrologConditions),
  separateConditions(Conditions, Inclusions, Exclusions),
  subsetOf(Inclusions, Facts),
  emptyIntersection(Exclusions, Facts),
  updatedFacts(Facts, RemovedFacts, AddedFacts, NewFacts),
  executeSideEffects(PrologSideEffects).

% planExecutionWithForbiddenStates(facts, goals, Plan, forbiddenStates)
planExecutionWithForbiddenStates(Facts, Goals, [], _) :-
  subsetOf(Goals, Facts), !. % red cut
planExecutionWithForbiddenStates(Facts, Goals, [FirstAction|RestOfPlan], ForbiddenStates) :-
  allowedActionExecution(FirstAction, Facts, NewFacts),
  not(setIsOneOf(NewFacts, ForbiddenStates)),
  planExecutionWithForbiddenStates(NewFacts, Goals, RestOfPlan, [NewFacts|ForbiddenStates]).

% planExecution(facts, goals, Plan)
planExecution(Facts, Goals, Plan) :-
  planExecutionWithForbiddenStates(Facts, Goals, Plan, [Facts]).

% PLANNER EXAMPLE

% Type facts
place(a).
place(b).
place(c).

character(monkey).

object(stairs).
object(banana).

% State facts
facts([
  onPos(monkey, a),
  onPos(stairs, b),
  onPos(banana, c),
  onGround(monkey)
  ]).

% actionSpecification(action, typeSpecifications, prologConditions, conditions, removedFacts, addedFacts, prologSideEffects).
actionSpecification(move(X, Y), [place(X), place(Y)], [X \== Y], [onPos(monkey, X), onGround(monkey)], [onPos(monkey, X)], [onPos(monkey, Y)], []).
actionSpecification(carry(X, Y), [place(X), place(Y)], [X \== Y], [onPos(monkey, X), onPos(stairs, X), onGround(monkey)], [onPos(monkey, X), onPos(stairs, X)], [onPos(monkey, Y), onPos(stairs, Y)], []).
actionSpecification(climb(X), [place(X)], [], [onPos(monkey, X), onPos(stairs, X), onGround(monkey)], [onGround(monkey)], [], []).
actionSpecification(win(X), [place(X)], [], [onPos(monkey, X), onPos(banana, X), \+ onGround(monkey)], [], [win], []).

% The following query retrieves the plan:
% facts(X), planExecution(X, [win], Plan).