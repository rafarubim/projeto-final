:- use_module(set).

facts([
  pos(a),
  pos(b),
  pos(c),
  onPos(monkey, a),
  onPos(stairs, b),
  onPos(banana, c),
  onGround(monkey)
  ]).

% updatedFacts(facts, removedFacts, addedFacts, NewFacts)
updatedFacts(Facts, RemovedFacts, AddedFacts, NewFacts) :-
  setDiff(Facts, RemovedFacts, DiffFacts),
  setUnion(DiffFacts, AddedFacts, NewFacts).

% allowedActionExecution(Action, facts, NewFacts)
allowedActionExecution(Action, Facts, NewFacts) :-
  actionSpecification(Action, Conditions, RemovedFacts, AddedFacts),
  subsetOf(Conditions, Facts),
  updatedFacts(Facts, RemovedFacts, AddedFacts, NewFacts).

% planExecutionWithForbiddenStates(facts, goals, Plan, forbiddenStates)
planExecutionWithForbiddenStates(Facts, Goals, [], _) :-
  subsetOf(Goals, Facts).
planExecutionWithForbiddenStates(Facts, Goals, [FirstAction|RestOfPlan], ForbiddenStates) :-
  allowedActionExecution(FirstAction, Facts, NewFacts),
  not(setIsOneOf(NewFacts, ForbiddenStates)),
  planExecutionWithForbiddenStates(NewFacts, Goals, RestOfPlan, [NewFacts|ForbiddenStates]).

% planExecution(facts, goals, Plan)
planExecution(Facts, Goals, Plan) :-
  planExecutionWithForbiddenStates(Facts, Goals, Plan, []).

% actionSolution(facts, goals, Action)
actionSolution(Facts, Goals, Action) :-
  allowedActionExecution(Action, Facts, NewFacts),
  subsetOf(Goals, NewFacts).

actionSpecification(move(Y), [onPos(monkey, X), pos(Y), X \== Y], [onPos(monkey, X)], [onPos(monkey, Y)]).
%actionSpecification(carry(Y), [onPos(monkey, X), onPos(stairs, X)], [onPos(monkey, X), onPos(stairs, X)], [onPos(monkey, Y), onPos(stairs, Y)]).
%actionSpecification(win, [onPos(monkey, X), onPos(banana, X), \+ onGround(mokey)], [], [win]).