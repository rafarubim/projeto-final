:- module(planning, [planExecution/3]).

/**
 * <module> Planning
 * 
 * This module implements a STRIPS-like planner with prolog functionalities. It declares a predicate (planExecution)
 * and interfaces (actionSpecification) for the planning.
 * To use the planner, an ontology similar to STRIPS must be followed to define a planning domain. It is explained
 * below, pointing the PDDL correspondent of each definition.
 * *Elements* are prolog terms
 * The planner works on a set of facts (prolog terms)
 * 
 * 
 * @author Rafael Rubim Cabral
 * @version 0.1.0
 * 
 * Docs model:
 * @see https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/pldoc.html%27)
 **/

/**
 * 
 * Action name: A complex term of any arity, with different variables as arguments.
 *              Represents the name of an action.
 * state: fact set
 * */

:- use_module(set).

% -------------------- External predicate interfaces

% actionSpecification(-Action:complex_term, -TypeSpecifications:list, -PrologConditions:list, -Conditions:list,
%                     -RemovedFacts:list, -AddedFacts:list, -PrologSideEffects:list) is nondet
% @arg Action The name of the action
% @arg TypeSpecifications Set of variable type instantiations
% @arg PrologConditions Sequence of Prolog goals which are conditions for execution of Action
% @arg Conditions Set of conditions of the action
% @arg RemovedActions Facts which are removed from the state as the effect of the action
% @arg AddedActions Facts which are added to the state as the effect of the action
% @arg PrologSideEffects Prolog goals which are executed as a side effect of the action, regardless of their success.
%
% True if all the arguments comply with the specification of the action

% -------------------- Public predicates

% planExecution(++Facts:list, ++Goals:list, ?Plan:list) is nondet
%
% @arg Facts A set of facts.
% @arg Goals A set of goals.
% @arg Plan A sequence of actions that satisfy the Goals after being applied to
%           the Facts.
%
% True if all Goals are satisfied after the sequence of actions in Plan is applied
% to the Facts.
planExecution(Facts, Goals, Plan) :-
  planExecutionWithForbiddenStates(Facts, Goals, Plan, [Facts]).

% -------------------- Private predicates

% negation(?Term:any, ?NegatedTerm:any) is semidet
%
% True if NegatedTerm is a negation of Term.
negation(X, not(X)) :- !. % green cut
negation(X, \+ X).

% isNegation(?Term) is semidet
%
% True if Term is a negation of some other term.
isNegation(X) :-
  negation(_, X).

% emptyIntersection(++SetA, ++SetB) is semidet
%
% True if the intersection of SetA/SetB is empty
emptyIntersection(SetA, SetB) :-
  setDiff(SetA, SetB, SetA).

% satisfiedGoalsList(:GoalsList:list) is nondet
%
% True if GoalsList is a list of satisfied goals (in conjunction).
satisfiedGoalsList(GoalsList) :-
  maplist(call, GoalsList).

% executeSideEffects(:SideEffectsList:list) is det
%
% All goals in SideEffectsList are called, regardless of their success.
executeSideEffects(SideEffectsList) :-
  maplist(ignore, SideEffectsList).

% separatedGoals(+Goals:list, -InclusionGoals:list, -AbscenceGoals:list) is semidet
%
% @arg Goals A set of goals which may or may not be negation terms.
% @arg InclusionGoals A set of non-negation terms obtained from Goals.
% @arg AbscenceGoals A set of non-negation terms which were originally negations in
%                    Goals.
%
% This predicate parses the terms which are and aren't negations in Goals. The ones
% that aren't are returned in -InclusionGoals. The ones that are negations are
% returned in -AbscenceGoals, but WITHOUT the negation itself (eg. "(\+x)" becomes
% "x")
separatedGoals(Goals, InclusionGoals, AbscenceGoals) :-
  partition(isNegation, Goals, Negations, InclusionGoals),
  maplist(negation, AbscenceGoals, Negations).

% updatedFacts(++Facts:list, ++RemovedFacts:list, ++AddedFacts:list, --NewFacts:list) is nondet
%
% @arg Facts A set of facts.
% @arg RemovedFacts A set of facts to be removed.
% @arg AddedFacts A set of facts to be added.
% @arg NewFacts A set of facts obtained from adding/removing from Facts.
%
% True if NewFacts is obtained from removing all RemovedFacts from Facts then adding
% AddedFacts to it. AddedFacts must have a specific order. 
updatedFacts(Facts, RemovedFacts, AddedFacts, NewFacts) :-
  setDiff(Facts, RemovedFacts, DiffFacts),
  setUnion(DiffFacts, AddedFacts, NewFacts).

% allowedActionExecution(?Action:complex_term, ++Facts:list, --NewFacts:list) is nondet
%
% @arg Action The name of an action
% @arg Facts A set of facts.
% @arg NewFacts A set facts obtained after applying Action in Facts.
%
% True if Action can be applied in the set of Facts (conditions are satisfied) and
% NewFacts is the resulting set after applying the Action. NewFacts must have a
% specific order.
allowedActionExecution(Action, Facts, NewFacts) :-
  actionSpecification(Action, TypeSpecifications, PrologConditions, Conditions, RemovedFacts, AddedFacts, PrologSideEffects),
  satisfiedGoalsList(TypeSpecifications),
  satisfiedGoalsList(PrologConditions),
  separatedGoals(Conditions, InclusionConditions, AbscenceConditions),
  subsetOf(InclusionConditions, Facts),
  emptyIntersection(AbscenceConditions, Facts),
  updatedFacts(Facts, RemovedFacts, AddedFacts, NewFacts),
  executeSideEffects(PrologSideEffects).

% planExecutionWithForbiddenStates(++Facts:list, ++Goals:list, ?Plan:list, ++ForbiddenStates:list) is nondet
%
% @arg Facts A set of facts.
% @arg Goals A set of goals.
% @arg Plan A sequence of actions that satisfy the Goals after being applied to
%           the Facts.
% @arg ForbiddenStates A list of states which are states that can't be passed through
%                      when the plan is followed starting from Facts.
%
% True if all Goals are satisfied after the sequence of actions in Plan is applied
% to the Facts, without passing thorugh any state in ForbiddenStates. 
planExecutionWithForbiddenStates(Facts, Goals, [], _) :-
  separatedGoals(Goals, InclusionGoals, AbsenceGoals),
  subsetOf(InclusionGoals, Facts),
  emptyIntersection(AbsenceGoals, Facts), !. % red cut
planExecutionWithForbiddenStates(Facts, Goals, [FirstAction|RestOfPlan], ForbiddenStates) :-
  allowedActionExecution(FirstAction, Facts, NewFacts),
  not(setIsOneOf(NewFacts, ForbiddenStates)),
  planExecutionWithForbiddenStates(NewFacts, Goals, RestOfPlan, [NewFacts|ForbiddenStates]).

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