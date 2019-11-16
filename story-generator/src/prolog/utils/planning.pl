:- module(planning, [planExecution/4, beginDomainDefinition/1, endDomainDefinition/1, beginProblemDefinition/1, endProblemDefinition/1]).

/**
 * <module> Planning
 * 
 *   This module implements a STRIPS-like planner with prolog functionalities. It declares a predicate (planExecution)
 * and interfaces (actionSpec) for the planning.
 * To use the planner, an ontology similar to STRIPS must be followed to define a planning domain. It is explained
 * below, pointing the PDDL correspondent of each definition.
 * 
 * *Types* are domain types, similar to PDDL types
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

% -------------------- External predicate interfaces

% actionSpec(-Action:compound_term, -TypeSpecs:list, -PrologConditions:list, -Conditions:list,
%                     -RemovedFacts:list, -AddedFacts:list, -PrologSideEffects:list) is nondet
% @arg Action The name of the action
% @arg TypeSpecs Set of variable type instantiations
% @arg PrologConditions Sequence of Prolog goals which are conditions for execution of Action
% @arg Conditions Set of conditions of the action
% @arg RemovedActions Facts which are removed from the state as the effect of the action
% @arg AddedActions Facts which are added to the state as the effect of the action
% @arg PrologSideEffects Prolog goals which are executed as a side effect of the action, regardless of their success.
%
% True if all the arguments comply with the specification of a user-defined action

% type(-Type:atom) is nondet
% @arg Type Name of the type
%
% True if Type is a user-defined type

% -------------------- End of external predicate interfaces

:- use_module(set).
:- use_module(getRuntimeClauses).

:- module_transparent([beginDomainDefinition/1]).
:- module_transparent([beginProblemDefinition/1]).

% actionSpec(++Namespace:atom, -Action:compound_term, -TypeSpecs:list, -PrologConditions:list, -Conditions:list,
%                     -RemovedFacts:list, -AddedFacts:list, -PrologSideEffects:list) is nondet
%
% Equivalent to the "external predicate" actionSpec/7, but with a "Namespace" atom as first argument.
:- dynamic actionSpec/8.

% type(++Namespace:atom, -Type:atom) is nondet
%
% Equivalent to the "external predicate" type/1, but with a "Namespace" atom as first argument.
:- dynamic actionSpec/8.

% -------------------- Public predicates

beginDomainDefinition(Namespace) :-
  % Capture "actionSpec" definitions
  namespaceOfActionSpec(Namespace, ActionSpecNamespace),
  beginGetRuntimeClauses(ActionSpecNamespace, actionSpec(_,_,_,_,_,_,_), true),
  % Capture "type" definitions
  namespaceOfType(Namespace, TypeNamespace),
  beginGetRuntimeClauses(TypeNamespace, type(_), true).

endDomainDefinition(Namespace) :-
  % Assert local "actionSpec" predicates with a Namespace
  namespaceOfActionSpec(Namespace, ActionSpecNamespace),
  endGetRuntimeClauses(ActionSpecNamespace, ActionSpecClauses),
  length(ActionSpecClauses, ActionSpecLength),
  findall(Namespace, between(1,ActionSpecLength,_), ActionSpecNamespaces),
  maplist(planning:clauseWithNamespace, ActionSpecNamespaces, ActionSpecClauses, ActionSpecClausesWithNamespace),
  maplist(planning:assertClause, ActionSpecClausesWithNamespace),
  % Assert local "type" predicates with a Namespace
  namespaceOfType(Namespace, TypeNamespace),
  endGetRuntimeClauses(TypeNamespace, TypeClauses),
  length(TypeClauses, TypeLength),
  findall(Namespace, between(1,TypeLength,_), TypeNamespaces),
  maplist(planning:clauseWithNamespace, TypeNamespaces, TypeClauses, TypeClausesWithNamespace),
  maplist(planning:assertClause, TypeClausesWithNamespace).

beginOneTypeFunctor(Context, Namespace) :-
  type(Namespace, TypeFunctor),
  namespaceOfTypeFunctor(TypeFunctor, Namespace, TypeFunctorNamespace),
  TypeHeadTerm =.. [TypeFunctor, _],
  beginGetRuntimeClauses(TypeFunctorNamespace, Context:TypeHeadTerm, _).

endOneTypeFunctor(Namespace) :-
  type(Namespace, TypeFunctor),
  namespaceOfTypeFunctor(TypeFunctor, Namespace, TypeFunctorNamespace),
  endGetRuntimeClauses(TypeFunctorNamespace, TypeFunctorClauses),
  length(TypeFunctorClauses, TypeFunctorLength),
  findall(Namespace, between(1,TypeFunctorLength,_), TypeFunctorNamespaces),
  maplist(planning:clauseWithNamespace, TypeFunctorNamespaces, TypeFunctorClauses, TypeFunctorClausesWithNamespace),
  maplist(planning:assertClause, TypeFunctorClausesWithNamespace).

beginProblemDefinition(Namespace) :-
  context_module(Context),
  % Capture all type functor definitions
  findall(_, planning:beginOneTypeFunctor(Context, Namespace), _).

endProblemDefinition(Namespace) :-
  % Assert local type functor predicates with a Namespace
  findall(_, planning:endOneTypeFunctor(Namespace), _).

% planExecution(++Namespace:atom, ++Facts:list, ++Goals:list, ?Plan:list) is nondet
%
% @arg Namespace This could be any atom. The Namespace used here should match the
%                one used in "beginPlanning" and "endPlanning".
% @arg Facts A set of facts.
% @arg Goals A set of goals.
% @arg Plan A sequence of actions that satisfy the Goals after being applied to
%           the Facts.
%
% True if all Goals are satisfied after the sequence of actions in Plan is applied
% to the Facts.
planExecution(Namespace, Facts, Goals, Plan) :-
  planExecutionWithForbiddenStates(Namespace, Facts, Goals, Plan, [Facts]).

% -------------------- Private predicates

namespaceOfActionSpec(Namespace, ActionSpecNamespace) :-
  atom_concat('actionSpec--', Namespace, ActionSpecNamespace).

namespaceOfType(Namespace, TypeNamespace) :-
  atom_concat('type--', Namespace, TypeNamespace).

namespaceOfTypeFunctor(TypeFunctor, Namespace, TypeFunctorNamespace) :-
  atomic_list_concat([TypeFunctor, '--', Namespace], TypeFunctorNamespace).

clauseWithNamespace(Namespace, Head:-Body, NewHead:-Body) :-
  Head =.. [Functor|Args],
  NewHead =.. [Functor,Namespace|Args].

termWithNamespace(Namespace, Term, TermWithNamespace) :-
  Term =.. [Functor|Args],
  TermWithNamespace =.. [Functor,Namespace|Args].

assertClause(Clause) :-
  assert(Clause).

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

% separatedGoals(+Goals:list, -InclusionGoals:list, -AbscenceGoals:list) is det
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

% updatedFacts(++Facts:list, ++RemovedFacts:list, ++AddedFacts:list, -NewFacts:list) is multi
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

typeSpecsWithNamespace(Namespace, TypeSpecs, TypeSpecsWithNamespace) :-
  length(TypeSpecs, TypeSpecsLength),
  findall(Namespace, between(1, TypeSpecsLength, _), Namespaces),
  maplist(termWithNamespace, Namespaces, TypeSpecs, TypeSpecsWithNamespace).

% allowedActionExecution(++Namespace:atom, ?Action:compound_term, ++Facts:list, -NewFacts:list) is nondet
%
% @arg Namespace This could be any atom. The Namespace used here should match the
%                one used in "beginPlanning" and "endPlanning".
% @arg Action The name of an action
% @arg Facts A set of facts.
% @arg NewFacts A set facts obtained after applying Action in Facts.
%
% True if Action can be applied in the set of Facts (conditions are satisfied) and
% NewFacts is the resulting set after applying the Action. NewFacts must have a
% specific order.
allowedActionExecution(Namespace, Action, Facts, NewFacts) :-
  actionSpec(Namespace, Action, TypeSpecs, PrologConditions, Conditions, RemovedFacts, AddedFacts, PrologSideEffects),
  typeSpecsWithNamespace(Namespace, TypeSpecs, TypeSpecsWithNamespace),
  satisfiedGoalsList(TypeSpecsWithNamespace),
  satisfiedGoalsList(PrologConditions),
  separatedGoals(Conditions, InclusionConditions, AbscenceConditions),
  subsetOf(InclusionConditions, Facts),
  emptyIntersection(AbscenceConditions, Facts),
  updatedFacts(Facts, RemovedFacts, AddedFacts, NewFacts),
  executeSideEffects(PrologSideEffects).

% planExecutionWithForbiddenStates(++Namespace:atom, ++Facts:list, ++Goals:list, ?Plan:list, ++ForbiddenStates:list) is nondet
%
% @arg Namespace This could be any atom. The Namespace used here should match the
%                one used in "beginPlanning" and "endPlanning".
% @arg Facts A set of facts.
% @arg Goals A set of goals.
% @arg Plan A sequence of actions that satisfy the Goals after being applied to
%           the Facts.
% @arg ForbiddenStates A list of states which are states that can't be passed through
%                      when the plan is followed starting from Facts.
%
% True if all Goals are satisfied after the sequence of actions in Plan is applied
% to the Facts, without passing thorugh any state in ForbiddenStates. 
planExecutionWithForbiddenStates(_, Facts, Goals, [], _) :-
  separatedGoals(Goals, InclusionGoals, AbsenceGoals),
  subsetOf(InclusionGoals, Facts),
  emptyIntersection(AbsenceGoals, Facts), !. % red cut
planExecutionWithForbiddenStates(Namespace, Facts, Goals, [FirstAction|RestOfPlan], ForbiddenStates) :-
  allowedActionExecution(Namespace, FirstAction, Facts, NewFacts),
  not(setIsOneOf(NewFacts, ForbiddenStates)),
  planExecutionWithForbiddenStates(Namespace, NewFacts, Goals, RestOfPlan, [NewFacts|ForbiddenStates]).