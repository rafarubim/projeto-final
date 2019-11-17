:- module(planning, [planExecution/5, beginDomainDefinition/1, endDomainDefinition/1, deleteDomain/1, beginProblemDefinition/1, endProblemDefinition/1, deleteProblem/1]).

/**
 * <module> Planning
 * 
 *   This module implements a STRIPS-like planner with prolog functionalities. It consumes external predicates
 * for domain definition (actionSpec, type) and problem definition. It defines a predicate (planExecution) to
 * return plans that satisfy the problem.
 * To use the module, an ontology similar to PDDL STRIPS must be followed to define a planning domain/problem.
 * It is explained below.
 * 
 *   You begin defining a domain. To do it, you must define all its types and actions. This is done by
 * defining your own predicates and passing them to the planning module in the runtime. Types are defined in the
 * argument of "type/1" facts. Actions are defined through the arguments of "actionSpec/7" facts. An example is
 * shown below, explaining each argument of the action specification. You can also check the "External predicate
 * interfaces" in the docs below.
 * 
 * ---------- yourModule.pl ----------
 * a
 * :- beginDomainDefinition(domain_name).
 * 
 * type(person).
 * type(location).
 * 
 * actionSpec(
 *   move(P, L1, L2),                       % arg1-> Name of the action. All variables used in actionSpec 
 *                                          %        should be declared here (using them for prolog capabilities
 *                                          %        only may be an exception).
 *   [person(P), location(X), location(Y)], % arg2-> Type declarations. All variables used in actionSpec should
 *                                          %        instantiated here to a domain type, in a list format.
 *   [X \== Y],                             % arg3-> Prolog conditions. This is a list of goals that use prolog
 *                                          %        capabilities (such as arithmetic and comparasion). The action
 *                                          %        can only execute if all of these are satisfied. Remember this
 *                                          %        is tested before anything else, and variables declared
 *                                          %        previously are already instantiated when these goals are 
 *                                          %        tested for. Just like in prolog, goal order matters.
 *   [onLocation(P, L1)],                   % arg4-> Planner Preconditions. This is a list of planner facts that
 *                                          %        should be true for the action to execute. If you want to
 *                                          %        specify that a fact should not be true, you can use the
 *                                          %        "not(x)" predicate or the not operator "\+ x" with the fact.
 *   [onLocation(P, L1)],                   % arg5-> Removed facts. This is a list of facts that are removed if
 *                                          %        the action is executed. "not(x)" and "\+ x" can't be used here.
 *   [onLocation(P, L2)],                   % arg6-> Added facts. This is a list of facts that are added if the
 *                                          %        action is executed. This always happens after the facts are
 *                                          %        removed. "not(x)" and "\+ x" can't be used here.
 *   [write("Moved person!")]               % arg7-> Prolog side effects. This is a list of goals that are executed
 *                                          %        if the action is executed. They execute regardless of their
 *                                          %        success. This should only be used for side effects, such as
 *                                          %        printing to the console.
 * ).
 * 
 * :- endDomainDefinition(domain_name).
 * 
 * ------ end of yourModule.pl -------
 * 
 *   Then, you must define a problem. To do it, you must define all the planner objects and their types. You must
 * also define the planner facts, but this is done later. An example is shown below:
 * 
 *  ---------- yourModule.pl ----------
 * 
 * % This must be called after the call to "endDomainDefinition(domain_name)".
 * :- beginProblemDefinition(domain_name).
 * 
 * person(jorge).
 * 
 * location(hogwarts).
 * location(hyrule).
 * 
 * :- endProblemDefinition(domain_name).
 * 
 * ------ end of yourModule.pl -------
 * 
 *   Now that the domain and problem are defined, you are able to do the planning. Note that both of them were
 * defined under the namespace "domain_name". This makes everything connected and lets you do multiple plannings
 * on different domains/problems simultanously, as long as you use different namespaces for them. Also note that
 * a namespace contains a single domain and problem. If you want to want to use the namespace again, you can call
 * ":- deleteProblem(domain_name)" or ":- deleteDomain(domain_name)". Deleting the domain also deletes the problem.
 *   To use the planner, you call the "planExecution" predicate with the corresponding namespace. It also receives
 * a list of planner facts and planner goals (which are just like actionSpec's arg4). The planner will return you
 * a plan (list of actions in the order they should be executed) and the final set of facts, resulting from the
 * executing each action in the plan. A final example follows below, with the query and the result.
 * 
 * ?- planExecution(
 * ?-   domain_name,                    % Namespace
 * ?-   [onLocation(jorge, hogwarts)],  % List of facts
 * ?-   [onLocation(jorge, hyrule)],    % List of goals
 * ?-   Plan,                           % Returned plan
 * ?-   FinalFacts                      % Facts that remain after plan is executed
 * ?- ).
 * Plan = [move(jorge, hogwarts, hyrule)];
 * FinalFacts = [onLocation(jorge, hyrule)].
 * 
 * @author Rafael Rubim Cabral
 * @version 0.1.0
 * 
 * Docs model:
 * @see https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/pldoc.html%27)
 **/

% -------------------- External predicate interfaces

% actionSpec(-Action:compound_term, -TypeSpecs:list, -PrologConditions:list, -Conditions:list,
%                     -RemovedFacts:list, -AddedFacts:list, -PrologSideEffects:list) is nondet
% 
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
% 
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
:- dynamic type/2.

% -------------------- Public predicates

% beginDomainDefinition(++Namespace:atom) is det
% 
% @arg Namespace Could be any atom. Will be the same to be used by the corresponding
%                "endDomainDefinition".
% 
% This calls "beginGetRuntimeClauses" for "actionSpec" and "type" clauses.
beginDomainDefinition(Namespace) :-
  % Capture "actionSpec" definitions
  namespaceOfActionSpec(Namespace, ActionSpecNamespace),
  beginGetRuntimeClauses(ActionSpecNamespace, actionSpec(_,_,_,_,_,_,_), true),
  % Capture "type" definitions
  namespaceOfType(Namespace, TypeNamespace),
  beginGetRuntimeClauses(TypeNamespace, type(_), true).


% endDomainDefinition(++Namespace:atom) is det
% 
% @arg Namespace Could be any atom. Must be the same that was used by the corresponding
%                "beginDomainDefinition".
% 
% This calls "endGetRuntimeClauses"  for "actionSpec" and "type" clauses. These clauses
% are asserted in the planning module with an extra first argument in their head's compound
% term: Namespace.  
endDomainDefinition(Namespace) :-
  % Assert local "actionSpec/8" predicates with a Namespace
  namespaceOfActionSpec(Namespace, ActionSpecNamespace),
  endGetRuntimeClauses(ActionSpecNamespace, ActionSpecClauses),
  length(ActionSpecClauses, ActionSpecLength),
  findall(Namespace, between(1,ActionSpecLength,_), ActionSpecNamespaces),
  maplist(planning:clauseWithNamespace, ActionSpecNamespaces, ActionSpecClauses, ActionSpecClausesWithNamespace),
  maplist(planning:assertClause, ActionSpecClausesWithNamespace),
  % Assert local "type/2" predicates with a Namespace
  namespaceOfType(Namespace, TypeNamespace),
  endGetRuntimeClauses(TypeNamespace, TypeClauses),
  length(TypeClauses, TypeLength),
  findall(Namespace, between(1,TypeLength,_), TypeNamespaces),
  maplist(planning:clauseWithNamespace, TypeNamespaces, TypeClauses, TypeClausesWithNamespace),
  maplist(planning:assertClause, TypeClausesWithNamespace).

% deleteDomain(++Namespace:atom) is det
%
% This deletes the problem associated to the Namespace, domain, if there's one. It also
% deletes all local "actionSpec/8" and "type/2" predicates associated to Namespace, which
% are the domain itself.
deleteDomain(Namespace) :-
  deleteProblem(Namespace),
  retractall(actionSpec(Namespace,_,_,_,_,_,_,_)),
  retractall(type(Namespace, _)).

% beginProblemDefinition(++Namespace:atom) is det
% 
% @arg Namespace Could be any atom. Must be the same that was used by the corresponding
%                "endDomainDefinition".
% 
% This calls "beginGetRuntimeClauses" for each type/2 definition with a type functor, for
% these type functors' clauses.
beginProblemDefinition(Namespace) :-
  context_module(Context),
  % Capture all type functor definitions
  findall(_, planning:beginOneTypeFunctor(Context, Namespace), _).

% endProblemDefinition(++Namespace:atom) is nondet
% 
% @arg Namespace Could be any atom. Must be the same that was used by the corresponding
%                "endDomainDefinition".
% 
% This calls "endGetRuntimeClauses" for each type/2 definition with a type functor, for
% these type functors' clauses. These clauses are asserted in the planning module with
% an extra first argument in their head's compound term: Namespace.  
endProblemDefinition(Namespace) :-
  % Assert local type functor predicates with a Namespace
  findall(_, planning:endOneTypeFunctor(Namespace), _).

% deleteProblem(++Namespace:atom) is det
%
% This deletes all local "typeFuctor/2" predicates associated to Namespace, for each
% type/2 definition with a type functor. It effectively deletes a problem.
deleteProblem(Namespace) :-
  findall(_, deleteOneTypeFunctor(Namespace), _).

% planExecution(++Namespace:atom, ++Facts:list, ++Goals:list, ?Plan:list, ?FinalFacts:list) is nondet
%
% @arg Namespace This could be any atom. The Namespace used here should match the
%                one used in "endDomainDefinition" and "endProblemDefinition".
% @arg Facts A set of facts.
% @arg Goals A set of goals.
% @arg Plan A sequence of actions that satisfy the Goals after being applied to
%           the Facts.
% @arg FinalFacts The set of facts that remains after Plan is executed.
%
% True if all Goals are satisfied after the sequence of actions in Plan is applied
% to the Facts, generating FinalFacts.
planExecution(Namespace, Facts, Goals, Plan, FinalFacts) :-
  planExecutionWithForbiddenStates(Namespace, Facts, Goals, Plan, [Facts], FinalFacts).

% -------------------- Private predicates

% beginOneTypeFunctor(++Context:atom, ++Namespace:atom) is nondet
% 
% @arg Context Name of the module that defined the type functors.
% @arg Namespace Could be any atom. Must be the same that was used by the corresponding
%                "endDomainDefinition" and will be used by "endProblemDefinition".
% 
% True if a type/2 definition exists with a type functor. This calls "beginGetRuntimeClauses"
% for that type functor's clauses.
beginOneTypeFunctor(Context, Namespace) :-
  type(Namespace, TypeFunctor),
  namespaceOfTypeFunctor(TypeFunctor, Namespace, TypeFunctorNamespace),
  TypeHeadTerm =.. [TypeFunctor, _],
  beginGetRuntimeClauses(TypeFunctorNamespace, Context:TypeHeadTerm, _).

% endOneTypeFunctor(++Namespace:atom) is nondet
% 
% @arg Namespace Could be any atom. Must be the same that was used by the corresponding
%                "endDomainDefinition".
% 
% True if a type/2 definition exists with a type functor. This calls "endGetRuntimeClauses"
% for that type functor's clauses. These clauses are asserted in the planning module with
% an extra first argument in their head's compound term: Namespace.  
endOneTypeFunctor(Namespace) :-
  type(Namespace, TypeFunctor),
  namespaceOfTypeFunctor(TypeFunctor, Namespace, TypeFunctorNamespace),
  endGetRuntimeClauses(TypeFunctorNamespace, TypeFunctorClauses),
  length(TypeFunctorClauses, TypeFunctorLength),
  findall(Namespace, between(1,TypeFunctorLength,_), TypeFunctorNamespaces),
  maplist(planning:clauseWithNamespace, TypeFunctorNamespaces, TypeFunctorClauses, TypeFunctorClausesWithNamespace),
  maplist(planning:assertClause, TypeFunctorClausesWithNamespace).

% deleteOneTypeFunctor(++Namespace:atom) is nondet
%
% True if a type/2 definition exists with a type functor. This deletes all local predicates
% typeFunctor/2 associated to Namespace.
deleteOneTypeFunctor(Namespace) :-
  type(Namespace, TypeFunctor),
  TypeFunctorTerm =.. [TypeFunctor, Namespace, _],
  retractall(TypeFunctorTerm).

% namespaceOfActionSpec(++Namespace:atom, -ActionSpecNamespace:atom) is det
%
% True if ActionSpecNamespace is the namespace associated to the actionSpec predicate.
namespaceOfActionSpec(Namespace, ActionSpecNamespace) :-
  atom_concat('actionSpec--', Namespace, ActionSpecNamespace).

% namespaceOfType(++Namespace:atom, -TypeNamespace:atom) is det
%
% True if TypeNamespace is the namespace associated to the type predicate.
namespaceOfType(Namespace, TypeNamespace) :-
  atom_concat('type--', Namespace, TypeNamespace).

% namespaceOfTypeFunctor(++TypeFunctor:atom, ++Namespace:atom, -TypeFunctorNamespace:atom) is det
%
% True if TypeFunctorNamespace is the namespace associated to the TypeFunctor predicate.
namespaceOfTypeFunctor(TypeFunctor, Namespace, TypeFunctorNamespace) :-
  atomic_list_concat([TypeFunctor, '--', Namespace], TypeFunctorNamespace).

% clauseWithNamespace(++Namespace:atom, +Clause:clause, -ClauseWithNamespace:clause) is det
%
% True if ClauseWithNamespace is equal to the Clause with an extra first argument in the head's
% term: Namespace.
clauseWithNamespace(Namespace, Head:-Body, NewHead:-Body) :-
  Head =.. [Functor|Args],
  NewHead =.. [Functor,Namespace|Args].

% termWithNamespace(++Namespace:atom, +Term:compound_term, -TermWithNamespace:compound_term) is det
%
% True if TermWithNamespace is equal to the Term with an extra first argument: Namespace.
termWithNamespace(Namespace, Term, TermWithNamespace) :-
  Term =.. [Functor|Args],
  TermWithNamespace =.. [Functor,Namespace|Args].

% assertClause(+Clause:clause) is det
%
% Asserts the clause within the planning module, regardless of the caller context module.
assertClause(Clause) :-
  (Head:-_)=Clause,
  write(Head),
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

% typeSpecsWithNamespace(++Namespace:atom, ++TypeSpecs:list, -TypeSpecsWithNamespace:list) is det
% 
% @arg Namespace This could be any atom. The Namespace used here should match the
%                one used in "endDomainDefinition" and "endProblemDefinition".
% @arg TypeSpecs A list of terms which declare object types.
% @arg TypeSpecsWithNamespace The same list as TypeSpecs, but with an extra first argument in
%                             each of its terms: Namespace.
% 
% True if TypeSpecsWithNamespace is the same list as TypeSpecs, but with an extra first argument in
% each of its terms: Namespace.
typeSpecsWithNamespace(Namespace, TypeSpecs, TypeSpecsWithNamespace) :-
  length(TypeSpecs, TypeSpecsLength),
  findall(Namespace, between(1, TypeSpecsLength, _), Namespaces),
  maplist(termWithNamespace, Namespaces, TypeSpecs, TypeSpecsWithNamespace).

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

% allowedActionExecution(++Namespace:atom, ?Action:compound_term, ++Facts:list, -NewFacts:list) is nondet
%
% @arg Namespace This could be any atom. The Namespace used here should match the
%                one used in "endDomainDefinition" and "endProblemDefinition".
% @arg Action The name of an action
% @arg Facts A set of facts.
% @arg NewFacts A set facts obtained after applying Action in Facts.
%
% True if Action can be applied in the set of Facts (conditions are satisfied) and
% NewFacts is the resulting set after applying the Action. NewFacts must have a
% specific order.
allowedActionExecution(Namespace, Action, Facts, NewFacts) :-
  actionSpect(Namespace, Action, TypeSpecs, PrologConditions, Conditions, RemovedFacts, AddedFacts, PrologSideEffects),
  typeSpecsWithNamespace(Namespace, TypeSpecs, TypeSpecsWithNamespace),
  satisfiedGoalsList(TypeSpecsWithNamespace),
  satisfiedGoalsList(PrologConditions),
  separatedGoals(Conditions, InclusionConditions, AbscenceConditions),
  subsetOf(InclusionConditions, Facts),
  emptyIntersection(AbscenceConditions, Facts),
  updatedFacts(Facts, RemovedFacts, AddedFacts, NewFacts),
  executeSideEffects(PrologSideEffects).

% planExecutionWithForbiddenStates(++Namespace:atom, ++Facts:list, ++Goals:list, ?Plan:list, ++ForbiddenStates:list,
%                                 ?FinalFacts:list) is nondet
%
% @arg Namespace This could be any atom. The Namespace used here should match the
%                one used in "endDomainDefinition" and "endProblemDefinition".
% @arg Facts A set of facts.
% @arg Goals A set of goals.
% @arg Plan A sequence of actions that satisfy the Goals after being applied to
%           the Facts.
% @arg ForbiddenStates A list of states which are states that can't be passed through
%                      when the plan is followed starting from Facts.
% @arg FinalFacts The set of facts that remains after Plan is executed.
%
% True if all Goals are satisfied after the sequence of actions in Plan is applied
% to the Facts, without passing thorugh any state in ForbiddenStates. 
planExecutionWithForbiddenStates(_, Facts, Goals, [], _, Facts) :-
  separatedGoals(Goals, InclusionGoals, AbsenceGoals),
  subsetOf(InclusionGoals, Facts),
  emptyIntersection(AbsenceGoals, Facts), !. % red cut
planExecutionWithForbiddenStates(Namespace, Facts, Goals, [FirstAction|RestOfPlan], ForbiddenStates, FinalFacts) :-
  allowedActionExecution(Namespace, FirstAction, Facts, NewFacts),
  not(setIsOneOf(NewFacts, ForbiddenStates)),
  planExecutionWithForbiddenStates(Namespace, NewFacts, Goals, RestOfPlan, [NewFacts|ForbiddenStates], FinalFacts).