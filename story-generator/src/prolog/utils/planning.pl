:- module(planning, [plan/5, planAStar/7, planAStar/8, beginDomainDefinition/1, endDomainDefinition/1, deleteDomain/1, beginProblemDefinition/1, endProblemDefinition/1, deleteProblem/1]).

/**
 * <module> Planning
 * 
 *   This module implements a STRIPS-like planner with prolog functionalities. It consumes external predicates
 * for domain definition (actionSpec, type) and problem definition. It defines two predicates (plan, planAStar) to
 * return plans that satisfy the problem.
 * To use the module, an ontology similar to PDDL STRIPS must be followed to define a planning domain/problem.
 * It is explained below.
 * 
 *   You begin defining a domain. To do it, you must define all its types and actions. This is done by
 * defining your own predicates and passing them to the planning module in the runtime. Types are defined in the
 * argument of "type/1" facts. Actions are defined through the arguments of "actionSpec/8" facts. An example is
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
 *                                          %        should be declared here (using them only for temporary
 *                                          %        prolog calculations may be an exception).
 *   [person(P), location(X), location(Y)], % arg2-> Type declarations. All variables used in actionSpec should
 *                                          %        instantiated here to a domain type, in a list format.
 *   [X \== Y],                             % arg3-> Prolog conditions. This is a list of goals that use prolog
 *                                          %        capabilities (such as arithmetic and comparasion). The action
 *                                          %        can only execute if all of these are satisfied. Remember this
 *                                          %        is tested before anything else, and variables declared
 *                                          %        previously in arg2 are already instantiated when these goals 
 *                                          %        are tested for. Just like in prolog, goal order matters.
 *   [onLocation(P, L1)],                   % arg4-> Planner Preconditions. This is a list of planner facts that
 *                                          %        should be true for the action to execute. If you want to
 *                                          %        specify that a fact should not be true, you can use the
 *                                          %        "not(x)" predicate or the not operator "\+ x" with the fact.
 *   [],                                    % arg5-> More Prolog conditions. This is the same as arg3, but executes
 *                                          %        after the goals in arg4, which may instantiate some variable.
 *   [write("Moved person!")]               % arg6-> Prolog effects. This is a list of goals that are executed
 *                                          %        if the action is executed. They execute regardless of their
 *                                          %        success. This runs before the other effects, so it may be
 *                                          %        useful to instantiate variables which were declared in arg3
 *                                          %        and not arg2, like the ones that deal with numbers. This can
 *                                          %        also be used to execute side effects, like printing.
 *   [onLocation(P, L1)],                   % arg7-> Removed facts. This is a list of facts that are removed if
 *                                          %        the action is executed. "not(x)" and "\+ x" can't be used here.
 *   [onLocation(P, L2)],                   % arg8-> Added facts. This is a list of facts that are added if the
 *                                          %        action is executed. This always happens after the facts are
 *                                          %        removed. "not(x)" and "\+ x" can't be used here.
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
 * location(araluen).
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
 *   To use the planner, you call the "plan" predicate with the corresponding namespace. It also receives
 * a list of planner facts and planner goals (which are just like actionSpec's arg4). The planner will return you
 * a plan (list of actions in the order they should be executed) and the final set of facts, resulting from the
 * executing each action in the plan. The predicate "planAStar" is almost the same, but it receives an Heuristic
 * meta predicate that determines the cost of each action, returning also the PlanCost of the whole plan. "planAStar"
 * Only returns the plan(s) with best score (lowest PlanCost). A final example follows below, with the queries and
 * results to "plan" and "planAStar".
 * 
 * ?- % This will return all plans that solve the problem.
 * ?- plan(
 * ?-   domain_name,                    % Namespace
 * ?-   [onLocation(jorge, hogwarts)],  % List of facts
 * ?-   [onLocation(jorge, hyrule)],    % List of goals
 * ?-   Plan,                           % Returned plan
 * ?-   FinalFacts                      % Facts that remain after plan is executed
 * ?- ).
 * Plan = [move(jorge,hogwarts,araluen),move(jorge,araluen,hyrule)],
 * FinalFacts = [onLocation(jorge, hyrule)];
 * Plan = [move(jorge,hogwarts,hyrule)],
 * FinalFacts = [onLocation(jorge, hyrule)];
 * false.
 *
 * ?- % Moving from hogwarts to hyrule directly costs 50. Any other action costs 5.
 * ?- assert(
 * ?-   heuristic(actionExecution(move(_,hogwarts, hyrule),_), 50) :- !
 * ?- ).
 * true.
 *
 * ?- assert(
 * ?-   heuristic(_, 5)
 * ?- ).
 * true.
 *
 * ?- % This will only return the best plan that solves the problem, given the heuristic.
 * ?- % It won't move directly from hogwarts to hyrule since it costs more.
 * ?- planAStar(
 * ?-   domain_name,                    % Namespace
 * ?-   [onLocation(jorge, hogwarts)],  % List of facts
 * ?-   [onLocation(jorge, hyrule)],    % List of goals
 * ?-   heuristic,                      % Heuristic meta predicate
 * ?-   Plan,                           % Returned plan
 * ?-   PlanCost,                       % Total cost of the plan
 * ?-   FinalFacts                      % Facts that remain after plan is executed
 * ?- ).
 * Plan = [move(jorge,hogwarts,araluen),move(jorge,araluen,hyrule)],
 * PlanCost = 10,
 * FinalFacts = [onLocation(jorge, hyrule)];
 * false.
 * 
 * @author Rafael Rubim Cabral
 * @version 0.1.0
 * 
 * Docs model:
 * @see https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/pldoc.html%27)
 **/

% -------------------- External and meta predicate interfaces

% actionSpec(?Action:compound_term, -TypeSpecs:list, -PrologConditions:list, -Conditions:list,
%            -MorePrologConditions:list, -PrologEffects:list, -RemovedFacts:list, -AddedFacts:list) is nondet
% 
% @arg Action The name of the action
% @arg TypeSpecs Set of variable type instantiations
% @arg PrologConditions Sequence of Prolog goals which are conditions for execution of Action
% @arg Conditions Set of conditions of the action
% @arg MorePrologConditions The same as PrologConditions, but it executes after Conditions does.
% @arg PrologSideEffects Prolog goals which are executed as an effect of the action, regardless of their success.
% @arg RemovedActions Facts which are removed from the state as the effect of the action
% @arg AddedActions Facts which are added to the state as the effect of the action
%
% True if all the arguments comply with the specification of a user-defined action

% type(?Type:atom) is nondet
% 
% @arg Type Name of the type
%
% True if Type is a user-defined type

% 'typeFunctor'(?Object:atom) is nondet
% 
% @arg Object Object instantiated to the type
%
%   'typeFunctor' is not an actual atom, but could be any atom the user defines as a type.
% True if Object is instantiated to the 'typeFunctor' type.

% planAStar's "Heuristic" meta predicate:
% 'heuristic'(++ActionExecution, -Cost) is det
% 
%   'heuristic' is not an actual atom, but could be any predicate name the user defines to use as
% meta predicate. It receives an ActionExecution in the format
% actionExecution(Action:compound_term, ObtainedFacts:list). Action is the action name defined by
% an "actionSpec" predicate, with instantiated variables. ObtainedFacts is a set of facts the planner
% will achieve after executing that action. The cost of this actionExecution should be returned through
% Cost.

% -------------------- End of external and meta predicate interfaces

:- use_module(apply).
:- use_module(lists).
:- use_module(set).
:- use_module(assertRuntimeTerms).
:- use_module(heaps).

:- module_transparent([beginDomainDefinition/1, beginProblemDefinition/1]).

:- meta_predicate planAStar(+,+,+,2,-,-,-).
:- meta_predicate planAStar(+,+,+,2,-,-,-,+).

% actionSpec(++Namespace:atom, -Action:compound_term, -TypeSpecs:list, -PrologConditions:list, -Conditions:list,
%            -MorePrologConditions:list, -PrologEffects:list, -RemovedFacts:list, -AddedFacts:list) is nondet
%
% Equivalent to the "external predicate" actionSpec/8, but with a "Namespace" atom as first argument.
:- dynamic actionSpec/9.

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
% This calls "beginAssertRuntimeTerms" for "actionSpec" and "type" clauses.
beginDomainDefinition(Namespace) :-
  beginAssertRuntimeTerms(planning, [actionSpec/8, type/1], Namespace).

% endDomainDefinition(++Namespace:atom) is det
% 
% @arg Namespace Could be any atom. Must be the same that was used by the corresponding
%                "beginDomainDefinition".
% 
% This calls "endAssertRuntimeTerms"  for "actionSpec" and "type" clauses. These clauses
% are asserted in the planning module with an extra first argument in their head's compound
% term: Namespace.  
endDomainDefinition(Namespace) :-
  endAssertRuntimeTerms(Namespace).

% deleteDomain(++Namespace:atom) is det
%
% This deletes the problem associated to the Namespace, domain, if there's one. It also
% deletes all local "actionSpec/9" and "type/2" predicates associated to Namespace, which
% are the domain itself.
deleteDomain(Namespace) :-
  deleteProblem(Namespace),
  retractall(actionSpec(Namespace,_,_,_,_,_,_,_,_)),
  retractall(type(Namespace, _)).

% beginProblemDefinition(++Namespace:atom) is det
% 
% @arg Namespace Could be any atom. Must be the same that was used by the corresponding
%                "endDomainDefinition".
% 
% This calls "beginAssertRuntimeTerms" with all type/2 definition with a type functor, for
% these type functors' clauses.
beginProblemDefinition(Namespace) :-
  findall(TypeFunctor/1, (planning:type(Namespace, TypeFunctor), (dynamic TypeFunctor/1)), TypePredicates),
  beginAssertRuntimeTerms(planning, TypePredicates, Namespace).

% endProblemDefinition(++Namespace:atom) is nondet
% 
% @arg Namespace Could be any atom. Must be the same that was used by the corresponding
%                "endDomainDefinition".
% 
% This calls "endAssertRuntimeTerms" with all type/2 definition with a type functor, for
% these type functors' clauses. These clauses are asserted in the planning module with
% an extra first argument in their head's compound term: Namespace.  
endProblemDefinition(Namespace) :-
  % Assert local type functor predicates with a Namespace
  endAssertRuntimeTerms(Namespace).

% deleteProblem(++Namespace:atom) is det
%
% This deletes all local "typeFuctor/2" predicates associated to Namespace, for each
% type/2 definition with a type functor. It effectively deletes a problem.
deleteProblem(Namespace) :-
  findall(_, deleteOneTypeFunctor(Namespace), _).

% plan(++Namespace:atom, ++Facts:list, +Goals:list, ?Plan:list, ?FinalFacts:list) is nondet
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
plan(Namespace, Facts, Goals, Plan, FinalFacts) :-
  planWithForbiddenStates(Namespace, Facts, Goals, Plan, [Facts], FinalFacts).


% planAStar(++Namespace:atom, ++Facts:list, +Goals:list, :Heuristic:atom, ?Plan:list, -PlanCost:number, -FinalFacts:list) is nondet
% planAStar(++Namespace:atom, ++Facts:list, +Goals:list, :Heuristic:atom, ?Plan:list, -PlanCost:number, -FinalFacts:list, AllowedActionsAsFirst:list) is nondet
% 
% @arg Namespace This could be any atom. The Namespace used here should match the one used in "endDomainDefinition" and "endProblemDefinition".
%
%   True if Plan is a list of actions that make the planer Goals be satisfied from the initial Facts. The Heuristic meta predicate determines
% the order in which actions happen to compute the Plan by associating costs to each action. That also determines the total PlanCost, which is
% returned. Only the best plan is returned, unless that are other equivalent-costed ones. FinalFacts is the set of facts that remain after the
% plan is executed.
%   If AllowedActionsAsFirst is passed, the algorithm only considers the actions in this list as the first step of the planning.
planAStar(_, Facts, Goals, _, [], 0, Facts) :-
  satisfiedPlannerGoals(Facts, Goals), !. % red cut
planAStar(Namespace, Facts, Goals, Heuristic, Plan, PlanCost, FinalFacts) :-
  empty_heap(Heap),
  add_to_heap(Heap, 0, halfPlanExecution([], Facts), MiddleHeap),
  openNonForbiddenActionsInHeap(Namespace, [Facts], Heuristic, -1, MiddleHeap, NewHeap, _),
  \+ empty_heap(NewHeap),
  planAStar_(Namespace, NewHeap, Goals, Heuristic, [Facts], ReversePlan, PlanCost, FinalFacts),
  reverse(ReversePlan, Plan).
planAStar(_, Facts, Goals, _, [], 0, Facts, _) :-
  satisfiedPlannerGoals(Facts, Goals), !. % red cut
planAStar(Namespace, Facts, Goals, Heuristic, Plan, PlanCost, FinalFacts, AllowedActionsAsFirst) :-
  empty_heap(Heap),
  add_to_heap(Heap, 0, halfPlanExecution([], Facts), MiddleHeap),
  setTempActions(Namespace, AllowedActionsAsFirst),
  openNonForbiddenActionsInHeap(Namespace, [Facts], Heuristic, -1, MiddleHeap, NewHeap, _),
  revertTempActions(Namespace),
  \+ empty_heap(NewHeap),
  planAStar_(Namespace, NewHeap, Goals, Heuristic, [Facts], ReversePlan, PlanCost, FinalFacts),
  reverse(ReversePlan, Plan).

% -------------------- Private predicates

% setTempActions(++Namespace:atom, +Actions:list) is det
%
% This temporarily changes the Namespace of all actions which are not in the Actions list, so they don't work in the planner.
setTempActions(Namespace, Actions) :-
  nontempActionNamespace(Namespace, NonTempActionNamespace),
  changeActionNamespaces(Namespace, NonTempActionNamespace),
  maplist(setTempAction(Namespace), Actions).

% revertTempActions(++Namespace:atom) is det
%
% This reverts what the "setTempActions" predicate do.
revertTempActions(Namespace) :-
  deleteTempActions(Namespace),
  nontempActionNamespace(Namespace, NonTempActionNamespace),
  changeActionNamespaces(NonTempActionNamespace, Namespace).

% changeActionNamespaces(++Namespace:atom, ++NewNamespace:atom) is det
%
% This changes all actionSpecs associated to Namespace to be associated to NewNamespace.
changeActionNamespaces(Namespace, NewNamespace) :-
  findall(
    _,
    (
      actionSpec(Namespace, Action, TypeSpecs, PlConds, Conds, MorePlConds, PlEffs, RmFs, AddFs),
      retract(actionSpec(Namespace, Action, TypeSpecs, PlConds, Conds, MorePlConds, PlEffs, RmFs, AddFs)),
      assert(actionSpec(NewNamespace, Action, TypeSpecs, PlConds, Conds, MorePlConds, PlEffs, RmFs, AddFs))
    ),
    _
  ).

% setTempAction(++Namespace:atom, +Action:compound_term) is det
%
% This creates a copy of the non temp actionSpec associated to Action. It uses Namespace to create this temporary action.
setTempAction(Namespace, Action) :-
  nontempActionNamespace(Namespace, NonTempActionNamespace),
  actionSpec(NonTempActionNamespace, Action, TypeSpecs, PlConds, Conds, MorePlConds, PlEffs, RmFs, AddFs),
  assert(actionSpec(Namespace, Action, TypeSpecs, PlConds, Conds, MorePlConds, PlEffs, RmFs, AddFs)).

% deleteTempActions(++Namespace:atom) is det
% 
% This removes all actionSpecs associated to Namespace (this should be used when they're temporary).
deleteTempActions(Namespace) :-
  retractall(actionSpec(Namespace, _, _, _, _, _, _, _, _)).

% nontempActionNamespace(?Namespace:atom, ?NonTempActionNamespace:atom) is semidet
% 
% True if NonTempActionNamespace is the Namespace with a temp sufix
nontempActionNamespace(Namespace, NonTempActionNamespace) :-
  atomic_concat(Namespace, '--nontemp', NonTempActionNamespace).

% deleteOneTypeFunctor(++Namespace:atom) is nondet
%
% True if a type/2 definition exists with a type functor. This deletes all local predicates
% typeFunctor/2 associated to Namespace.
deleteOneTypeFunctor(Namespace) :-
  type(Namespace, TypeFunctor),
  TypeFunctorTerm =.. [TypeFunctor, Namespace, _],
  retractall(TypeFunctorTerm).

% termWithNamespace(++Namespace:atom, +Term:compound_term, -TermWithNamespace:compound_term) is det
%
% True if TermWithNamespace is equal to the Term with an extra first argument: Namespace.
termWithNamespace(Namespace, Term, TermWithNamespace) :-
  Term =.. [Functor|Args],
  TermWithNamespace =.. [Functor,Namespace|Args].

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

% emptyIntersection(+SetA, ++SetB) is semidet
%
% True if the intersection of SetA/SetB is empty. If any element in SetA can
% be instanced to one in SetB, then it is not empty.
emptyIntersection(SetA, SetB) :-
  \+ hasIntersection(SetA, SetB).

% satisfiedPrologGoalsList(:GoalsList:list) is nondet
%
% True if GoalsList is a list of satisfied goals (in conjunction).
satisfiedPrologGoalsList(GoalsList) :-
  maplist(call, GoalsList).

% executePrologEffects(:EffectsList:list) is det
%
% All goals in EffectsList are called, regardless of their success.
executePrologEffects(EffectsList) :-
  maplist(ignore, EffectsList).

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

% allowedActionExecution(++Namespace:atom, ?Action:compound_term, ++Facts:list, -NewFacts:list) is nondet
%
% @arg Namespace This could be any atom. The Namespace used here should match the
%                one used in "endDomainDefinition" and "endProblemDefinition".
% @arg Action The name of an action
% @arg Facts A set of facts.
% @arg NewFacts A set facts obtained after applying Action in Facts.
%
%   True if Action can be applied in the set of Facts (conditions are satisfied) and
% NewFacts is the resulting set after applying the Action. NewFacts must have a
% specific order.
allowedActionExecution(Namespace, Action, Facts, NewFacts) :-
  actionSpec(Namespace, Action, TypeSpecs, PrologConditions, Conditions, MorePrologConditions, PrologEffects, RemovedFacts, AddedFacts),
  maplist(termWithNamespace(Namespace), TypeSpecs, TypeSpecsWithNamespace),
  satisfiedPrologGoalsList(TypeSpecsWithNamespace),
  satisfiedPrologGoalsList(PrologConditions),
  separatedGoals(Conditions, InclusionConditions, AbscenceConditions),
  subsetOf(InclusionConditions, Facts),
  emptyIntersection(AbscenceConditions, Facts),
  satisfiedPrologGoalsList(MorePrologConditions),
  executePrologEffects(PrologEffects),
  updatedFacts(Facts, RemovedFacts, AddedFacts, NewFacts).

% satisfiedPlannerGoals(++Facts:list, +Goals:list) is semidet
%
%   True if planner Goals are satisfied by the Facts. Goals like "not(x)" or "\+ x" are satisfied if the fact
% doesn't exist in Facts.
satisfiedPlannerGoals(Facts, Goals) :-
  separatedGoals(Goals, InclusionGoals, AbsenceGoals),
  subsetOf(InclusionGoals, Facts),
  emptyIntersection(AbsenceGoals, Facts).

% planWithForbiddenStates(++Namespace:atom, ++Facts:list, +Goals:list, ?Plan:list, ++ForbiddenStates:list,
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
%   True if all Goals are satisfied after the sequence of actions in Plan is applied
% to the Facts, without passing thorugh any state in ForbiddenStates. 
planWithForbiddenStates(_, Facts, Goals, [], _, Facts) :-
  satisfiedPlannerGoals(Facts, Goals), !. % red cut
planWithForbiddenStates(Namespace, Facts, Goals, [FirstAction|RestOfPlan], ForbiddenStates, FinalFacts) :-
  allowedActionExecution(Namespace, FirstAction, Facts, NewFacts),
  not(setIsOneOf(NewFacts, ForbiddenStates)),
  planWithForbiddenStates(Namespace, NewFacts, Goals, RestOfPlan, [NewFacts|ForbiddenStates], FinalFacts).

% planAStar_(++Namespace:atom, ++Heap:heap, +Goals:list, :Heuristic:atom, ++ForbiddenStates:list, -Plan:list, -PlanCost:number, -FinalFacts:list) is nondet
% 
% @arg Namespace This could be any atom. The Namespace used here should match the one used in "endDomainDefinition" and "endProblemDefinition".
%
%   True if Plan is a list of actions that make the planer Goals be satisfied from the initial facts. The halfPlanExecutions being computed are stored
% in the Heap. The initial facts should already be consistent with the information on the Heap. The Plan to be computed won't pass through any set of facts
% equivalent to those in ForbiddenStates. The Heuristic meta predicate determines the order in which halfPlanExecutions are computed (through heap priority)
% by associating costs to each action. That also determines the total PlanCost, which is returned. FinalFacts is the set of facts that remain after the plan
% is executed.
planAStar_(Namespace, Heap, Goals, Heuristic, ForbiddenStates, Plan, PlanCost, FinalFacts) :-
  min_of_heap(Heap, PlanCost, halfPlanExecution(HalfPlan, Facts)),
  satisfiedPlannerGoals(Facts, Goals),
  (
    Plan = HalfPlan,
    FinalFacts = Facts
  ;
    !, % red cut
    get_from_heap(Heap, _, _, PopedHeap),
    planAStar_(Namespace, PopedHeap, Goals, Heuristic, ForbiddenStates, Plan, PlanCost, FinalFacts)
  ).
planAStar_(Namespace, Heap, Goals, Heuristic, ForbiddenStates, Plan, PlanCost, FinalFacts) :-
  (
    var(PlanCost),
    openNonForbiddenActionsInHeap(Namespace, ForbiddenStates, Heuristic, -1, Heap, NewHeap, OpenedFacts)
  ;
    nonvar(PlanCost),
    openNonForbiddenActionsInHeap(Namespace, ForbiddenStates, Heuristic, PlanCost, Heap, NewHeap, OpenedFacts)
  ),
  \+ empty_heap(NewHeap),
  planAStar_(Namespace, NewHeap, Goals, Heuristic, [OpenedFacts|ForbiddenStates], Plan, PlanCost, FinalFacts).

% openNonForbiddenActionsInHeap(++Namespace:atom, ++ForbiddenStates:list, :Heuristic:atom, ++Heap:heap, -NewHeap:heap, -OpenedFacts:list) is det
% 
% @arg Namespace This could be any atom. The Namespace used here should match the one used in "endDomainDefinition" and "endProblemDefinition".
% @arg ForbiddenStates List of fact sets that were already opened in a previous heap opening. Plans that achieve the same set of facts shouldn't
%                      be added to heap again.
% @arg Heuristic Meta predicate that receives an actionExecution and returns its cost, so it can be added to the heap's planExecutions.
% @arg MaxCost Max cost the new halfPlanExecutions can have. -1 stands for infinity.
%
%   This assumes the Heap is not empty. Heap contains halfPlanExecution(HalfPlan, OpenedFacts). This predicate removes
% the highest priority entry (lowest value), computates all valid next actions and adds them to the Heap as new
% halfPlanExecutions. It returns the new heap in NewHeap and the facts that resulted from the opened halfPlanExecution
% in OpenedFacts.
openNonForbiddenActionsInHeap(Namespace, ForbiddenStates, Heuristic, MaxCost, Heap, NewHeap, OpenedFacts) :-
  get_from_heap(Heap, HalfPlanCost, halfPlanExecution(HalfPlan, OpenedFacts), PopedHeap),
  findall(actionExecution(Action, ObtainedFacts), allowedActionExecution(Namespace, Action, OpenedFacts, ObtainedFacts), AllActionExecutions),
  filterNonForbiddenActionExecutions(AllActionExecutions, ForbiddenStates, NonForbiddenActions),
  addPlanExecutionsToHeap(NonForbiddenActions, Heuristic, MaxCost, HalfPlanCost, HalfPlan, PopedHeap, NewHeap).

% filterNonForbiddenActionExecutions(++ActionExecutions:list, ++ForbiddenStates:list, -NonForbiddenActionExecutions:list) is det
% 
%   Receives a ActionExecutions list of actionExecution(Action:compound_term, ObtainedFacts:list). This checks whether
% each ObtainedFacts set has an equivalent set in ForbiddenStates. If has, that means that action can't be executed,
% so it is filtered out of that list. The new filtered list is returned in NonForbiddenActionExecutions.
filterNonForbiddenActionExecutions([], _, []).
filterNonForbiddenActionExecutions([actionExecution(_, ObtainedFacts)|RestOfActionExecutions], ForbiddenStates, NonForbiddenActionExecutions) :-
  setIsOneOf(ObtainedFacts, ForbiddenStates), !, % red cut
  filterNonForbiddenActionExecutions(RestOfActionExecutions, ForbiddenStates, NonForbiddenActionExecutions).
filterNonForbiddenActionExecutions([actionExecution(Action, ObtainedFacts)|RestOfActionExecutions], ForbiddenStates, [actionExecution(Action, ObtainedFacts)|RestOfNonForbiddenActionExecutions]) :-
  filterNonForbiddenActionExecutions(RestOfActionExecutions, ForbiddenStates, RestOfNonForbiddenActionExecutions).

% addPlanExecutionsToHeap(++ActionExecutions:list, :Heuristic:atom, ++MaxCost:number, ++HalfPlanCost:number, ++HalfPlan:list, ++Heap:heap, NewHeap:heap) is det
% 
%   Receives a Heap of halfPlanExecution(HalfPlan:list, ObtainedFacts:list) and an ActionExecutions list of
% actionExecution(Action:compound_term, ObtainedFacts:list). The meta predicate Heuristic is called for each actionExecution, retrieving
% the costOfAction of that action. Adds new halfPlanExecutions to the heap for each actionExecution, with priority equal to TotalCost =
% HalfPlanCost + costOfAction. It doesn't get added if it is greater than MaxCost. MaxCost == -1 stands for infinity, so there is no limit.
% Each new halfPlan is the HalfPlan with the Action as the head of the list (the plans are reversed). Note that the heap addition only
% happens if there is no entry in the heap which already contains an equivalent ObtainedFacts set and better cost (lower priority).
addPlanExecutionsToHeap([], _, _, _, _, Heap, Heap).
addPlanExecutionsToHeap([actionExecution(Action, ObtainedFacts)|RestOfActionExecutions], Heuristic, MaxCost, HalfPlanCost, HalfPlan, Heap, NewHeap) :-
  call(Heuristic, actionExecution(Action, ObtainedFacts), Cost),
  TotalCost is HalfPlanCost + Cost,
  (
    MaxCost == -1
  ;
    MaxCost \== -1,
    TotalCost =< MaxCost
  ),
  addHalfPlanToHeapIfBetterOrEqual(Heap, TotalCost, halfPlanExecution([Action|HalfPlan], ObtainedFacts), MiddleHeap),
  addPlanExecutionsToHeap(RestOfActionExecutions, Heuristic, MaxCost, HalfPlanCost, HalfPlan, MiddleHeap, NewHeap).

% addHalfPlanToHeapIfBetterOrEqual(++Heap:heap, ++HalfPlanCost:number, ++HalfPlanExecution:compound_term, -NewHeap:heap) is det
% 
%   Receives a Heap of halfPlanExecution(HalfPlan:list, ObtainedFacts:list) and tries to add HalfPlanExecution
% of priority HalfPlanCost. It searches the heap for an equivalent ObtainedFacts set. If there isn't one, then
% HalfPlanExecution is added. If there is one, then it looks at its cost and replaces that entry for
% HalfPlanExecution if HalfPlanCost is better costed (less than the one in the Heap). If the costs are equal,
% HalfPlanExecution is simply added and nothing is replaced or removed. The new heap is returned in NewHeap.
addHalfPlanToHeapIfBetterOrEqual(Heap, HalfPlanCost, HalfPlanExecution, NewHeap) :-
  empty_heap(Heap), !, % red cut
  add_to_heap(Heap, HalfPlanCost, HalfPlanExecution, NewHeap).
addHalfPlanToHeapIfBetterOrEqual(Heap, HalfPlanCost, halfPlanExecution(HalfPlan, Facts), NewHeap) :-
  get_from_heap(Heap, PopedCost, halfPlanExecution(PopedHalfPlan, PopedFacts), PopedHeap),
  (
    equivalentTo(PopedFacts, Facts), !, % red cut
    (
      HalfPlanCost < PopedCost, !, % green cut
      add_to_heap(PopedHeap, HalfPlanCost, halfPlanExecution(HalfPlan, Facts), NewHeap)
    ;
      HalfPlanCost > PopedCost, !, % green cut
      add_to_heap(PopedHeap, PopedCost, halfPlanExecution(PopedHalfPlan, PopedFacts), NewHeap)
    ;
      HalfPlanCost =:= PopedCost,
      PopedHalfPlan == HalfPlan, !, % green cut
      add_to_heap(PopedHeap, PopedCost, halfPlanExecution(PopedHalfPlan, PopedFacts), NewHeap)
    ;
      HalfPlanCost =:= PopedCost,
      PopedHalfPlan \== HalfPlan, !, % green cut
      add_to_heap(PopedHeap, PopedCost, halfPlanExecution(PopedHalfPlan, PopedFacts), MiddleHeap),
      add_to_heap(MiddleHeap, HalfPlanCost, halfPlanExecution(HalfPlan, Facts), NewHeap)
    )
  ;
    % \+ equivalentTo(PopedFacts, Facts)
    addHalfPlanToHeapIfBetterOrEqual(PopedHeap, HalfPlanCost, halfPlanExecution(HalfPlan, Facts), AddedHeap),
    add_to_heap(AddedHeap, PopedCost, halfPlanExecution(PopedHalfPlan, PopedFacts), NewHeap)
  ).