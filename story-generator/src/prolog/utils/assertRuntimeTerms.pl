:- module(assertRuntimeTerms, [beginAssertRuntimeTerms/2, beginAssertRuntimeTerms/3, endAssertRuntimeTerms/0, endAssertRuntimeTerms/1]).

/**
 * <module> Assert Runtime Terms
 * 
 *   This module is meant to be used similarly to the "getRuntimeClauses" module, in the module definition.
 * It declares two predicates. The "beginAssertRuntimeTerms" predicate receives a list of name/arity
 * predicate descriptions (e.g. [length/2]). It will start capturing all terms associated to these predicates
 * which are defined in the runtime. It also receives an AssertionModule. After endAssertRuntimeTerms is called,
 * all terms captured will be asserted again in the module "AssertionModule". An example follows below:
 * 
 * ---------- yourModule.pl ----------
 * 
 * :- use_module(assertRuntimeTerms).
 * 
 * :- beginAssertRuntimeTerms(myModule2, [termA/2, termB/0]).
 * 
 * termA(a, b).
 * termA(c).
 * termB.
 * 
 * :- endAssertRuntimeTerms.
 * 
 * :- beginAssertRuntimeTerms(myModule2, [termC/2, termD/0], namespace).
 * 
 * termC(a, b).
 * termC(c).
 * termD.
 * 
 * :- endAssertRuntimeTerms(namespace).
 * 
 * ------ end of yourModule.pl -------
 *   It will also work if you use multiple calls to "beginAssertRuntimeTerms" simultaneously with different AssertingModules
 * (e.g. "myModule2") or different Namespaces (e.g. "namespace"). Just make sure you have a corresponding "endAssertRuntimeTerms"
 * call with the same AssertinModule and Namespace for each one.
 *   One important note: when you pass an extra argument to the predicates (e.g. namespace), that namespace will be added as the first
 * argument of the asserted terms. that means the exemple above will assert the following predicates in "myModule2":
 * 
 * % As a result of not passing "namespace":
 * termA(a, b).
 * termB.
 * 
 * % As a result of passing "namespace":
 * termA(namespace, a, b).
 * termB(namespace).
 * 
 *   Note that the module context which calls "beginAssertRuntimeTerms" is important, because each name/arity predicate is kind of
 * a meta_predicate. So if you're going to create your own predicate which delegates its call to yet another module, then you should
 * make that predicate inherit the caller's module context with "module_transparent". An example follows below:
 * 
 *  ---------- yourModule.pl ----------
 * 
 * :- module_transparent([beginDelegation/2]).
 * 
 * beginDelegation(Namespace) :-
 *   beginAssertRuntimeTerms(yourModule, [localPredicate/2], Namespace).
 *
 * ------ end of yourModule.pl -------
 * 
 * @author Rafael Rubim Cabral
 * @version 0.1.0
 * 
 * Docs model:
 * @see https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/pldoc.html%27)
 **/

:- use_module(getRuntimeClauses).

:- module_transparent([beginAssertRuntimeTerms/2, beginAssertRuntimeTerms/3]).

% -------------------- Public predicates

%! beginAssertRuntimeTerms(++AssertingModule:atom, ++Predicates:list) is det
%! beginAssertRuntimeTerms(++AssertingModule:atom, ++Predicates:list, ++Namespace:atom) is det
%
% Begins the runtime assertion of all terms which are in Predicates list. They are asserted in the AssertingModule module.
% If the Namespace arg is present, the terms are asserted with it as an extra first argument.
beginAssertRuntimeTerms(AssertingModule, Predicates) :-
  context_module(DefiningModule),
  beginAssertRuntimeTerms(DefiningModule, AssertingModule, Predicates, default).
beginAssertRuntimeTerms(AssertingModule, Predicates, Namespace) :-
  context_module(DefiningModule),
  beginAssertRuntimeTerms(DefiningModule, AssertingModule, Predicates, Namespace).

%! endAssertRuntimeTerms is det
%! endAssertRuntimeTerms(++Namespace:atom) is det
%
% Ends the runtime assertion of all terms defined by the corresponding call to "beginAssertRuntimeTerms". The Namespace arg should
% should the present if it is in the corresponding call.
endAssertRuntimeTerms :-
  endAssertRuntimeTerms(default, no).
endAssertRuntimeTerms(Namespace) :-
  endAssertRuntimeTerms(Namespace, yes).

% -------------------- Private predicates

% beginAssertRuntimeTerms(++DefiningModule:atom, ++AssertingModule:atom, ++Predicates:list, ++Namespace:atom) is det
%
% Begins the runtime assertion of all terms which are in Predicates list.
beginAssertRuntimeTerms(DefiningModule, AssertingModule, Predicates, Namespace) :-
  maplist(beginAssertAllPredicateTerms(DefiningModule, AssertingModule, Namespace), Predicates),
  assert(assertRuntimeTerms:predicatesDef(DefiningModule, AssertingModule, Predicates, Namespace)).

% beginAssertAllPredicateTerms(++DefiningModule:atom, ++AssertingModule:atom, ++Namespace:atom, ++PredicateDesc:predicate_description) is det
%
% Calls "beginGetRuntimeClauses" for the clauses associated to PredicateDesc.
beginAssertAllPredicateTerms(DefiningModule, AssertingModule, Namespace, PredicateName/Arity) :-
  length(VarArgsLst, Arity),
  PredicateGenericTerm =.. [PredicateName|VarArgsLst],
  predicateNamespace(DefiningModule:PredicateName/Arity, AssertingModule, Namespace, PredicateNamespace),
  beginGetRuntimeClauses(PredicateNamespace, DefiningModule:PredicateGenericTerm, true).

% endAssertRuntimeTerms(++Namespace:atom, ++AssertWithNamespace) is det
%
% Ends the runtime assertion of all terms which were in the predicate list of "beginAssertRuntimeTerms". It passes Namespace as an extra first argument of these
% terms if AssertWithNamespace is "yes".
endAssertRuntimeTerms(Namespace, AssertWithNamespace) :-
  retract(assertRuntimeTerms:predicatesDef(DefiningModule, AssertingModule, Predicates, Namespace)),
  maplist(assertRuntimeTerms:endAssertAllPredicateTerms(DefiningModule, AssertingModule, Namespace, AssertWithNamespace), Predicates).

% endAssertAllPredicateTerms(++DefiningModule:atom, ++AssertingModule:atom, ++Namespace:atom, ++AssertWithNamespace:atom, ++PredicateDesc:predicate_description) is det
%
% Calls "endGetRuntimeClauses" for the clauses associated to PredicateDesc, then asserts them in AssertingModule (with Namespace as an extra first argument if
% AssertWithNamespace is "yes").
endAssertAllPredicateTerms(DefiningModule, AssertingModule, Namespace, AssertWithNamespace, PredicateName/Arity) :-
  predicateNamespace(DefiningModule:PredicateName/Arity, AssertingModule, Namespace, PredicateNamespace),
  endGetRuntimeClauses(PredicateNamespace, Clauses),
  maplist(assertRuntimeTerms:reassertClauseInModule(AssertingModule, Namespace, AssertWithNamespace), Clauses).

% reassertClauseInModule(++AssertingModule:atom, ++Namespace:atom, ++AssertWithNamespace:atom, +Clause:clause) is det
%
% @arg AssertWithNamespace This should be the atom "yes" or "no".
%
% Asserts Clause in AssertingModule. If AssertWithNamespace is "yes", then Namespace is asserted a Clause's head
% extra argument.
reassertClauseInModule(AssertingModule, Namespace, yes, Head:-Body) :-
  !, % green cut
  Head =.. [Functor|Args],
  NewHead =.. [Functor, Namespace|Args],
  AssertingModule:retractall(NewHead:-Body),
  AssertingModule:assert(NewHead:-Body).
reassertClauseInModule(AssertingModule, _, no, Clause) :-
  !, % green cut
  AssertingModule:retractall(Clause),
  AssertingModule:assert(Clause).

% predicateNamespace(++Predicate:predicate_description, ++AssertingModule:atom, ++Namespace:atom, -PredicateNamespace:atom) is det
%
% True if PredicateNamespace is the namespace associated to the predicate (used in beginGetRuntimeClauses/
% endGetRuntimeClauses).
predicateNamespace(DefiningModule:PredicateName/Arity, AssertingModule, Namespace, PredicateNamespace) :-
  atomic_list_concat([DefiningModule, ':', PredicateName, '/', Arity, '--', AssertingModule, '--', Namespace], PredicateNamespace).