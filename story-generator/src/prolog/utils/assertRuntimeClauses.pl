:- module(assertRuntimeClauses, [beginAssertRuntimeClauses/2, beginAssertRuntimeClauses/3, endAssertRuntimeClauses/0, endAssertRuntimeClauses/1]).

:- use_module(getRuntimeClauses).

:- module_transparent([beginAssertRuntimeClauses/2, beginAssertRuntimeClauses/3]).

beginAssertRuntimeClauses(AssertingModule, Predicates) :-
  context_module(DefiningModule),
  beginAssertRuntimeClauses(DefiningModule, AssertingModule, Predicates, default).
beginAssertRuntimeClauses(AssertingModule, Predicates, Namespace) :-
  context_module(DefiningModule),
  beginAssertRuntimeClauses(DefiningModule, AssertingModule, Predicates, Namespace).

endAssertRuntimeClauses :-
  endAssertRuntimeClauses(default, no).
endAssertRuntimeClauses(Namespace) :-
  endAssertRuntimeClauses(Namespace, yes).

% Private

beginAssertRuntimeClauses(DefiningModule, AssertingModule, Predicates, Namespace) :-
  maplist(beginAssertAllPredicateClauses(DefiningModule, Namespace), Predicates),
  assert(assertRuntimeClauses:predicatesDef(DefiningModule, AssertingModule, Predicates, Namespace)).

beginAssertAllPredicateClauses(DefiningModule, Namespace, PredicateName/Arity) :-
  length(VarArgsLst, Arity),
  PredicateGenericTerm =.. [PredicateName|VarArgsLst],
  predicateNamespace(DefiningModule:PredicateName/Arity, Namespace, PredicateNamespace),
  beginGetRuntimeClauses(PredicateNamespace, DefiningModule:PredicateGenericTerm, true).

endAssertRuntimeClauses(Namespace, AssertWithNamespace) :-
  retract(assertRuntimeClauses:predicatesDef(DefiningModule, AssertingModule, Predicates, Namespace)),
  maplist(assertRuntimeClauses:endAssertAllPredicateClauses(DefiningModule, AssertingModule, Namespace, AssertWithNamespace), Predicates).

endAssertAllPredicateClauses(DefiningModule, AssertingModule, Namespace, AssertWithNamespace, PredicateName/Arity) :-
  predicateNamespace(DefiningModule:PredicateName/Arity, Namespace, PredicateNamespace),
  endGetRuntimeClauses(PredicateNamespace, Clauses),
  maplist(assertRuntimeClauses:reassertClauseInModule(AssertingModule, Namespace, AssertWithNamespace), Clauses).

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

predicateNamespace(Context:PredicateName/Arity, Namespace, PredicateNamespace) :-
  atomic_list_concat([Context, ':', PredicateName, '/', Arity, '--', Namespace], PredicateNamespace).