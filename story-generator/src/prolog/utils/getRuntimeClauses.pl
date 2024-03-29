:- module(getRuntimeClauses, [beginGetRuntimeClauses/3, endGetRuntimeClauses/2]).

/**
 * <module> Get Runtime Clauses
 * 
 *   This module declares two predicates to be run on other module definitions. It captures all clauses you want
 * defined by the other module that are between these two calls in the runtime. This can be used similarly
 * to begin_tests and end_tests of the "plunit" library. An example follows below:
 * 
 * ---------- yourModule.pl ----------
 * 
 * :- use_module(getRuntimeClauses).
 * 
 * :- beginGetRuntimeClauses(namespace, clauseHead(a, _), clauseBody(_)).
 * 
 * clauseHead(a, b) :- clauseBody(c). % Is caught by "Get Runtime Clauses".
 * clauseHead(a) :- clauseBody(b). % Is not caught (the head doesn't unify with the one defined in "beginGetRuntimeClauses").
 * clauseHead(X, Y) :- clauseBody([X, Y]). % Is caught.
 * 
 * :- endGetRuntimeClauses(namespace, Clauses), write(Clauses). 
 * % This will print a list with both clauses which were caught (it renames variables):
 * % "[(clauseHead(a,b):-clauseBody(c)),(clauseHead(_4796,_4798):-clauseBody([_4796,_4798]))]"
 * 
 * ------ end of yourModule.pl -------
 * 
 *   This will also work if you use multiple calls to "beginGetRuntimeClauses" simultaneously with different
 * namespaces. Just make sure you have a corresponding "endGetRuntimeClauses" call with the same namespace
 * for each one.
 *   Note that the clause "happy(john)." is a clause with Head = "happy(john)" and Body = "true".
 *   Note that the module context which calls "beginGetRuntimeClauses" is important, because it is a meta_predicate.
 * So if you're going to create your own predicate which delegates its call to yet another module, then you should
 * also make that predicate a meta_predicate, with a Head meta argument. If your predicate doesn't receive a Head argument,
 * You can use "module_transparent" and the "context_module" predicate instead.
 * An example follows below:
 * 
 *  ---------- yourModule.pl ----------
 * 
 * :- meta_predicate beginDelegationWhichReceivesHead(+, :).
 * :- module_transparent([beginDelegationWhichDoesntReceiveHead/2]).
 * 
 * beginDelegationWhichReceivesHead(Namespace, Head) :-
 *   beginGetRuntimeClauses(Namespace, Head, true).
 *
 * beginDelegationWhichDoesntReceiveHead(Namespace, Body) :-
 *   context_module(Context),
 *   beginGetRuntimeClauses(Namespace, Context:_, Body).
 *
 * ------ end of yourModule.pl -------
 * 
 * @author Rafael Rubim Cabral
 * @version 0.1.0
 * 
 * Docs model:
 * @see https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/pldoc.html%27)
 **/

:- use_module(apply).
:- use_module(set, [setDiff/3]).

% clauseDef(++Namespace:atom, -Head:any, -Body:any) is nondet
%
% True if "beginGetRuntimeClauses" was called with the same parameters and
% "endGetRuntimeClauses" wasn't yet called with the corresponding Namespace.
:- dynamic clauseDef/3.

% refs(++Namespace:atom, -Refs:list) is nondet
%
% True if "beginGetRuntimeClauses" was called with Namespace and Refs is a list
% of all the refs of the clauses in the calling module that matched "beginGetRuntimeClauses" 
% args. Also only true if "endGetRuntimeClauses" wasn't yet called with the
% corresponding Namespace.
:- dynamic refs/2.

:- meta_predicate beginGetRuntimeClauses(+,:,+).

% -------------------- Public predicates

%! beginGetRuntimeClauses(++Namespace:atom, +Head:any, +Body:any) is det
%
% @arg Namespace This could be any atom. The Namespace used here should also be
%                used by the corresponding call to "endGetRuntimeClauses".
% @arg Head An expression that should match the head of clauses you're interested in.
% @arg Body An expression that should match the body of clauses you're interested in.
%
% Begins watching clause definition that matches (Head :- Body). All clauses
% defined in the runtime after this is called and before "endGetRuntimeClauses"
% is called are kept in a list which is returned by "endGetRuntimeClauses".
beginGetRuntimeClauses(Namespace, Head, Body) :-
  findall(Ref, clause(Head, Body, Ref), Refs),
  assert(getRuntimeClauses:clauseDef(Namespace, Head, Body)),
  assert(getRuntimeClauses:refs(Namespace, Refs)).

%! endGetRuntimeClauses(++Namespace:atom, -Clauses:list) is det
%
% @arg Namespace This could be any atom. The Namespace used here should match
%                the one used by the corresponding call to "beginGetRuntimeClauses".
% @arg Clauses List of all the clauses caught since "beginGetRuntimeClauses" was called.
%
% True if Clauses is a list of all the runtime-defined clauses caught
% since the call to "beginGetRuntimeClauses". The list is ordered in the clause
% definition order.
endGetRuntimeClauses(Namespace, Clauses) :-  
  retract(getRuntimeClauses:clauseDef(Namespace, Head, Body)),
  retract(getRuntimeClauses:refs(Namespace, OldRefs)),
  findall(Ref, clause(Head, Body, Ref), NewRefs),
  setDiff(NewRefs, OldRefs, Refs), !, % green cut
  maplist(getRuntimeClauses:refToClause, Refs, Clauses).

% -------------------- Private predicates

% refToClause(++Ref, -Clause) is det
%
% @arg Ref Reference to a clause
% @arg Clause Clause that corresponds to the ref
%
% True if Clause is the clause referenced by Ref
refToClause(Ref, Clause) :-
  clause(_:Head, Body, Ref),
  (Clause) = (Head :- Body).
