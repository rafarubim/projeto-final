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
 *   Note that the module context in which "beginGetRuntimeClauses" and "endGetRuntimeClauses"
 * run is important, so if you're going to create your own predicates which delegate their call to
 * yet another module, then you should make these predicates transparent with "module_transparent".
 * An example follows below:
 * 
 *  ---------- yourModule.pl ----------
 * 
 * :- module_transparent([beginDelegationWhichOnlyCaresAboutHead/2, endDelegationWhichOnlyCaresAboutHead/2]).
 * 
 * beginDelegationWhichOnlyCaresAboutHead(Namespace, Head) :-
 *  beginGetRuntimeClauses(Namespace, Head, true).
 *
 * endDelegationWhichOnlyCaresAboutHead(Namespace, Clauses) :-
 *  endGetRuntimeClauses(Namespace, Clauses).
 *
 * ------ end of yourModule.pl -------
 * 
 * @author Rafael Rubim Cabral
 * @version 0.1.0
 * 
 * Docs model:
 * @see https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/pldoc.html%27)
 **/

:- use_module(set, [setDiff/3]).

:- dynamic clauseDef/3.
:- dynamic refs/2.
:- module_transparent([beginGetRuntimeClauses/3, endGetRuntimeClauses/2]).

beginGetRuntimeClauses(Namespace, Head, Body) :-
  findall(Ref, clause(Head, Body, Ref), Refs),
  assert(getRuntimeClauses:clauseDef(Namespace, Head, Body)),
  assert(getRuntimeClauses:refs(Namespace, Refs)).

endGetRuntimeClauses(Namespace, Clauses) :-  
  retract(getRuntimeClauses:clauseDef(Namespace, Head, Body)),
  retract(getRuntimeClauses:refs(Namespace, OldRefs)),
  findall(Ref, clause(Head, Body, Ref), NewRefs),
  setDiff(NewRefs, OldRefs, Refs),
  maplist(getRuntimeClauses:mapClauses, Refs, Clauses).

mapClauses(Ref, Clause) :-
  clause(_:Head, Body, Ref),
  (Clause) = (Head :- Body).
