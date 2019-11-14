:-  module(set, [isSet/1, elementOf/2, subsetOf/2, equivalentTo/2, setIsOneOf/2, setDiff/3, setUnion/3]).

/**
 * <module> Set
 * 
 * This module declares predicates to deal with the set data structure.
 * A set is represented by a prolog list which doesn't have repeated elements.
 * The order of the elements in the list doesn't matter, which means [a, b] and
 * [b, a] are equivalent.
 * 
 * @author Rafael Rubim Cabral
 * @version 0.1.0
 * 
 * @see Docs model: https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/pldoc.html%27)
 **/

% -------------------- Public predicates

%! isSet(++Set:list) is semidet
%
% True if Set does not contain repeated elements
isSet([]).
isSet([H|T]) :-
  not(elementOf(H, T)),
  isSet(T).

%! elementOf( ?Element:any, ++Set:list) is nondet
%
% True if Element belongs to Set. ?Element returns each set element.
elementOf(H, [H|_]).
elementOf(X, [_|T]) :-
  elementOf(X, T).

%! setDiff(++SetA:list, ++SetB:list, --Diff:list) is multi
%
% --Diff returns the set difference SetA \ SetB.
setDiff(S, [], S) :- !. % green cut
setDiff(S, [H|T], Z) :-
  setWithout(S, H, Y),
  setDiff(Y, T, Z).

%! setUnion(++SetA:list, ++SetB:list, --Union:list) is multi
%
% --Union returns the set union SetA U SetB.
setUnion([], S, S).
setUnion([H|T], S, Z) :-
  setWith(S, H, Y),
  setUnion(T, Y, Z).

%! subsetOf( ?Subset:list, ++Set:list) is nondet
% 
% True if every element in Subset belongs to Set (without repeating).
% ?Subset returns every sequence of each subset of Set.
subsetOf([], _).
subsetOf([H|T], X) :-
  elementOf(H, X),
  setWithout(X, H, Z),
  subsetOf(T, Z).

%! equivalentTo( ?SetA:list, ++SetB:list) is nondet
%
% True if every element in SetA belongs to SetB (without repeating),
% with no one left. ?SetA returns every sequence of SetB.
equivalentTo([], []).
equivalentTo([H|T], X) :-
  elementOf(H, X),
  setWithout(X, H, Z),
  equivalentTo(T, Z).

%! setIsOneOf( ?Set:list, ++SetList:list) is nondet
%
% True if Set is equivalent to any set in SetList. Equivalence is the
% the same defined by "equivalentTo". ?Set returns every sequence of
% each equivalent set in SetList.
setIsOneOf(S, [H|_]) :-
  equivalentTo(S, H).
setIsOneOf(S, [H|T]) :-
  setIsOneOf(S, T),
  H \= S.

% -------------------- Private predicates

% setWith(++Set:list, ++Element:any, --SetWithElement:list) is semidet
%
% True if SetWithElement is Set with inclusion of Element, unless Element
% already belonged to Set.
setWith(S, X, S) :-
  elementOf(X, S), !. % red cut
setWith(T, H, [H|T]).

% setWithout( +Set:list,  ?Element:any,  -SetWithoutElement:list) is nondet
%
% True if SetWithoutElement is Set with first match of Element removed.
% SetWithoutElement equals Set if there is no such match.
% ?Element returns all elements in Set which match Element, but doesn't repeat
% matching instantiations (if one instantiation returned would match another
% element in Set, that element will not be returned).
setWithout([], _, []).
setWithout([H|T], H, T).
setWithout([H|T], X, [H|Z]) :-
  setWithout(T, X, Z),
  H \= X.