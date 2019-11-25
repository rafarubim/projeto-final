:- module(state, [beginStateTypesDefinition/0, endStateTypesDefinition/0, beginStatesDefinition/0, endStatesDefinition/0, respectsSignature/1]).

:- use_module(entity).
:- use_module(enumeration).

% signatureSpec(?StateTypeName, ?ArgsSpec).
%
% entityArg(entityType)
% scalarArg(scalarType)
% stateArg(stateType)
% eventArg(eventType)

% 'stateFunctor'(...).

:- use_module('utils/assertRuntimeTerms').

:- module_transparent([beginStateTypesDefinition/0, endStateTypesDefinition/0, beginStatesDefinition/0, endStatesDefinition/0]).

:- dynamic signatureSpec/2.

beginStateTypesDefinition :-
  beginAssertRuntimeTerms(state, [signatureSpec/2]).

endStateTypesDefinition :-
  endAssertRuntimeTerms.

beginStatesDefinition :-
  findall(StateTypeName/ArgsAmt, (state:signatureSpec(StateTypeName, ArgsSpec), length(ArgsSpec, ArgsAmt)), StatePredicates),
  beginAssertRuntimeTerms(state, StatePredicates).

endStatesDefinition :-
  endAssertRuntimeTerms.

respectsSignature(State) :-
  respectsSignature(State, _).

signatureSpec(knows, [
  entityArg(character),
  entityArg(character)
]).
signatureSpec(standsIn, [
  entityArg(character),
  entityArg(place)
]).
signatureSpec(distanceInKilometers, [
  entityArg(place),
  entityArg(place),
  scalarArg(number)
]).
signatureSpec(knowsThat, [
  entityArg(character),
  stateArg
]).

% respectsSignature(?State:term, -StateTypeName:atom) is nondet
respectsSignature(State, StateTypeName) :-
  var(State),
  signatureSpec(StateTypeName, ArgsSpec),
  maplist(argFromType, Args, ArgsSpec),
  State =.. [StateTypeName|Args].
respectsSignature(State, StateTypeName) :-
  nonvar(State),
  State =.. [StateTypeName|Args],
  signatureSpec(StateTypeName, ArgsSpec),
  maplist(argFromType, Args, ArgsSpec).

% argFromType(?Arg, ++ArgType) is nondet
argFromType(Arg, entityArg) :-
  entity(Arg).
argFromType(Arg, entityArg(Classification)) :-
  entityClassification(Arg, Classification).
argFromType(Arg, scalarArg) :-
  argFromType(Arg, scalarArg(_)).
argFromType(Arg, scalarArg(number)) :-
  number(Arg).
argFromType(Arg, scalarArg(EnumName)) :-
  EnumName \== number,
  enumValue(EnumName, Arg, _).
argFromType(Arg, stateArg) :-
  argFromType(Arg, stateArg(_)).
argFromType(Arg, stateArg(StateTypeName)) :-
  isNotMetastate(StateTypeName),
  respectsSignature(Arg, StateTypeName).

isMetastate(StateTypeName) :-
  signatureSpec(StateTypeName, ArgsSpec),
  containsMetaArg(ArgsSpec).

isNotMetastate(StateTypeName) :-
  signatureSpec(StateTypeName, ArgsSpec),
  \+ containsMetaArg(ArgsSpec).

containsMetaArg(ArgsSpec) :-
  contains_term(stateArg, ArgsSpec).
containsMetaArg(ArgsSpec) :-
  contains_term(stateArg(_), ArgsSpec).