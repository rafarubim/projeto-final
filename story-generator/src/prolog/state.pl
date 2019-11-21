:- module(state, [respectsSignature/2]).

:- use_module(entity).
:- use_module(enumeration).

% signatureSpec(?StateTypeName, ?ArgsSpec).
signatureSpec(knows, [
  entityArg(character),
  entityArg(character)
]).
signatureSpec(distanceInKilometers, [
  entityArg(place),
  entityArg(place),
  scalarArg(number)
]).
signatureSpec(standsIn, [
  entityArg(building),
  entityArg(place)
]).
signatureSpec(hasColor, [
  entityArg(owl),
  scalarArg(color)
]).
signatureSpec(knowsThat, [
    entityArg(character),
    stateArg
  ]).

containsMetaArg(ArgsSpec) :-
  contains_term(stateArg, ArgsSpec).
containsMetaArg(ArgsSpec) :-
  contains_term(stateArg(_), ArgsSpec).

isMetastate(StateTypeName) :-
  signatureSpec(StateTypeName, ArgsSpec),
  containsMetaArg(ArgsSpec).

isNotMetastate(StateTypeName) :-
  signatureSpec(StateTypeName, ArgsSpec),
  \+ containsMetaArg(ArgsSpec).

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