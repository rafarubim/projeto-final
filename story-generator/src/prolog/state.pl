:- module(state, [beginStateTypesDefinition/0, endStateTypesDefinition/0, beginStatesDefinition/0, endStatesDefinition/0, respectsSignature/1, allStates/1, allStateTypes/1, removeStates/1, addStates/1]).

stateTypeSpec(standsIn, [
  entityArg(character),
  entityArg(place)
]).
stateTypeSpec(standsIn, [
  entityArg(thing),
  entityArg(place)
]).
stateTypeSpec(liesIn, [
  entityArg(thing),
  entityArg(place)
]).
stateTypeSpec(distanceInKilometers, [
  entityArg(place),
  entityArg(place),
  scalarArg(number)
]).
stateTypeSpec(ownedBy, [
  entityArg(thing),
  entityArg(character)
]).
stateTypeSpec(ownedBy, [
  entityArg(animal),
  entityArg(character)
]).
stateTypeSpec(isHolding, [
  entityArg(character),
  entityArg(thing)
]).
stateTypeSpec(knowsPerson, [
  entityArg(character),
  entityArg(character)
]).
stateTypeSpec(knowsWhere, [
  entityArg(character),
  entityArg(place)
]).
stateTypeSpec(knowsThat, [
  entityArg(character),
  stateArg
]).
stateTypeSpec(knowsThat, [
  entityArg(character),
  eventArg
]).
stateTypeSpec(personalityOf, [
  entityArg(character),
  scalarArg(number),
  scalarArg(number),
  scalarArg(number),
  scalarArg(number),
  scalarArg(number)
]).
stateTypeSpec(relationshipBetween, [
  entityArg(character),
  entityArg(character),
  scalarArg(number),
  scalarArg(number),
  scalarArg(number),
  scalarArg(number),
  scalarArg(number)
]).

:- use_module(entity).
:- use_module(event).
:- use_module(enumeration).

% stateTypeSpec(?StateTypeName, ?ArgsSpec).
%
% entityArg(entityType)
% scalarArg(scalarType)
% stateArg(stateType)
% eventArg(eventType)

% 'stateFunctor'(...).

:- use_module('utils/apply').
:- use_module('utils/assertRuntimeTerms').

:- module_transparent([beginStateTypesDefinition/0, endStateTypesDefinition/0, beginStatesDefinition/0, endStatesDefinition/0]).

:- dynamic stateTypeSpec/2.
:- dynamic knows/2.
:- dynamic standsIn/2.
:- dynamic distanceInKilometers/3.
:- dynamic knowsThat/2.
:- dynamic hasColor/2.

beginStateTypesDefinition :-
  beginAssertRuntimeTerms(state, [stateTypeSpec/2]).

endStateTypesDefinition :-
  endAssertRuntimeTerms,
  findall(_, (state:stateTypeSpec(StateTypeName, ArgsSpec), length(ArgsSpec, ArgsAmt), (dynamic StateTypeName/ArgsAmt)), _).

beginStatesDefinition :-
  findall(StateTypeName/ArgsAmt, (state:stateTypeSpec(StateTypeName, ArgsSpec), length(ArgsSpec, ArgsAmt)), StatePredicates),
  beginAssertRuntimeTerms(state, StatePredicates).

endStatesDefinition :-
  endAssertRuntimeTerms.

respectsSignature(State) :-
  respectsSignature(State, _).

allStates(States) :-
  findall(
    StateTerm,
    (
      state:stateTypeSpec(StateTypeName, ArgsSpec),
      length(ArgsSpec, ArgsAmt),
      length(VarArgs, ArgsAmt),
      StateTerm =.. [StateTypeName|VarArgs],
      StateTerm
    ),
    States
  ).

allStateTypes(StateTypes) :-
  findall(
    signature(StateTypeName, ArgsSpec),
    (
      state:stateTypeSpec(StateTypeName, ArgsSpec)
    ),
    StateTypes
  ).

removeStates(States) :-
  maplist(retractall, States).

addStates(States) :-
  maplist(assert, States).

% respectsSignature(?State:term, -StateTypeName:atom) is nondet
respectsSignature(State, StateTypeName) :-
  var(State),
  stateTypeSpec(StateTypeName, ArgsSpec),
  maplist(argFromType, Args, ArgsSpec),
  State =.. [StateTypeName|Args].
respectsSignature(State, StateTypeName) :-
  nonvar(State),
  State =.. [StateTypeName|Args],
  stateTypeSpec(StateTypeName, ArgsSpec),
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
argFromType(event(EvSignature, OcurrenceTime), eventArg) :-
  event(EvSignature, OcurrenceTime).
argFromType(event(EvSignature, OcurrenceTime), eventArg(EvType)) :-
  EvSignature =.. [EvType|_],
  event(EvSignature, OcurrenceTime).

isMetastate(StateTypeName) :-
  stateTypeSpec(StateTypeName, ArgsSpec),
  containsMetaArg(ArgsSpec).

isNotMetastate(StateTypeName) :-
  bagof(ArgsSpec, stateTypeSpec(StateTypeName, ArgsSpec), AllArgSpecs),
  maplist(doesNotContainMetaArg, AllArgSpecs).

doesNotContainMetaArg(ArgsSpec) :-
  \+ containsMetaArg(ArgsSpec).

containsMetaArg(ArgsSpec) :-
  contains_term(stateArg, ArgsSpec).
containsMetaArg(ArgsSpec) :-
  contains_term(stateArg(_), ArgsSpec).