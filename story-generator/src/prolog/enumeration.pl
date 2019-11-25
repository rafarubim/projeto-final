:- module(enumeration, [beginEnumsDefinition/0, endEnumsDefinition/0, enumValue/3]).

% enumSpec(?EnumName:atom, -EnumValues:list) is nondet
%
% True if . Value and complement can't be variables.

:- use_module('utils/assertRuntimeTerms').

:- module_transparent([beginEnumsDefinition/0, endEnumsDefinition/0]).

:- dynamic enumSpec/2.

beginEnumsDefinition :-
  beginAssertRuntimeTerms(enumeration, [enumSpec/2]).

endEnumsDefinition :-
  endAssertRuntimeTerms.

enumSpec(color, [black, red, blue, yellow, green, white, grey, pink, orange, purple]).
enumSpec(cardSuit, [
  value(clubs, black),
  value(hearts, red),
  value(spades, black),
  value(diamonds, red)
]).

% enumValue(?Enum:atom, ?Value:atom, -Complement:any) is nondet
%
% True if Enum is an enumeration and Value is one of its values with complement
% Complement (no complement is "nil").
enumValue(Enum, Value, Complement) :-
  enumSpec(Enum, ValueList),
  member(EnumValue, ValueList),
  valueAndComplement(EnumValue, Value, Complement).

% valueAndComplement(+StoredValue:compound_term, -Value:atom, -Complement:any) is det
%
% True if StoredValue is just a Value atom and Complement is "nil" or if StoredValue
% a "value" tuple with Value and Complement.
valueAndComplement(value(Value, Complement), Value, Complement) :- !. % red cut
valueAndComplement(Value, Value, nil).