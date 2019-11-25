:- module(trigger, [beginTriggersDefinition/0, endTriggersDefinition/0]).

% triggerSpec(?TriggerName:atom) is nondet
%
% True if TriggerName is a trigger.

:- use_module('utils/assertRuntimeTerms').

:- module_transparent([beginTriggersDefinition/0, endTriggersDefinition/0]).

:- dynamic triggerSpec/1.

beginTriggersDefinition :-
  beginAssertRuntimeTerms(trigger, [triggerSpec/1]).

endTriggersDefinition :-
  endAssertRuntimeTerms.

triggerSpec(createChar).
triggerSpec(createPlace).