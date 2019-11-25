:- use_module(state).
:- use_module(event).
:- use_module(entity).

% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Genre Definition

% --------------------------------- Entity types
:- beginEntityTypesDefinition.

categorySpec(man, character).
categorySpec(woman, character).
categorySpec(knight, character).
categorySpec(prince, man).
categorySpec(horse, animal).
categorySpec(building, place).

:- endEntityTypesDefinition.

% --------------------------------- State types
:- beginStateTypesDefinition.

signatureSpec(builtIn, [
  entityArg(building),
  entityArg(place)
]).

:- endStateTypesDefinition.

% --------------------------------- Events
/*
:- beginEventTypesDefinition.



:- endEventTypesDefinition.
*/
% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Story limitation

% --------------------------------- Entities
:- beginEntitiesDefinition.

entitySpec(cassandra, woman).
entitySpec(horace, prince).
entitySpec(fields, place).
entitySpec(city, place).
entitySpec(palace, building).

:- endEntitiesDefinition.

% --------------------------------- States
:- beginStatesDefinition.

builtIn(a, b).
builtIn(a, b, c).
whatever(s).
knowsThat(p, q).

:- endStatesDefinition.