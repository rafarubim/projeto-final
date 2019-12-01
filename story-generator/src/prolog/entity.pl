:- module(entity, [beginEntityTypesDefinition/0, endEntityTypesDefinition/0, beginEntitiesDefinition/0, endEntitiesDefinition/0, type/1, category/1, entity/1, entityClassification/2, allEntities/1, beginRemoveNativeEntityTypes/0, endRemoveNativeEntityTypes/0]).

typeSpec(thing).
typeSpec(character).
typeSpec(place).
typeSpec(animal).

% typeSpec(?Type:atom) is nondet

% categorySpec(?Category, -Superclassification) is nondet

% entitySpec(?Entity:atom, -Superclassification:atom) is nondet

:- use_module('utils/getRuntimeClauses').
:- use_module('utils/assertRuntimeTerms').
:- use_module('utils/apply').

:- module_transparent([beginEntityTypesDefinition/0, endEntityTypesDefinition/0, beginEntitiesDefinition/0, endEntitiesDefinition/0, beginRemoveNativeEntityTypes/0, endRemoveNativeEntityTypes/0]).

:- dynamic typeSpec/1.
:- dynamic categorySpec/2.
:- dynamic entitySpec/2.

beginEntityTypesDefinition :-
  beginAssertRuntimeTerms(entity, [typeSpec/1, categorySpec/2]).

endEntityTypesDefinition :-
  endAssertRuntimeTerms.

beginEntitiesDefinition :-
  beginAssertRuntimeTerms(entity, [entitySpec/2]).

endEntitiesDefinition :-
  endAssertRuntimeTerms.

beginRemoveNativeEntityTypes :-
  beginGetRuntimeClauses("entity-removeNativeEntityType/1", removeNativeEntityType(_), true).

endRemoveNativeEntityTypes :-
  endGetRuntimeClauses("entity-removeNativeEntityType/1", Clauses),
  maplist(entity:retractNativeEntityType, Clauses).

retractNativeEntityType(removeNativeEntityType(NativeType):-true) :-
  retractall(typeSpec(NativeType)).

% type(?Type:atom) is nondet
type(X) :-
  typeSpec(X).

% category(?Category:atom) is nondet
category(Category) :-
  categorySpec(Category, _).

% entity(?Entity:atom) is nondet
entity(Entity) :-
  entitySpec(Entity, _).

% categoryClassification(?Category, ?Classification) is nondet
categoryClassification(Category, Classification) :-
  categorySpec(Category, Classification).
categoryClassification(Category, Classification) :-
  categorySpec(Category, Superclassification),
  categoryClassification(Superclassification, Classification).

allEntities(Entities) :-
  findall(
    Entity,
    (
      entity:entity(Entity)
    ),
    Entities
  ).

% entityClassification(?Entity, ?Classification) is nondet
entityClassification(Entity, Superclassification) :-
  entitySpec(Entity, Superclassification).
entityClassification(Entity, Classification) :-
  entitySpec(Entity, Superclassification),
  categoryClassification(Superclassification, Classification).
