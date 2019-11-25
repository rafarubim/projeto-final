:- module(entity, [beginEntityTypesDefinition/0, endEntityTypesDefinition/0, beginEntitiesDefinition/0, endEntitiesDefinition/0, type/1, category/1, entity/1, entityClassification/2]).

% typeSpec(?Type:atom) is nondet

% categorySpec(?Category, -Superclassification) is nondet

% entitySpec(?Entity:atom, -Superclassification:atom) is nondet

:- use_module('utils/assertRuntimeTerms').

:- module_transparent([beginEntityTypesDefinition/0, endEntityTypesDefinition/0, beginEntitiesDefinition/0, endEntitiesDefinition/0]).

:- dynamic typeSpec/1.
:- dynamic categorySpec/1.
:- dynamic entitySpec/2.

beginEntityTypesDefinition :-
  beginAssertRuntimeTerms(entity, [typeSpec/1, categorySpec/2]).

endEntityTypesDefinition :-
  endAssertRuntimeTerms.

beginEntitiesDefinition :-
  beginAssertRuntimeTerms(entity, [entitySpec/2]).

endEntitiesDefinition :-
  endAssertRuntimeTerms.

typeSpec(thing).
typeSpec(character).
typeSpec(place).
typeSpec(relationship).
typeSpec(animal).

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

% entityClassification(?Entity, ?Classification) is nondet
entityClassification(Entity, Superclassification) :-
  entitySpec(Entity, Superclassification).
entityClassification(Entity, Classification) :-
  entitySpec(Entity, Superclassification),
  categoryClassification(Superclassification, Classification).
