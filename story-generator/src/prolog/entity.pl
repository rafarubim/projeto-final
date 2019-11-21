:- module(entity, [type/1, category/1, entity/1, entityClassification/2]).

% typeSpec(?Type:atom) is nondet

% categorySpec(?Category, -Superclassification) is nondet

% entitySpec(?Entity:atom, -Superclassification:atom) is nondet

typeSpec(thing).
typeSpec(character).
typeSpec(place).
typeSpec(relationship).
typeSpec(animal).

categorySpec(man, character).
categorySpec(woman, character).
categorySpec(prince, man).
categorySpec(owl, animal).
categorySpec(building, place).

entitySpec(merlin, man).
entitySpec(arthur, prince).
entitySpec(hedwig, owl).
entitySpec(fields, place).
entitySpec(city, place).
entitySpec(palace, building).

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
