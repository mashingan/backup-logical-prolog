dog(fido).
dog(rover).
dog(jane).
dog(tom).
dog(fred).
dog(henry).

cat(mike).
cat(mary).
cat(harry).
cat(felix).
cat(bill).
cat(steve).

large(fido).
large(mary).
large(tom).
large(fred).
large(steve).
large(jim).
large(mike).

small(henry).

large_animal(X) :-
    cat(X),
    large(X), !.
large_animal(X) :-
    dog(X),
    large(X).

writechase(X, Y) :-
    write(X), write(' chases '), write(Y), nl.

chases(X, Y) :-
    dog(X), cat(Y),
    writechase(X, Y), !.

chases(X, Y) :-
    large(X), not(large(Y)),
    writechase(X, Y).

animal(mammal, tiger, carnivore, stripes).
animal(mammal, hyena, carnivore, ugly).
animal(mammal, lion, carnivore, mane).
animal(mammal, zebra, herbivore, stripes).
animal(bird, eagle, carnivore, large).
animal(bird, sparrow, scavenger, small).
animal(reptile, snake, carnivore, long).
animal(reptile, lizard, scavenger, small).

mammals(X):- animal(mammal, X, _, _).
mammal_carnivore(X):- animal(mammal, X, carnivore, _).
reptile_stripe(X):- animal(reptile, X, _, mane).

person(bill, male).
person(george, male).
person(alfred, male).
person(carol, female).
person(margaret, female).
person(jane, female).

coupling(X, Y):-
    person(X, male), person(Y, female).
