/* Animals program 1 */
dog(fido).
dog(rover).
dog(tom).
dog(henry).
cat(mary).
cat(harry).
cat(bill).
cat(felix).
cat(steve).
animal(X) :- dog(X).

animals(lion).
animals(tiger).
animals(cow).

carnivore(lion).
carnivore(tiger).
