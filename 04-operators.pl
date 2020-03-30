:- discontiguous(isa_cat).
:- discontiguous(isa_dog).

?-op(150, xfy, likes).
?-op(150, xf, is_female).
?-op(150, xf, isa_cat).
?-op(150, xfy, owns).

john likes X :- X is_female, X owns Y, Y isa_cat.

mary is_female.
mary owns fido.
%fido isa_cat.

% X + Y
% X - Y
% X * Y
% X / Y
% X // Y    quotient
% X mod Y   remainder
% X ^ Y
% -X
% abs(X)
% sin(X)
% cos(X)
% max(X, Y)
% round(X)
% sqrt(X)

increase(N, M) :- M is N + 1.

checkeven(N) :- M is N//2, N =:= 2*M.

dog(fido).
dog(rover).
dog(tom).
dog(fred).

cat(mary).
cat(jane).
cat(harry).
cat(henry).
cat(bill).
cat(steve).

small(jane).
small(tom).
small(rover).

large(fido).
large(mary).
large(fred).
large(steve).
large(henry).
large(jim).
large(mike).

large_dog(X) :- dog(X), large(X).
small_animal(A) :- dog(A), small(A).
small_animal(B) :- cat(B), small(B).
chases(X, Y) :-
    large_dog(X), small_animal(Y),
    write(X), write(' chases '), write(Y), nl.

?-op(150, xf, isa_dog).
?-op(150, xf, is_large).
?-op(150, xf, is_small).
?-op(150, xf, is_small_animal).
?-op(150, xf, is_large_dog).
?-op(150, xfy, chasing).

fido isa_dog.
rover isa_dog.
tom isa_dog.
fred isa_dog.

mary isa_cat.
jane isa_cat.
harry isa_cat.
henry isa_cat.
bill isa_cat.
steve isa_cat.

rover is_small.
jane is_small.
tom is_small.

fido is_large.
mary is_large.
fred is_large.
steve is_large.
henry is_large.
jim is_large.
mike is_large.

X is_large_dog :- X isa_dog, X is_large.
X is_small_animal :-
    (
        X isa_cat;
        X isa_dog
    ),
    X is_small.
X chasing Y :-
    X is_large_dog, Y is_small_animal,
    write(X), write(' chasing '), write(Y), nl.

prompt(X, Y, Label) :-
    write(X), write(' and '), write(Y), write(' '), write(Label), write(': ').

numop(X, Y) :-
    prompt(X, Y, average), Average is (X + Y) / 2, write(Average), nl,
    prompt(X, Y, sqrt_prod), SqrtProd is sqrt(X*Y), write(SqrtProd), nl,
    prompt(X, Y, max), Max is max(X, Y), write(Max), nl.
