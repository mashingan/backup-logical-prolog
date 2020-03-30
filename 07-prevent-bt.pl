larger1(A, B, A) :- A > B.
larger1(_, B, B).

% when run:
% larger(8, 6, X).
% X = 8 ;
% X = 6
% but this is wrong

sum1to(1, 1).
sum1to(N, S) :- N1 is N-1, sum1to(N1, S1), S is S1 + N.
% when run:
% sum1to(3, S).
% S = 6 ;
% __crash__

larger(A, B, A) :- A > B.
larger(A, B, B) :- A =< B.

sumtoGood(1, 1).
sumtoGood(N, S) :- N > 1, N1 is N-1, sumtoGood(N1, S1), S is S1 + N.

largerCut(A, B, A) :- A > B, !.
largerCut(_, B, B).

sumtoCut(1, 1) :- !.
sumtoCut(N, S) :- N1 is N-1, sumto(N1, S1), S is S1 + N.

classify(0, zero).
classify(N, negative) :- N < 0.
classify(N, positive) :- N > 0.
go_example5 :-
    write(start), nl,
    repeat, write('enter a positive value: '), read(N),
    classify(N, positive),
    write('positive value is '), write(N), nl.

bird(sparrow).
bird(eagle).
bird(duck).
bird(crow).
bird(ostrich).
bird(puffin).
bird(swan).
bird(albatross).
bird(starling).
bird(owl).
bird(kingfisher).
bird(thrush).

can_fly(ostrich) :- !, fail.
can_fly(X) :- bird(X).

exfactorial(1, 1) :- !.
exfactorial(N, NFact) :-
    N1 is N-1,
    exfactorial(N1, NFact1), NFact is N * NFact1.
exercise1 :- exfactorial(6, N), write(N), nl.

exercise2 :-
    repeat, read_and_check(N, Type),
    write(N), write(' is '), write(Type), nl, N =:= 100.

oddEvenType(N, even) :- Mod is N mod 2, Mod =:= 0, !.
oddEvenType(_, odd).
read_and_check(N, Type) :-
    write('Enter next number: '),
    read(N), oddEvenType(N, Type).
