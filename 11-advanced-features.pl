?-op(700, xfx, iss).
?-op(150, xf, !).
?-op(200, yfx, ++).
?-op(200, fx, head).
?-op(200, fx, tail).

factorial(1, 1).
factorial(N, R) :-
    N1 is N-1,
    factorial(N1, RR),
    R is RR * N, !.

join2(S1, S2, SR) :-
    name(S1, L1), name(S2, L2),
    append(L1, L2, RL), name(SR, RL).

convert(X, X) :- atom(X).
convert(X, X1) :- X1 iss X.

Y iss N! :- N1 iss N, factorial(N1, Y), !.
Y iss A+B:- A1 iss A, B1 iss B, Y is A1+B1,!.
Y iss A-B:- A1 iss A, B1 iss B, Y is A1-B1,!.
Y iss A*B:- A1 iss A, B1 iss B, Y is A1*B1,!.
Y iss A/B:- A1 iss A, B1 iss B, Y is A1/B1,!.
Y iss A//B:- A1 iss A, B1 iss B, Y is A1//B1,!.
Y iss A^B:- A1 iss A, B1 iss B, Y is A1^B1,!.
Y iss +A:- Y iss A,!.
Y iss -A:- A1 iss A, Y is -A1,!.
Y iss sqrt(A) :- A1 iss A, Y is sqrt(A1), !.
Y iss abs(A) :- A1 iss A, Y is abs(A1), !.
Y iss sin(A) :- A1 iss A, Y is sin(A1), !.
Y iss cos(A) :- A1 iss A, Y is cos(A1), !.
Y iss tan(A) :- A1 iss A, Y is tan(A1), !.
Y iss A**B :- A1 iss A, B1 iss B, Y is A1*A1+B1*B1, !.
%Y iss head X :- X1 iss X, X1 = [Y|_], !.
%Y iss tail X :- X1 iss X, X1 = [_|Y], !.
Y iss head X :- X = [Y|_], !.
Y iss tail X :- X = [_|Y], !.
S iss S1++S2 :-
    convert(S1, A), convert(S2, B),
    join2(A, B, S), !.
X iss Y :- X is Y, !.

?-op(710, xfx, sis).
?-op(200, yfx, and).
?-op(200, yfx, or).

Y sis A and B :-
    A1 sis A, B1 sis B,
    findall(X, (member(X, A1), member(X, B1)), Y), !.
Y sis A or B :-
    A1 sis A, B1 sis B,
    findall(X, (member(X, A1); member(X, B1), not(member(X, A1))), Y), !.
Y sis A-B :-
    A1 sis A, B1 sis B,
    findall(X, (member(X, A1), not(member(X, B1))), Y), !.
A sis A :- !.

munify(CT1, CT2) :-
    functor(CT1, Func1, Ar1),
    functor(CT2, Func2, Ar2),
    compare(CT1, CT2, Func1, Ar1, Func2, Ar2).
compare(_, _, F, 0, F, 0) :- !.
compare(_, _, _, 0, _, 0) :- fail.
compare(CT1, CT2, F, A, F, A) :-
    munify2(CT1, CT2), !.
%compare(F, _, F, _) :- fail.

munify2(CT1, CT2) :-
    CT1 =..[F|L1],
    CT2 =..[F|L2], !,
    paircheck(L1, L2).
paircheck([], []).
paircheck([A|L1], [A|L2]) :- paircheck(L1, L2).

addArg(Term, NewArg, Res) :-
    Term =.. [F|Arg],
    append(Arg, [NewArg], Arg1),
    Res =.. [F|Arg1].
