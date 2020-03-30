loopCount(0).
loopCount(N) :-
    N>0, write('The value is: '), write(N), nl,
    M is N - 1, loopCount(M).

output_values(Last, Last) :-
    write(Last), nl, write('End of example.'), nl.
output_values(Count, Last) :-
    Count =\= Last, write(Count), nl,
    N is Count + 1, output_values(N, Last).

sumto(1, 1).
sumto(N, S) :- N > 1, N1 is N-1, sumto(N1, S1), S is S1 + N.

writesquare(1) :- write(1), nl.
writesquare(N) :-
    N > 1, N1 is N - 1, Nsq is N * N,
    writesquare(N1),
    write(Nsq), nl.

loopEnd(end).
loopEnd(X) :-
    X \== end, write('Type end to end: '), read(Word),
    write('Input was: '), write(Word), nl, loopEnd(Word).

loopOrEnd :-
    write('Type end. to end: '), read(Word),
    write('Input was: '), write(Word), nl,
    (
        Word == end;
        loop
    ).

yesno(Ans) :- write('Enter answer to question'), nl, yesno2(Ans).
yesno2(Ans) :-
    write('Answer yes or no: '),
    read(A),
    ((valid(A), Ans = A, write('Answer is '),
        write(A), nl);
        yesno2(Ans)).
valid(yes).
valid(no).

repeatYesNo(Ans) :-
    write('Enter answer to question'), nl,
    repeat, write('Answer yes or no: '), read(Ans),
    valid(Ans), write('Answer is '), write(Ans), nl.

readTermsToEnd(Infile) :-
    seeing(S), see(Infile),
    repeat, read(X), write(X), nl, (X == end; X == end_of_file),
    seen, see(S).

goMenu :- write('This shows how a repeated menu works'), nl, menugo.
menugo :-
    nl, write('MENU'), nl,
    write('a. Activity A'), nl, write('b. Activity B'), nl,
    write('c. Activity C'), nl, write('d. End.'), nl,
    read(Choice), nl, menuchoice(Choice).

menuchoice(a) :- write('Activity A chosen'), menugo.
menuchoice(b) :- write('Activity B chosen'), menugo.
menuchoice(c) :- write('Activity C chosen'), menugo.
menuchoice(d) :- write('Good bye!'), nl.
menuchoice(_) :- write('Please try again!'), menugo.

dog(fido).
dog(fred).
dog(jonathan).
alldogs :- dog(X), write(X), write(' is a dog'), nl, fail.
alldogs.

person(john, smith, 45, london, doctor).
person(martin, williams, 33, birmingham, teacher).
person(henry, smith, 26, manchester, plumber).
person(jane, wilson, 62, london, teacher).
person(mary, smith, 29, glasgow, surveyor).
allteachers :-
    person(Forename, Surname, _, _, teacher),
    write(Forename), write(' '), write(Surname), nl,
    fail.
allteachers.

somepeople(Famname) :-
    person(Forename, Surname, _, _, _),
    write(Forename), write(' '), write(Surname), nl,
    Surname == Famname.
somepeople(_).

someWilliams :- somepeople(williams).

exercise1 :- squares(6, 12), !.
writesqr(N) :-
    Sqr is N * N,
    write(N), write(' squares: '), write(Sqr), nl.
squares(To, To) :-
    writesqr(To).
squares(From, To) :-
    From < To,
    Next is From + 1,
    writesqr(From),
    squares(Next, To).

exercise2 :- get0(N), processBackward(N), !.
processBackward(10) :- nl.
processBackward(N) :-
    N =\= 10,
    put(N),
    exercise2.

exercise3 :- professionOver40.
professionOver40 :-
    write('All person profession which age more than 60'), nl,
    person(_, _, N, _, Profession),
    N > 40,
    write('Profession: '), write(Profession), nl,
    fail.
professionOver40.
