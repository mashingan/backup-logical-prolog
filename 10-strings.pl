%built-in: name/2
%name('Prolog', L).
%L = [80, 114, 111, 108, 111, 103].

join2(Str1, Str2, NewStr) :-
    name(Str1, L1), name(Str2, L2),
    append(L1, L2, NewList),
    name(NewStr, NewList).
join3(S1, S2, S3, RStr) :-
    join2(S1, S2, SS),
    join2(SS, S3, RStr).

trim([H|T], L1) :- H =< 32, trim(T, L1).
trim(L, L) :- L = [A|_], A > 32.

trim2(L, L1) :- reverse(L, Lrev), trim(Lrev, L2), reverse(L2, L1).
trim3(L, L1) :- trim(L, L2), trim2(L2, L1).

times(S, Snew) :- name(S, L), trim3(L, L1), name(Snew, L1).

readline(S) :- readline1([], L), name(S, L), !.
readline1(Oldlist, L) :-
    get0(X), processRLine(Oldlist, X, L).
processRLine(Oldlist, 10, Oldlist).
processRLine(Oldlist, X, L) :-
    append(Oldlist, [X], L1), readline1(L1, L).

readlineF(File, S) :-
    seeing(See), see(File),
    readline1([], L), name(S, L), !,
    seen, see(See).

separate(L, Before, After) :- append(Before, [32|After], L), !.
findnext(L) :- separate(L, Before, After), procfind(Before, After).
findnext(L) :- write('Last item is '), name(S, L), write(S), nl.
procfind(Before, After):-
    write('Next item is '), name(S, Before), write(S), nl,
    findnext(After).
splitup(S) :- name(S, L), findnext(L), !.

startlist(L1, L2) :- append(L1, _, L2).
includedlist(_, []) :- !, fail.
includedlist(L1, L2) :- startlist(L1, L2).
includedlist(L1, [_|L2]) :- includedlist(L1, L2).

checkit(L, Plist, present) :- includedlist(Plist, L).
checkit(_, _, absent).

checkprolog(X) :-
    readline(S), name(S, L),
    name('Prolog', Plist), checkit(L, Plist, X), !.

splits(Str, Sep, Sep, Right) :-
    name(Sep, L1), name(Str, L3),
    append(L1, L2, L3), name(Right, L2), !.
splits(Str, Sep, L, Sep) :-
    name(Sep, L2), name(Str, L3),
    append(L1, L2, L3), name(L, L1), !.
splits(Str, Sep, Left, Right) :-
    name(Str, L3), append(Lleft, Lrest, L3),
    name(Sep, L4), append(L4, Lright, Lrest),
    name(Left, Lleft), name(Right, Lright), !.
splits(Str, _, Str, '') :- !.

remove_spaces(S, S1) :-
    splits(S, ' ', Sleft, Sright),
    remove2(S, Sleft, Sright, S1), !.
remove2(_, ' ', Sright, S1) :- remove_spaces(Sright, S1).
remove2(S, _, _, S).

spalindrome(S) :-
    writeq(S), write(' is palindrom: '),
    name(S, L), reverse(L, LL),
    (
        L == LL, write('yes'), nl;
        write('no'), nl
    ), !.

exercise1 :-
    spalindrome('abcd dcba'),
    spalindrome('xyz').

remove_final(S, SL) :-
    name(S, LL), reverse(LL, Lrev),
    discardspaces(Lrev, Lrev2),
    reverse(Lrev2, L), name(SL, L).
discardspaces(L, L).
discardspaces([32|T], L) :- discardspaces(T, L).

exercise2 :-
    remove_final('hello world  ', X1),
    remove_final('hello world', X2),
    X1 == X2, !.

replacefront('', '').
replacefront(S, SL) :-
    name(S, L), L \== [], L = [_|T],
    L2 = [63|T],
    name(SL, L2).

exercise3:- replacefront('abcde', X), X == '?bcde'.
