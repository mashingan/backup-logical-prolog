sentence([s1, both, V, NP1, Noun1, NP2, Noun2]) -->
    noun_phrase(NP1, _, Noun1),
    verb(both, V),
    noun_phrase(NP2, _, Noun2),
    {assertz(wordlist(verb, both, V))}.
sentence([s2, Plurality, V, NP1, Noun1, NP2, Noun2]) -->
    noun_phrase(NP1, Plurality, Noun1),
    verb(Plurality, V),
    noun_phrase(NP2, _, Noun2),
    {assertz(wordlist(verb, Plurality, V))}.
%sentence([s3, both, V, NP1, Noun, Adv]) -->
sentence(L) -->
    noun_phrase(NP1, _, Noun),
    verb_sequence(both, V, Adv),
    {
    assertz(wordlist(verb, both, V)),
    LL = [s3, both, V, NP1, Noun],
    (
        var(Adv), L = LL;
        append(LL, [Adv], L)
    )
    }.
%sentence([s4, Plurality, V, NP1, Noun, Adv]) -->
sentence(L) -->
    noun_phrase(NP1, Plurality, Noun),
    verb_sequence(Plurality, V, Adv),
    {
    assertz(wordlist(verb, Plurality, V)),
    LL = [s4, Plurality, V, NP1, Noun],
    (
        var(Adv), L = LL;
        append(LL, [Adv], L)
    )
    }.

noun_phrase(np1, Plurality, N) -->
    determiner, adjective_sequence, noun(Plurality, N).
noun_phrase(np2, Plurality, N) -->
    determiner, noun(Plurality, N).
noun_phrase(np3, Plurality, N) -->
    noun(Plurality, N).

/*
verb(both) --> [sat].
verb(both) --> [saw].
verb(both) --> [heard].
verb(both) --> [took].
verb(both) --> [will_see].

verb(singular) --> [hears].
verb(singular) --> [sees].

verb(plural) --> [hear].
verb(plural) --> [see].
*/

adverbs(X) --> [X], {member(X, [well, badly, quickly, slowly])}.

verb_sequence(Plurality, X, Adv) --> verb(Plurality, X), adverbs(Adv).
verb_sequence(Plurality, X, _) --> verb(Plurality, X).

verb(both, X) --> [X], {member(X, [sat,saw,heard,took,will_see])}.
verb(singular, X) --> [X], {member(X, [hears, sees])}.
verb(plural, X) --> [X], {member(X, [hear, see])}.

/*
determiner --> [the].
determiner --> [a].
determiner --> [an].
*/
determiner --> [X], {member(X, [the,a,an])}.

/*
noun(singular) --> [cat].
noun(singular) --> [dog].
noun(singular) --> [man].
noun(singular) --> [boy].
noun(singular) --> [mat].

noun(plural) --> [cats].
noun(plural) --> [dogs].
noun(plural) --> [men].
noun(plural) --> [boys].
noun(plural) --> [mats].
*/
noun(singular, X) --> [X],{member(X,[cat,dog,man,boy,mat])}.
noun(plural, X) --> [X],{member(X,[cats,dogs,men,boys,mats])}.

adjective_sequence --> adjective, adjective_sequence.
adjective_sequence --> adjective.

/*
adjective --> [X], {adjective_is(X)}.

adjective_is(large).
adjective_is(small).

adjective --> [brown].
adjective --> [orange].
adjective --> [green].
adjective --> [blue].
*/
adjective --> [X], {member(X, [large,small,brown,orange,green,blue])}.

%?- phrase(sentence, [the,cat,saw,the,mat]).
%true ; false.
%?- phrase(sentence, [the,cat,mat]).
%false.
%?- phrase(sentence, [the,cat,saw,the,X]).
%X = cat;
%X = dog;
%X = man;
%X = boy;
%X = mat;
%false.

readlineF(File) :-
    seeing(S), see(File),
    repeat, inputline(L), L = [end_of_file], !,
    seen, see(S).

inputline(L) :- buildlist(L, []), reverse(L, L1), writeout(L1), !.

writeout([]).
writeout([end_of_file]).
%writeout(L) :- write('Sentence: '), write(L), nl.
writeout(L) :- writeq(L), write('.'), nl.

buildlist(L, OldL) :-
    findword(Word, []),
    (
        Word = [], L = OldL;
        Word = [end_of_file], L = [end_of_file];
        Word = [sep], buildlist(L, OldL);
        Word = [termin|Word1], name(S, Word1), L = [S|OldL];
        name(S, Word), buildlist(L, [S|OldL])
    ).

findword(Word, OldWord) :-
    get0(X1), repchar(X1, X),
    (
        terminator(X), Word = [termin|OldWord];
        separator(X), (OldWord = [], Word = [sep]; Word = OldWord);
        X < 0, Word = [end_of_file];
        append(OldWord, [X], New), findword(Word, New)
    ).

repchar(C, Cout) :- C >= 65, C =< 90, Cout is C+32, !.
repchar(C, C).

%separator(X) :- member(X, [10, 32, 44, 58]).
%terminator(X) :- member(X, [46, 33, 63]).
separator(10).  % end of line
separator(32).  % space
separator(44).  % comma
separator(58).  % colon
terminator(46). % full stop
terminator(33). % exclamation mark
terminator(63). % question mark

readoutF(File, Out) :-
    telling(T), tell(Out),
    readlineF(File), !,
    told, tell(T).

processS(File) :-
    seeing(S), see(File),
    repeat, read(T), proc(T), T = end_of_file, !,
    seen, see(S).
proc(end_of_file).
proc(S) :- write('Sentence: '), write(S), nl, proc2(S).
proc2(S) :- phrase(sentence(L1), S), write('Structure: '), write(L1), nl, nl, !.
proc2(_) :- write('Invalid sentence structure'), nl, nl.
