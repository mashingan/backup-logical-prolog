%:- debug.
readin :- get0(X), process(X).
process(42).
process(X) :- X =\= 42, write(X), nl, readin.

go(Total) :- count(0, Total).
count(OldCount, Result) :-
    get0(X), process(X, OldCount, Result).
process(42, OldCount, OldCount).
process(X, OldCount, Result) :-
    X =\= 42, New is OldCount + 1, count(New, Result).

go_vowels(V) :- count(vowels, 0, V).
count(vowels, Old, Result) :-
    get0(X), process(vowels, X, Old, Result).
process(vowels, 42, Old, Old).
process(vowels, X, Old, Result) :-
    X =\= 42, processChar(X, Old, New), count(vowels, New, Result).
processChar(X, Old, New) :- vowel(X), New is Old+1.
processChar(_, Old, Old).
vowel(65).
vowel(69).
vowel(73).
vowel(79).
vowel(85).
vowel(97).
vowel(101).
vowel(105).
vowel(111).
vowel(117).

% tell(+Stream): to change current output stream to +Stream
%   - any action of write, and writeq will be added to +Stream
% told/0: to revert the output stream back to stdout
% see(+Stream): to change current input stream to +Stream
%   - any action of get/1, get0/1, read/2 will use +Stream
% seen/0: to rever the input stream back to stdin

readline :- get0(X), processLine(X).
processLine(10).
processLine(X) :- X =\= 10, X =\= -1, put(X), nl, readline.

% get current stream with seeing and telling
% bind current stream to Infile and Outfile
% read/write operations
% close Infile and Outfile stream,
% bind current stream to previous stream
read4terms(Infile, Outfile) :-
    seeing(S), see(Infile), telling(T), tell(Outfile),
    read(T1), write(T1), nl, read(T2), write(T2), nl,
    read(T3), write(T3), nl, read(T4), write(T4), nl,
    seen, see(S), told, tell(T).

copychars(Outfile) :- telling(T), tell(Outfile), copyCharacters, told, tell(T).
copyCharacters :- get0(N), process(exclamation, N).
process(exclamation, 33).
process(exclamation, N) :- N =\= 33, put(N), copyCharacters.

% run it:
% makelower.
%   This is jUST Example of making ALL lower.
makelower :- get0(N), process_lower(N), !.
process_lower(10) :- nl.
process_lower(N) :-
    N =\= 10,
    (
        N >= 65, N =< 90, N2 is N+32;
        N2 = N
    ),
    put(N2), makelower.

% run it:
% copyterms('05_copyterms.txt', '05_termsout.txt').
copyterms(Infile, Outfile) :-
    seeing(S), see(Infile), telling(T), tell(Outfile),
    readFile, !,
    seen, see(S), told, tell(T).

readFile :- read(X), processTerms(X).
processTerms(end_of_file).
processTerms(X) :- X \== end_of_file, write(X), nl, readFile.

exercise2 :- copyterms('05_copyterms.txt', '05_termsout.txt').

read13testa :-
    seeing(S), see('testa.txt'),
    read13(0), !,
    seen, see(S).

read13(Count) :- get0(N), process13(Count, N).
process13(13, _).
process13(_, end_of_file).
process13(Count, N) :-
    Count =\= 13,
    N \== end_of_file,
    write('Count: '), write(Count), write(' '),
    IncCount is Count + 1,
    write('ASCII: '), write(N), write(' char: '), nl,
    read13(IncCount).

exercise3 :- read13testa.

% run it:
% combinefiles('in1.txt', 'in2.txt', 'combineout.txt').
combinefiles(In1, In2, Outfile) :-
    seeing(S), see(In1), seeing(S1), see(In2), seeing(S2),
    see(S1),
    telling(T), tell(Outfile),
    readSS(S1, S2), !,
    see(S1), seen, see(S2), seen, see(S), told, tell(T).

readSS(S1, S2) :- read(X), processSS(S1, S2, X).
processSS(_, _, end).
processSS(_, _, end_of_file).
processSS(S1, S2, X) :-
    X \== end, X \== end_of_file,
    write(X), nl, see(S2), readSS(S2, S1).

exercise4 :- combinefiles('in1.txt', 'in2.txt', 'combineout.txt').

% run it:
% comparefiles('in1.txt', 'in2.txt').
comparefiles(In1, In2) :-
    seeing(S), see(In1), seeing(S1), see(In2), seeing(S2),
    see(S1),
    readcompare(S1, S2), !,
    see(S1), seen, see(S2), seen, see(S).

readcompare(S1, S2) :-
    read(X), see(S2), read(Y), see(S1),
    processCompare(X, Y, S1, S2).
processCompare(end_of_file, _, _, _).
processCompare(_, end_of_file, _, _).
processCompare(end, _, _, _).
processCompare(_, end, _, _).
processCompare(X, Y, S1, S2) :-
    X \== end_of_file, X \== end, Y \== end_of_file, Y \== end,
    writeq(X),
    (
        X \== Y, write(' is different with ');
        X == Y, write(' is same with ')
    ),
    writeq(Y), nl,
    readcompare(S1, S2).

exercise5 :- comparefiles('in1.txt', 'in2.txt').
