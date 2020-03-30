airobot :-
    repeat, inputline([], L), name(S, L), write('S='), write(S), nl,
    S = 'STOP', write('FINISHED'), nl, !.

inputline(OldL, L) :- get0(X), process(X, OldL, L).
process(10, L, L).
process(X, OldL, NewL) :-
    X =\= 10, append(OldL, [X], L2), inputline(L2, NewL).

control_robot :- initialise, report, repeat, inputline2(L), L = [stop], !.

inputline2(L) :- buildlist(L, []), reverse(L, L1), writeout(L1), !.

writeout(['']).
writeout(L) :-
    %writeq(L), nl,
    (
        verify(L), X =.. L, write(X), nl, call(X);
        write('Invalid input'), nl
    ).

buildlist(L, OldL) :-
    findword(Word, []),
    (
        Word = [], L = OldL;
        Word = [sep], buildlist(L, OldL);
        Word = [termin|Word1], name(S, Word1), L = [S|OldL];
        name(S, Word), buildlist(L, [S|OldL])
    ).

findword(Word, OldWord) :-
    get0(X1), repchar(X1, X),
    (
        terminator(X), Word = [termin|OldWord];
        separator(X), (OldWord = [], Word = [sep]; Word = OldWord);
        append(OldWord, [X], New), findword(Word, New)
    ).

separator(32).
terminator(10).

repchar(X, New) :- X >= 65, X =< 90, New is X+32, !.
repchar(C, C).

stop :- write('End of input'), nl.

verify([H|_]) :- member(H, [forward, back, turn, goto, face, report, stop]).

initialise :-
    retractall(orientation(_)),
    retractall(position(_, _)),
    assertz(position(0, 0)),
    assertz(orientation(90)).

infopos(N) :-
    write('** New orientation is '), write(N),
    write(' degrees anticlockwise from East'), nl.

turn(right) :- turn(90, degrees, clockwise).
turn(left) :- turn(90, degrees, anticlockwise).
turn(round) :- turn(180, degrees, anticlockwise).
turn(N, degrees, clockwise) :-
    N1 is -1*N,
    turn(N1, degrees, anticlockwise).
turn(N, degrees, anticlockwise) :-
    retract(orientation(Current)),
    New is (Current+N) mod 360,
    assertz(orientation(New)),
    infopos(New).

face(N, degrees) :-
    retract(orientation(_)),
    assertz(orientation(N)),
    infopos(N).

report :-
    position(North, East),
    write('** Position is '),
    round2dp(North, North1), round2dp(East, East1),
    write(North1),
    write(' meters North and '), write(East1),
    write(' meters East'), nl,
    orientation(N),
    infopos(N).

round2dp(X, Y) :- Y is round(X*100) / 100.

infopos(North, East) :-
    write('** New position is '),
    write(North), write(' meters North and '),
    write(East), write(' meters East'), nl.

goto(North, north, East, east) :-
    retract(position(_, _)),
    assertz(position(North, East)),
    write('** New position is '),
    infopos(North, East).

back(N, meters) :-
    turn(180, degrees, anticlockwise),
    forward(N, meters).

forward(N, meters) :-
    retract(position(North, East)),
    orientation(Degrees), radians(Degrees, Rads),
    North1 is North+N*sin(Rads),
    East1 is East+N*cos(Rads),
    assertz(position(North1, East1)),
    round2dp(North1, North2), round2dp(East1, East2),
    infopos(North2, East2).

radians(N, M) :- M is (3.14159265)*N/180.

%===========
getTitle :-
    read(T),
    T \== end_of_file,
    write('Title:'), writeq(T), nl,
    Title =.. [title, T],
    write(Title), nl,
    assertz(Title).

%getQuestions :- repeat, getQA(T, _), T = endofquestions.

%getQA(T, Ans) :-
getQuestion :-
    read(T),
    T \== endquestions,
    write('T:'), writeq(T), nl,
    getAnswers(Ans), !,
    write('Ans:'), writeq(Ans), nl,
    assertz(question(T, Ans)), getQuestion.
getQuestion.

getAnswers(Ans) :-
    retrieveAns([], Ans).
retrieveAns(Curr, Ans) :-
    read(Answer),
    Answer \== end,
    read(Mark),
    NewAns = ans(Answer, Mark),
    retrieveAns([NewAns|Curr], Ans).
retrieveAns(Ans, Ans).

cleartest :-
    see('quiz1.txt'), seeing(S), set_stream(S, line_position(0)), seen,
    retractall(title(_)),
    retractall(question(_, _)),
    retractall(question(_, _, _)), % this is from setup
    retractall(range(_, _, _)).

getRanges :-
    read(First), First \== endmarkscheme,
    read(Last), read(Feedback), !,
    assertz(range(First, Last, Feedback)), getRanges.
getRanges.

testcurr :-
    cleartest,
    seeing(S), see('quiz1.txt'),
    getTitle, getQuestion,
    getRanges,
    seen, see(S).

setup :-
    see('quiz1.txt'), readin, seen, write('Setup completed'), nl.

readin :- read(Title), assertz(title(Title)), readqs.

readqs :-
    repeat, read(Qtext), processQ(Qtext),
    Qtext = endquestions, readranges.

processQ(endquestions) :- !.
processQ(Qtext) :-
    proc2([], Anslist, -9999, MaxScore),
    assertz(question(Qtext, Anslist, MaxScore)).

proc2(Anscurrent, Anslist, Maxsofar, Maxscore) :-
    read(Ans),
    (
        Ans = end, Anslist = Anscurrent, Maxscore = Maxsofar, !;
        read(Score), append(Anscurrent, [ans(Ans, Score)], Ansnew),
        Maxnew is max(Maxsofar, Score),
        proc2(Ansnew, Anslist, Maxnew, Maxscore)
    ).

readranges :-
    repeat, read(First), proc(First), First = endmarkscheme.

proc(endmarkscheme) :- !.
proc(First) :-
    read(Last), read(Feedback), assertz(range(First, Last, Feedback)).
