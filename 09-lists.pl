write_all([]).
write_all([H|T]) :- write(H), nl, write_all(T).

write_english([]).
write_english([[City, england]|L]) :-
    write(City), nl,
    write_english(L), !.
write_english([_|L]) :- write_english(L), !.
example1 :-
    write_english([[london,england],[paris,france],[berlin,germany],
    [portsmouth, england],[bristol,england],[edinburgh,scotland]]).

replace([_|L], [first|L]).

%built-in:
%member:
%member(a, [a, b, c]). true
%member(mypred(a, b, c), [q, r, s, mypred(a, b, c), w]). true
%member(x, []). false
%member([1,2,3], [a, b, [1, 2, 3], c]). true
%member(X, [a, b, c]).
%X = a ;
%X = b ;
%X = c ;
%false.

get_ans2(Ans) :-
    repeat,
    write('answer yes, no, or maybe: '), read(Ans),
    member(Ans, [yes, no, maybe]),
    write('answer is '), write(Ans), nl, !.

%built-in: length
%length([a, b, c, d], X). X = 4.
%length([[a, b, c], [d, e, f], [g, h, i]], L). L = 3.
%length([], L). L = 0.

%built-in: reverse
%reverse([a, b, c, d], X). X = [d, c, b, a].
%reverse([[a, b, c], [d, e, f], [g, h, i]], L). L = [[g, h, i], [d, e, f], [a, b, c]].
%reverse([], L). L = [].

front(L1, L2) :-
    reverse(L1, L3), remove_head(L3, L4), reverse(L4, L2).
remove_head([_|L], L).

%front([a, b, c], L). L = [a, b].
%front([[a, b, c], [d, e, f], [g, h, i]], L). [[a, b, c], [d, e, f]].

%built-in: append
%append([1, 2, 3, 4], [5, 6, 7, 8], L). L=[1,2,3,4,5,6,7,8].
%append([], [1,2,3], L). L=[1,2,3].
%append([[a,b,c],d,e,f],[g,h[i,j,k]], L). L=[[a,b,c],d,e,f,g,h,[i,j,k]].
%check:
%append(X, Y, [1,2,3,4]).
%X=[], Y=[1,2,3,4];
%X=[1], Y=[2,3,4];
%X=[1,2], Y=[3,4];
%X=[1,2,3], Y=[4];
%X=[1,2,3,4], Y=[];
%false
%append(X,[Y|Z], [1,2,3,4]).
%X=[], Y=1, Z=[2,3,4];
%X=[1], Y=2, Z=[3,4];
%X=[1,2], Y=3, Z=[4];
%X=[1,2,3], Y=4, Z=[];
%false

find_largest([X|Tail], MaxVal) :-
    find_largest_util(Tail, MaxVal, X).
find_largest_util([], X, X).
find_largest_util([H|T], MaxVal, CurrentLargest) :-
    H > CurrentLargest,
    find_largest_util(T, MaxVal, H), !.
find_largest_util([_|T], MaxVal, CurrentLargest) :-
    %H =< CurrentLargest,
    find_largest_util(T, MaxVal, CurrentLargest), !.

butlast([_], []).
butlast([X|Y], [X|Z]) :- butlast(Y, Z).

mmember(X, [X|_]).
mmember(X, [_|L]) :- mmember(X, L).
mreverse(L1, L2) :- mrev(L1, [], L2).
mrev([], L, L).
mrev([X1|Y1], CurrList, L2) :-
    mrev(Y1, [X1|CurrList], L2).
mappend([], L, L).
mappend([H|T], L2, [H|L3]) :-
    mappend(T, L2, L3).

setup :-
    seeing(S), see('people.txt'),
    read_data, !,
    write('Data read'), nl,
    seen, see(S).
read_data :-
    read(A), process(A).
process(end).
process(A) :-
    read(B), read(C), read(D), read(E),
    assertz(person(A, B, C, D, E)),
    read_data.

%built-in: findall/3
%findall(S, person(_, S, _, _), L).
%L = [smith, williams, smith, wilson, smith].
%findall(fullname(F,S), person(F,S,_,_,_), L).
%L = [fullname(john, smith), fullname(martin, williams), fullname(henry, smith), fullname(jane, wilson), fullname(mary, smith)].

find_under_30(L) :- findall(name(F, S), (person(F, S, A, _, _), A < 30), L).

pred1([_|L], L).
inc1(LL, L) :-
    incutil(LL, [], L).
incutil([], L, L).
incutil([H|T], HNext, L) :-
    incutil(T, HNext, LL),
    HX is H+1,
    L = [HX|LL].

palindrom(L) :- L = LL, reverse(L, LL).

putfirst(X, L, [X|L]).
putlast(_, [], []).
putlast(X, [H|T], L) :-
    putlast(X, T, LL),
    L = [H|LL], !.

pred2(L, R) :- findall([H], member(H, L), R).
pred3(L, R) :- findall(pred(H,H), member(H, L), R).
pred4(L, R) :- findall([element,H], member(H, L), R).
