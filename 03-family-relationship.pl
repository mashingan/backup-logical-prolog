mother(ann,henry).
mother(ann,mary).
mother(jane,mark).
mother(jane,francis).
mother(annete,jonathan).
mother(mary,bill).
mother(janice,louise).
mother(lucy,janet).
mother(louise,caroline).
mother(louise,martin).
father(henry,jonathan).
father(john,mary).
father(francis,william).
father(francis,louise).
father(john,mark).
father(gavin,lucy).
father(john,francis).
father(martin,david).
father(martin,janet).
parent(victoria,george).
parent(victoria,edward).
%parent(X,Y) :- write('mother?'),nl,mother(X,Y),write('mother!'),nl.
%parent(A,B) :- write('father?'),nl,father(A,B),write('father!'),nl.
parent(X,Y) :- mother(X,Y).
parent(A,B) :- father(A,B).
parent(elizabeth,charles).
parent(elizabeth,andrew).

rich(jane).
rich(john).
rich(gavin).
rich_father(X,Y) :- rich(X), father(X, Y).

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

child_of(A, B) :- parent(B, A).
grandfather_of(A, B) :- father(B, C), parent(C, A).
grandmother_of(A, B) :- mother(B, C), parent(C, A).
great_grandfather_of(A, B) :- father(B, C), 
(
    grandfather_of(C, A);
    grandmother_of(C, A)
).

% ?- ancestor(louise, Desc).
% - parent(louise, Desc).
% -- mother(louise, Desc).
% --- Desc = caroline, bt
% -- mother(louise, Desc).
% --- Desc = martin, bt
% -- father(louise, Desc), bt
% - parent(louise, Dtemp), ancestor(Dtemp, Desc).
% -- mother(louise, Dtemp), ancestor(Dtemp, Desc).
% --- mother(louise, Dtemp), parent(Dtemp, Desc).
% ---- mother(louise, Dtemp), mother(Dtemp, Desc).
% ----- Dtemp = caroline, Desc = _, false, bt
% ---- mother(louise, Dtemp), mother(Dtemp, Desc).
% ----- Dtemp = martin, Desc = _, false, bt
% ---- mother(louise, Dtemp), mother(Dtemp, Desc), false, bt
% --- mother(louise, Dtemp), parent(Dtemp, Desc).
% ---- mother(louise, Dtemp), father(Dtemp, Desc).
% ----- Dtemp = caroline, Desc = _, false, bt
% ---- mother(louise, Dtemp), father(Dtemp, Desc).
% ----- Dtemp = martin, Desc = david, bt
% ---- mother(louise, Dtemp), father(Dtemp, Desc).
% ----- Dtemp = martin, Desc = janet, bt
% result:
% Desc = caroline;
% Desc = martin;
% Desc = david;
% Desc = janet;
% false.
