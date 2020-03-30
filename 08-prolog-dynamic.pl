?-dynamic(person/5).

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

teardown :- retractall(person(_, _, _, _, _)).

remove(Forename, Surname) :- retract(person(Forename, Surname, _, _, _)).

change_job(Forename, Surname, New_Profession) :-
    person(Forename, Surname, Age, City, Profession),
    retract(Forename, Surname, Age, City, Profession),
    assertz(Forename, Surname, Age, City, New_Profession).

output_data :-
    telling(T), tell('people2.txt'),
    write_data, !,
    told, tell(T),
    write('Data written'), nl.
write_data :-
    person(A, B, C, D, E),
    write(A), write('. '),
    write(B), write('. '),
    write(C), write('. '),
    write(D), write('. '),
    write(E), write('. '), nl,
    fail.
write_data :- write('end.'), nl.

?-dynamic(animal/1).
add_animals_data :-
    read(A), process_animal_data(A), !.
process_animal_data(end).
process_animal_data(A) :-
    A \== end,
    assertz(animal(A)),
    add_animals_data.

display_animals :- animal(X), write(X), nl, fail.
display_animals.

remove2(F, A) :-
    (
        A == cat; A == dog
    ),
    compound_name_arguments(T, F, [A]),
    retract(T), !.
