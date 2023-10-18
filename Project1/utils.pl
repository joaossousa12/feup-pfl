% To be implemented

/*
computer_difficulty(comp) :-
    format('Select a computer difficulty for ', [comp]), nl,
    write('1. Easy\n'),
    write('2. Medium\n'),
    write('3. Difficult\n'), */

get_line(Res, Acc) :-
    get_char(Char),
    Char \= '\n',
    append(Acc, [Char], Acc1),
    get_line(Res, Acc1).
get_line(Res, Acc):-
    atom_chars(Res, Acc).

read_name(Player) :- % both outputs showing at the same time
    format('~a, choose the name you want to be referred as: ', [Player]),
    read(Name), % later add names with spaces
    asserta(player_name(Player,Name)).

clear_console:- 
    write('\33\[2J').

clear_data :-
    % to add more
    retractall(player_name(_,_)).