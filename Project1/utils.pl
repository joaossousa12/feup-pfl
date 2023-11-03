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

first_element([X | _], X).
second_element([_, X | _], X).

read_row(Row, Str) :-
    format('~w', Str),
    read(DispRow),
    Row is 8 - DispRow.

read_col(Col, Str) :-
    % pequeno problema ter de ser lowercase para funcionar, se não, dá erro
    % com get_char o programa passa à frente por alguma razão
    format('~w', Str),
    read(DispCol),
    char_code(DispCol, AsciiCode),
    Col is AsciiCode - 96.
get_move(Board, ColI-RowI-ColF-RowF) :-
    read_row(RowI, 'Origin row: '),
    read_col(ColI, 'Origin column (lowercase): '),
    read_row(RowF, 'Destiny row: '),
    read_col(ColF, 'Destiny column (lowercase): ').


list_contains([], _) :- fail.
list_contains([X|_], Target) :- X = Target, !.
list_contains([_|Rest], Target) :- list_contains(Rest, Target).