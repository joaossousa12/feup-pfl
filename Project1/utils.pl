greetings :-
    write('--------------------------------------\n'),
    write('      Welcome to Murus Gallicus       \n'),
    write('--------------------------------------\n\n').

title :-
    write('   -----------------------------------------\n'),
    write('                 Murus Gallicus             \n'),
    write('   -----------------------------------------\n').

get_line(Res, Acc) :-
    get_char(Char),
    Char \= '\n',
    append(Acc, [Char], Acc1),
    get_line(Res, Acc1).
get_line(Res, Acc):-
    atom_chars(Res, Acc).

% +Player, -PlayerName 
read_name(Player, PlayerName) :- 
    format('~w, choose the name you want to be referred as: ', [Player]),
    read(PlayerName).

clear_console:- 
    write('\33\[2J').

clear_data :-
    % to add more
    retractall(player_name(_,_)).

first_element([X | _], X).
second_element([_, X | _], X).

read_row(Row, Str) :-
    repeat,
    format('~w', Str),
    read(DispRow),
    ((integer(DispRow), DispRow >= 1, DispRow =< 7) ->
        Row is 8 - DispRow
    ;   write('Wrong input. Please enter a valid row (1-7).\n'),
        fail  % Fail to re-prompt
    ).
read_col(Col, Str) :-
    % pequeno problema ter de ser lowercase para funcionar, se não, dá erro
    % com get_char o programa passa à frente por alguma razão
    format('~w', Str),
    read(DispCol),
    char_code(DispCol, AsciiCode),
    Col is AsciiCode - 96.
get_move(ColI-RowI-ColF-RowF) :-
    read_row(RowI, 'Origin row: '),
    read_col(ColI, 'Origin column (lowercase): '),
    read_row(RowF, 'Destiny row: '),
    read_col(ColF, 'Destiny column (lowercase): ').


list_contains([], _) :- fail.
list_contains([X|_], Target) :- X = Target, !.
list_contains([_|Rest], Target) :- list_contains(Rest, Target).