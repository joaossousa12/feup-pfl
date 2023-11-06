% greetings
% greetings message
greetings :-
    write('--------------------------------------\n'),
    write('      Welcome to Murus Gallicus       \n'),
    write('--------------------------------------\n\n').

% title
% title screen message
title :-
    write('   -----------------------------------------\n'),
    write('                 Murus Gallicus             \n'),
    write('   -----------------------------------------\n').

% get_line(-Res, +Acc)
% reads a line of user input
get_line(Res, Acc) :-
    get_char(Char),
    Char \= '\n',
    append(Acc, [Char], Acc1),
    get_line(Res, Acc1).
get_line(Res, Acc):-
    atom_chars(Res, Acc).

% read_name(+Player, -PlayerName)
% reads the player name from user input 
read_name(Player, PlayerName) :- 
    format('~w, choose the name you want to be referred as: ', [Player]),
    read(PlayerName).

% clear_console
% clears all console output
clear_console:- 
    write('\33\[2J').

% clear_data
% clears data after the game is finished
clear_data :-
    % to add more
    retractall(player_name(_,_)).

% first_element(+List, -Element)
% gets the first element of a list
first_element([X | _], X).

% second_element(+List, -Element)
% gets the second element of a list
second_element([_, X | _], X).

% read_row(-Row, +Str)
% reads a row from user input with a personalized message
read_row(Row, Str) :-
    repeat,
    format('~w', Str),
    read(DispRow),
    ((integer(DispRow), DispRow >= 1, DispRow =< 7) ->
        Row is 8 - DispRow
    ;   write('Wrong input. Please enter a valid row (1-7).\n'),
        fail  % Fail to re-prompt
    ).

% read_col(-Col, +Str)
% reads a column from user input with a personalized message
read_col(Col, Str) :-
    format('~w', Str),
    read(DispCol),
    char_code(DispCol, AsciiCode),
    Col is AsciiCode - 96.

% get_move(-Move)
% reads a move from user input
get_move(ColI-RowI-ColF-RowF) :-
    read_row(RowI, 'Origin row: '),
    read_col(ColI, 'Origin column (lowercase): '),
    read_row(RowF, 'Destiny row: '),
    read_col(ColF, 'Destiny column (lowercase): ').

% min_list(+List, -Min)
% find the minimum element of a list
min_list([Min], Min).
min_list([H|T], Min) :-
    min_list(T, TMin),
    Min is min(H, TMin). 

% sum_list(+List, -Sum)
% sum the elements of a list
sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, TSum),
    Sum is H + TSum.

% list_contains(+List, +Element)
% finds if a list contains a specific element
list_contains([], _) :- fail.
list_contains([X|_], Target) :- X = Target, !.
list_contains([_|Rest], Target) :- list_contains(Rest, Target).