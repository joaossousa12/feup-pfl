% 
% consult('utils.pl').
print_initial_board :- initial_board(X), print_board(X).

% Define the initial state of the board as a matrix.
initial_board([
    [" 2", " 2", " 2", " 2", " 2", " 2", " 2", " 2"],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["II", "II", "II", "II", "II", "II", "II", "II"]
]).

% Function to print the board.
print_board(Board) :-
    nl,
    print_board_separator,
    print_rows(Board, 1).

print_rows([Row | Rest], RowNumber) :-
    DisplayNumber is 8 - RowNumber,
    format(' ~w ', [DisplayNumber]),
    print_row(Row, RowNumber),
    nl,
    NextRowNumber is RowNumber + 1,
    print_other_rows(Rest, NextRowNumber).

print_other_rows([], _) :-
    print_board_separator,
    print_bottom_letters.

print_other_rows([Row | Rest], RowNumber) :-
    print_row_separator,
    DisplayNumber is 8 - RowNumber,
    format(' ~w ', [DisplayNumber]),
    print_row(Row, RowNumber),
    nl,
    NextRowNumber is RowNumber + 1,
    print_other_rows(Rest, NextRowNumber).

print_row([], _) :-
    write('|').

print_row([Cell | Rest], RowNumber) :-
    format('| ~w ', [Cell]),
    print_row(Rest, RowNumber).

print_board_separator :-
    write('   +----+----+----+----+----+----+----+----+'),
    nl.

print_row_separator :-
    write('   |----+----+----+----+----+----+----+----|'),
    nl.

print_bottom_letters :-
    write('      A    B    C    D    E    F    G    H'),
    nl.

% para testar:
% ?- initial_board(Board), print_board(Board).
