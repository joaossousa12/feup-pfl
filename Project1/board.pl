% 
% consult('utils.pl').
print_initial_board :- initial_board(X), print_board(X).

% Define the initial state of the board as a matrix.
initial_board([
    [' 2', ' 2', ' 2', ' 2', ' 2', ' 2', ' 2', ' 2'],
    ['  ', '  ', '  ', '  ', '  ', '  ', '  ', '  '],
    ['  ', '  ', '  ', '  ', '  ', '  ', '  ', '  '],
    ['  ', '  ', '  ', '  ', '  ', '  ', '  ', '  '],
    ['  ', '  ', '  ', '  ', '  ', '  ', '  ', '  '],
    ['  ', '  ', '  ', '  ', '  ', '  ', '  ', '  '],
    ['II', 'II', 'II', 'II', 'II', 'II', 'II', 'II']
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

position(Board, Col-Row, Piece) :-
    nth1(Row, Board, Line),
    nth1(Col, Line, Piece).

replace(Index, Element, List, Result) :-
    nth0(Index, List, _, R),
    nth0(Index, Result, Element, R).

remove_piece(Board, Col-Row, NewBoard) :-
    RowIndex is Row - 1, ColIndex is Col - 1,
    nth0(RowIndex, Board, Line),
    replace(ColIndex, '  ', Line, NewLine),
    replace(RowIndex, NewLine, Board, NewBoard).
    
move_piece(Board, ColI-RowI-ColF-RowF, Piece, NewNewBoard) :-
    remove_piece(Board, ColI-RowI, NewBoard),
    RowIndex is RowF - 1, ColIndex is ColF - 1,
    nth0(RowIndex, NewBoard, Line),
    replace(ColIndex, Piece, Line, NewLine),
    replace(RowIndex, NewLine, NewBoard, NewNewBoard).
