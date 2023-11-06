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
display_game(GameState) :-
    nl,
    print_board_separator,
    [Board, _, _] = GameState,
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

behind_pos(ColI-RowI-ColF-RowF, ColRes-RowRes) :-
    % straight down, place at rowf-1
    Ydiff is RowF - RowI, Xdiff is ColF - ColI,
    Ydiff > 0, Xdiff = 0,
    ColRes is ColF, % could be ColI as well since they're the exact same
    RowRes is RowF - 1.

behind_pos(ColI-RowI-ColF-RowF, ColRes-RowRes) :-
    % straight up, place at rowf+1
    Ydiff is RowF - RowI, Xdiff is ColF - ColI,
    Ydiff < 0, Xdiff = 0,
    ColRes is ColF,
    RowRes is RowF + 1.

behind_pos(ColI-RowI-ColF-RowF, ColRes-RowRes) :-
    % straight right, place at colf-1
    Ydiff is RowF - RowI, Xdiff is ColF - ColI,
    Ydiff = 0, Xdiff > 0,
    ColRes is ColF - 1,
    RowRes is RowF.

behind_pos(ColI-RowI-ColF-RowF, ColRes-RowRes) :-
    % straight left, place at colf+1
    Ydiff is RowF - RowI, Xdiff is ColF - ColI,
    Ydiff = 0, Xdiff < 0,
    ColRes is ColF + 1,
    RowRes is RowF.

behind_pos(ColI-RowI-ColF-RowF, ColRes-RowRes) :-
    % diagonally down and right, place at colf-1 and rowf-1
    Ydiff is RowF - RowI, Xdiff is ColF - ColI,
    Ydiff > 0, Xdiff > 0,
    ColRes is ColF - 1,
    RowRes is RowF - 1.

behind_pos(ColI-RowI-ColF-RowF, ColRes-RowRes) :-
    % diagonally down and left, place at colf+1 and rowf-1 
    Ydiff is RowF - RowI, Xdiff is ColF - ColI,
    Ydiff > 0, Xdiff < 0,
    ColRes is ColF + 1,
    RowRes is RowF - 1.

behind_pos(ColI-RowI-ColF-RowF, ColRes-RowRes) :-
    % diagonally up and right, place at colf-1 and rowf+1
    Ydiff is RowF - RowI, Xdiff is ColF - ColI,
    Ydiff < 0, Xdiff > 0,
    ColRes is ColF - 1,
    RowRes is RowF + 1.

behind_pos(ColI-RowI-ColF-RowF, ColRes-RowRes) :-
    % diagonally up and left, place at colf+1 and rowf+1
    Ydiff is RowF - RowI, Xdiff is ColF - ColI,
    Ydiff < 0, Xdiff < 0,
    ColRes is ColF + 1,
    RowRes is RowF + 1.

behind_pos(ColI-RowI-ColF-RowF, ColRes-RowRes) :- 
    % idk other cases where the previous doesn't work?
    ColRes is ColF,
    RowRes is RowF. 
    % probably useless lol

resulting_piece(Board, Piece, Col-Row, Res) :-
    position(Board, Col-Row, Aux),
    Aux = '  ', Piece = ' 2',
    Res = ' 1'.

resulting_piece(Board, Piece, Col-Row, Res) :-
    position(Board, Col-Row, Aux),
    Aux = ' 1', Piece = ' 2',
    Res = ' 2'.

resulting_piece(Board, Piece, Col-Row, Res) :-
    position(Board, Col-Row, Aux),
    Aux = '  ', Piece = 'II',
    Res = ' I'.

resulting_piece(Board, Piece, Col-Row, Res) :-
    position(Board, Col-Row, Aux),
    Aux = ' I', Piece = 'II',
    Res = 'II'.
    
move_piece(Board, ColI-RowI-ColF-RowF, Piece, NewBoard3) :-
    remove_piece(Board, ColI-RowI, NewBoard),
    RowIndex is RowF - 1, ColIndex is ColF - 1,
    nth0(RowIndex, NewBoard, Line),
    resulting_piece(NewBoard, Piece, ColF-RowF, ResPiece),
    replace(ColIndex, ResPiece, Line, NewLine),
    replace(RowIndex, NewLine, NewBoard, NewBoard2),

    behind_pos(ColI-RowI-ColF-RowF, ColRes-RowRes),
    resulting_piece(NewBoard2, Piece, ColRes-RowRes, ResPiece2),
    RowIndex2 is RowRes - 1, ColIndex2 is ColRes - 1,
    nth0(RowIndex2, NewBoard2, Line2),
    replace(ColIndex2, ResPiece2, Line2, NewLine2),
    replace(RowIndex2, NewLine2, NewBoard2, NewBoard3).

replace_piece(Board, Col-Row, Piece, NewBoard) :-
    RowIndex is Row - 1,
    ColIndex is Col - 1,
    nth0(RowIndex, Board, Line),
    replace(ColIndex, Piece, Line, NewLine),
    replace(RowIndex, NewLine, Board, NewBoard).

eat_piece(Board, ColI-RowI-ColF-RowF, NewBoard) :-
    position(Board, ColI-RowI, InitialPiece),

    ((InitialPiece = 'II') ->
        replace_piece(Board, ColI-RowI, ' I', TempBoard)
    ;
        replace_piece(Board, ColI-RowI, ' 2', TempBoard)
    ),

    remove_piece(TempBoard, ColF-RowF, NewBoard).

