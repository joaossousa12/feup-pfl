:- dynamic isRandomBot/2.
:- dynamic isGreedyBot/2.

% other_player(+CurrentPlayer,-NextPlayer)
% get opposing player
other_player(player1, player2).
other_player(player2, player1).

% piece_info(+PieceType, -Player)
% get the player using a specific piece type
piece_info(roman, player1).
piece_info(numeral, player2).

% symbol(+PieceType, -Piece)
% get which pieces belong to each piece type
symbol(numeral, ' 2').
symbol(numeral, ' 1').
symbol(roman, ' I').
symbol(roman, 'II').

% isRandomBot(+Player, -Bool)
% initially set this status to "false", or "0"
isRandomBot(player1, 0).
isRandomBot(player2, 0).
isGreedyBot(player1, 0).
isGreedyBot(player2, 0).

% isBot(+Player, -Bool)
% find if a player is a bot or not
isBot(Player, 1) :- isRandomBot(Player, 1) ; isGreedyBot(Player, 1).

% initial_state(+GameState)
% initialize gamestate with initial board, player1 and 0 total moves
initial_state([Board, player1, 0]) :-
    main_menu,
    clear_console,
    title,
    initial_board(Board), display_game([Board, player1, 0]).

% choose_move(+GameState, -Move)
% choose a random bot move
choose_move(GameState, ColI-RowI-ColF-RowF) :-
    [_, Player, _] = GameState,
    isRandomBot(Player, 1),
    valid_moves(GameState, ListOfMoves),
    random_member(ColI-RowI-ColF-RowF, ListOfMoves).

% choose_move(+GameState, -Move)
% choose a greedy bot move using minimax algorithm
choose_move(GameState, ColI-RowI-ColF-RowF) :-
    [_, Player, _] = GameState,
    isGreedyBot(Player, 1),
    valid_moves(GameState, ListOfMoves),
    other_player(Player, OtherPlayer),
    findall(Value-Coordinate, ( member(Coordinate, ListOfMoves), 
                                move(GameState, Coordinate, NewGameState), 
                                value(NewGameState,Player, Value1),
                                minimax(NewGameState, OtherPlayer, min, 1, Value2),
                                Value is Value1 + Value2), Pairs),
    sort(Pairs, SortedPairs),
    last(SortedPairs, Max-_),
    findall(Coordinates, member(Max-Coordinates, SortedPairs), MaxCoordinates),
    random_member(ColI-RowI-ColF-RowF, MaxCoordinates).

% move(+GameState, +Move, -NewGameState)
% execute a move on a specific piece, to specific coordinates
move(GameState, ColI-RowI-ColF-RowF, NewGameState) :-
    [Board, Player, TotalMoves] = GameState,
    position(Board, ColI-RowI, Piece), % get the piece at the initial position
    (isOneSquareAway(ColI-RowI, ColF-RowF) ->
        eat_piece(Board, ColI-RowI-ColF-RowF, NewBoard)
    ;
        move_piece(Board, ColI-RowI-ColF-RowF, Piece, NewBoard)
    ),
    other_player(Player, NewPlayer),
    NewTotalMoves is TotalMoves + 1,
    NewGameState = [NewBoard, NewPlayer, NewTotalMoves].

% valid_moves(+GameState, -ListOfMoves)
% returns a list of valid moves
valid_moves(GameState, ListOfMoves) :-
    [Board, _, _] = GameState,
    findall(ColI-RowI-ColF-RowF, (
        insideBoard(ColI-RowI), 
        position(Board, ColI-RowI, Piece), 
        isTower(Piece),    
        insideBoard(ColF-RowF),
        validate(GameState, ColI-RowI, ColF-RowF)  
    ), ListOfMoves).

% base_reached(+GameState)
% used to find if any player's base has been reached
base_reached(GameState) :-
    [Board, _, _] = GameState,
    nth1(1, Board, FirstRow), % row 1 is actually row 7 ingame 
    nth1(7, Board, LastRow), % and vice-versa
    ( list_contains(FirstRow, ' I') ; list_contains(LastRow, ' 1') ).

% game_over(+GameState)
% checks if game is over
game_over(GameState) :-
    [Board, Player, _] = GameState,
    other_player(Player, PreviousPlayer),
    count_towers(Board, PreviousPlayer, TowerCount),
    ( base_reached(GameState) ;  TowerCount =:= 0).

% game_cycle(+GameState)
% game loop
game_cycle(GameState) :-
    [_, Player, TotalMoves] = GameState,
    player_name(Player, WinnerName),
    game_over(GameState), !,
    PlayerMoves is TotalMoves div 2,
    format('~a won with ~d moves!\n', [WinnerName, PlayerMoves]),
    % quits
    write('\nPress any key to quit. Thanks for playing!\n'),
    read(_).
game_cycle(GameState):-
    second_element(GameState, Player),
    player_name(Player, PlayerName),
    format('~w\'s turn\n', [PlayerName]),
    (isBot(Player, 1) ->
        choose_move(GameState, Col11-Row11-Col21-Row21),
        move(GameState,  Col11-Row11-Col21-Row21, NewGameState),
        format('~w chose move: ColI:~w-RowI:~w-ColF:~w-RowF:~w\n', [PlayerName, Col11, Row11, Col21, Row21]),
        sleep(2),
        first_element(NewGameState, NewBoard),
        clear_console,
        title,
        display_game(NewGameState),
        game_cycle(NewGameState)
    ;
    get_move(Col1-Row1-Col2-Row2),
    % Validate the move
    (validate(GameState, Col1-Row1, Col2-Row2) ->
        move(GameState, Col1-Row1-Col2-Row2, NewGameState),
        first_element(NewGameState, NewBoard),
        clear_console,
        title,
        display_game(NewGameState),
        game_cycle(NewGameState)
    ; % Invalid move
        write('Invalid move. Please try again.\n'),
        game_cycle(GameState)
    )
    ).

% insideBoard(+Position)
% checks if a position is whithin board limits
insideBoard(Col-Row) :- 
    between(1, 7, Row),
    between(1, 8, Col).

% isEmpty(+Board, +Position)
% checks if a position is empty
isEmpty(Board, Col-Row) :- 
    position(Board, Col-Row, Slot),
    Slot = '  '.

% pieceBelongsToPlayer(+Piece, +Player)
% checks if a piece belongs to a player
pieceBelongsToPlayer(Piece, Player) :-
    symbol(Type, Piece),
    piece_info(Type, Player).

% isTower(+Piece)
% checks if a piece is a tower
isTower(Piece) :- 
    (Piece = ' 2' ; Piece = 'II').

% targetAvailable(+GameState, +Position)
% checks if target position is available
targetAvailable(GameState, Col-Row) :-
    % se for espaço vazio, não há problema
    [Board, _, _] = GameState,
    position(Board, Col-Row, TargetContent),
    TargetContent = '  '.
targetAvailable(GameState, Col-Row) :-
    % se não for um espaço vazio, tem de ser uma peça unitária que pertence ao jogador
    [Board, Player, _] = GameState,
    position(Board, Col-Row, TargetContent),
    ( TargetContent = ' I' ; TargetContent = ' 1' ),
    pieceBelongsToPlayer(TargetContent, Player).

% isTwoSquaresAway(+Position1, +Position2)
% checks is position 1 is exactly 2 squares of distance away from position 2
isTwoSquaresAway(Col1-Row1, Col2-Row2) :-
    DeltaCol is abs(Col2 - Col1),
    DeltaRow is abs(Row2 - Row1),
    ((DeltaCol = 2, DeltaRow = 0) ; (DeltaCol = 0, DeltaRow = 2) ; (DeltaCol = 2, DeltaRow = 2)).

% isOneSquareAway(+Position1, +Position2)
% checks is position 1 is exactly 1 square of distance away from position 2
isOneSquareAway(Col1-Row1, Col2-Row2) :-
    DeltaCol is abs(Col2 - Col1),
    DeltaRow is abs(Row2 - Row1),
    ((DeltaCol = 1, DeltaRow = 0) ; (DeltaCol = 0, DeltaRow = 1) ; (DeltaCol = 1, DeltaRow = 1)).

% validate(+GameState, +InitialPosition, +FinalPosition)
% validates if a move conforms with the game's rules 
% by doing a series of verifications 
validate(GameState, ColI-RowI, ColF-RowF) :-
    % get board and player from gamestate
    [Board, Player, _] = GameState,
    
    % get final and behind-piece position
    position(Board, ColI-RowI, Piece),
    position(Board, ColF-RowF, PieceFinal),
    behind_pos(ColI-RowI-ColF-RowF, ColBeh-RowBeh), !,

    % check if the selected slot contains a piece that is a tower
    isTower(Piece),

    % check if initial, behind and final slots are whithin the board's range
    insideBoard(ColI-RowI),
    insideBoard(ColBeh-RowBeh),
    insideBoard(ColF-RowF),

    % check if the selected starting piece belongs to the player    
    pieceBelongsToPlayer(Piece, Player),

    (isTwoSquaresAway(ColI-RowI, ColF-RowF) ->
        % check if the destinations belong to the player or are empty
        targetAvailable(GameState, ColBeh-RowBeh),
        targetAvailable(GameState, ColF-RowF)
    ;
    isOneSquareAway(ColI-RowI, ColF-RowF) ->
        ((PieceFinal = ' 1') ; (PieceFinal = ' I')),
        \+isTower(PieceFinal),
        \+pieceBelongsToPlayer(PieceFinal, Player)
    ).

% count_towers(+Board, +Player, -TowerCount)
% counts how many towers a player has left
count_towers(Board, Player, TowerCount) :-
    count_towers_in_board(Board, Player, 0, TowerCount).
count_towers_in_board([], _, TowerCount, TowerCount).
count_towers_in_board([Row|RestRows], Player, Acc, TowerCount) :-
    count_towers_in_row(Row, Player, 0, RowTowers),
    NewAcc is Acc + RowTowers,
    count_towers_in_board(RestRows, Player, NewAcc, TowerCount).
count_towers_in_row([], _, RowTowers, RowTowers).
count_towers_in_row([Piece | RestRow], Player, Acc, RowTowers) :-
    (isTower(Piece), pieceBelongsToPlayer(Piece, Player) -> NewAcc is Acc + 1 ; NewAcc is Acc),
    count_towers_in_row(RestRow, Player, NewAcc, RowTowers).

% swap_minimax(+Mode, -NewMode)
% swaps minimax algorithm mode
swap_minimax(min, max).
swap_minimax(max, min).

% eval(+Mode, +Values, -Result)
% unifies result with the value according to the minimax mode
eval(min, [Value|_], Result) :- Result is -Value.
eval(max, Values, Value) :- last(Values, Value).

% minimax(+GameState, +Player, +Type, -Value)
% minimax algorithm with depth 2 for greedy bot
minimax(_, _, _, 2, 0):- !.
minimax(GameState, Player, Type, Level, Value):-
	other_player(Player, NewPlayer),
	swap_minimax(Type, NewType),
    NextLevel is Level + 1,
	valid_moves(GameState, ListOfMoves),
	setof(Val, (  member(Coordinate, ListOfMoves), 
                  move(GameState, Coordinate, NewGameState), 
                  value(NewGameState,Player,Value1),
                  minimax(NewGameState, NewPlayer, NewType, NextLevel, Value2), 
                  Val is Value1 + Value2), Values),
    eval(Type, Values, Value).

% value(+GameState, +Player, -Value)
% used by minimax to value a specific game state
value([Board, _, _], Player, Value):-
    other_player(Player, EnemyPlayer),
    count_towers(Board, Player, TowerCount),
    count_towers(Board, EnemyPlayer, EnemyTowerCount),
    evaluate_positions(Board, Player, PositionValue),
    Value is PositionValue + 100*(TowerCount - EnemyTowerCount).

% evaluate_positions(+Board, +Player, -PositionValue)
% evaluates piece positions
evaluate_positions(Board, Player, PositionValue) :-
    findall(Value, 
            (position(Board, Col-Row, Piece), 
            pieceBelongsToPlayer(Piece, Player), 
            piece_value(Piece, Value), 
            position_value(Board, Col-Row, Player, PositionValue)), 
    Values),
    sum_list(Values, PositionValue).

% piece_value(+Piece, -Value)
% determines how valuable a piece is
piece_value(Piece, Value) :-
    (isTower(Piece) -> Value is 3 ; Value is 1).  % Give higher value to towers

% position_value(+Board, +Position, +Player, -PositionValue)
% determines the value of a specific piece position based on game rules and strategy
position_value(Board, Col-Row, Player, PositionValue) :-
    other_player(Player, Opponent),
    (Player = player1 -> DistanceFromOpponentHomeRow is 7 - Row ; DistanceFromOpponentHomeRow is Row - 1),
    findall(Distance, 
            (position(Board, Col1-Row1, OpponentPiece), 
            pieceBelongsToPlayer(OpponentPiece, Opponent), 
            Distance is sqrt((Col-Col1)^2 + (Row-Row1)^2)), 
    Distances),
    (Distances = [] -> MinDistance is 0 ; min_list(Distances, MinDistance)),
    (DistanceFromOpponentHomeRow =:= 0 -> PositionValue1 is 0 ; PositionValue1 is 1 / DistanceFromOpponentHomeRow),
    (MinDistance =:= 0 -> PositionValue2 is 0 ; PositionValue2 is 1 / MinDistance),
    PositionValue is PositionValue1 - PositionValue2.


