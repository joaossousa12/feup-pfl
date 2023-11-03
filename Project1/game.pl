:- consult('board.pl').
:- consult('utils.pl').
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(random)).
:- use_module(library(between)).
:- dynamic isbot/2.

other_player(player1, player2).
other_player(player2, player1).

piece_info(roman, player1).
piece_info(numeral, player2).

symbol(numeral, ' 2').
symbol(numeral, ' 1').
symbol(roman, ' I').
symbol(roman, 'II').

isbot(player1, 0). % 0 = false
isbot(player2, 0). % 0 = false

% initialize gamestate with board, first player is player1 and 0 totalmoves
gamestate([Board, player1, 0]) :-
    main_menu,
    clear_console,
    title,
    initial_board(Board), print_board(Board).

% ColI-RowI-ColF-RowF output move
randomBotMove(GameState, ColI-RowI-ColF-RowF) :-
    valid_moves(GameState, ListOfMoves),
    random_member(ColI-RowI-ColF-RowF, ListOfMoves).

move(GameState, ColI-RowI-ColF-RowF, NewGameState) :-
    [Board, Player, TotalMoves] = GameState,
    position(Board, ColI-RowI, Piece), % get the piece at the initial position
    (isOneSquareAway(ColI-RowI, ColF-RowF) ->
        eat_piece(Board, ColI-RowI-ColF-RowF, Piece, NewBoard)
    ;
        move_piece(Board, ColI-RowI-ColF-RowF, Piece, NewBoard)
    ),
    other_player(Player, NewPlayer),
    NewTotalMoves is TotalMoves + 1,
    NewGameState = [NewBoard, NewPlayer, NewTotalMoves].

valid_moves(GameState, ListOfMoves) :-
    [Board, Player, _] = GameState,
    findall(ColI-RowI-ColF-RowF, (
        insideBoard(Board, ColI-RowI), 
        position(Board, ColI-RowI, Piece), 
        isTower(Piece),    
        insideBoard(Board, ColF-RowF),
        validate(GameState, ColI-RowI, ColF-RowF)  
    ), ListOfMoves).

base_reached(GameState) :-
    [Board, _, _] = GameState,
    nth1(1, Board, FirstRow), % row 1 is actually row 7 ingame 
    nth1(7, Board, LastRow), % and vice-versa
    ( list_contains(FirstRow, ' I') ; list_contains(LastRow, ' 1') ).

game_over(GameState) :-
    valid_moves(GameState, ListOfMoves),
    % either we reached the opponent's base or we ran out of moves
    ( base_reached(GameState) ; length(ListOfMoves, 0) ).

game_cycle(GameState) :-
    [_, CurrentPlayer, TotalMoves] = GameState,
    other_player(CurrentPlayer, Winner),
    player_name(Winner, WinnerName),
    game_over(GameState), !,
    PlayerMoves is TotalMoves div 2,
    format('~a won with ~d moves!', [WinnerName, PlayerMoves]).
game_cycle(GameState):-
    second_element(GameState, Player),
    player_name(Player, PlayerName),
    format('~w\'s turn\n', [PlayerName]),
    (isbot(Player, 1) ->
        randomBotMove(GameState, Col11-Row11-Col21-Row21),
        move(GameState,  Col11-Row11-Col21-Row21, NewGameState),
        format('~w chose move: ColI:~w-RowI:~w-ColF:~w-RowF:~w\n', [PlayerName, Col11, Row11, Col21, Row21]),
        sleep(2),
        first_element(NewGameState, NewBoard),
        clear_console,
        title,
        print_board(NewBoard),
        game_cycle(NewGameState)
    ;
    get_move(Board, Col1-Row1-Col2-Row2),
    % Validate the move
    (validate(GameState, Col1-Row1, Col2-Row2) ->
        move(GameState, Col1-Row1-Col2-Row2, NewGameState),
        first_element(NewGameState, NewBoard),
        clear_console,
        title,
        print_board(NewBoard),
        game_cycle(NewGameState)
    ; % Invalid move
        write('Invalid move. Please try again.\n'),
        game_cycle(GameState)
    )
    ).

insideBoard(Board, Col-Row) :- 
    % length(Board, Size), aqui n podia ser assim pq a board n é quadrada ent quando movias para a column h ñ dava
    between(1, 7, Row),
    between(1, 8, Col).

isEmpty(Board, Col-Row) :- 
    position(Board, Col-Row, Slot),
    Slot = '  '.

pieceBelongsToPlayer(Piece, Player) :-
    symbol(Type, Piece),
    piece_info(Type, Player).

isTower(Piece) :- 
    (Piece = ' 2' ; Piece = 'II').

targetAvailable(Board, Col-Row, Player) :-
    % se for espaço vazio, não há problema
    position(Board, Col-Row, TargetContent),
    TargetContent = '  '.

targetAvailable(Board, Col-Row, Player) :-
    % se não for um espaço vazio, tem de ser uma peça unitária que pertence ao jogador
    position(Board, Col-Row, TargetContent),
    ( TargetContent = ' I' ; TargetContent = ' 1' ),
    pieceBelongsToPlayer(TargetContent, Player).

isTwoSquaresAway(Col1-Row1, Col2-Row2) :-
    DeltaCol is abs(Col2 - Col1),
    DeltaRow is abs(Row2 - Row1),
    ((DeltaCol = 2, DeltaRow = 0) ; (DeltaCol = 0, DeltaRow = 2) ; (DeltaCol = 2, DeltaRow = 2)).

isOneSquareAway(Col1-Row1, Col2-Row2) :-
    DeltaCol is abs(Col2 - Col1),
    DeltaRow is abs(Row2 - Row1),
    ((DeltaCol = 1, DeltaRow = 0) ; (DeltaCol = 0, DeltaRow = 1) ; (DeltaCol = 1, DeltaRow = 1)).

validate(GameState, ColI-RowI, ColF-RowF) :-
    % get board and player from gamestate
    [Board, Player, _] = GameState,
    
    % get final and behind-piece position
    position(Board, ColI-RowI, Piece),
    position(Board, ColF-RowF, PieceFinal),
    behind_pos(ColI-RowI-ColF-RowF, ColBeh-RowBeh), !, % por alguma razão, sem
    % este cut, quando falha a target available para o behind-position, ele volta
    % a calcular a posição, mas mal, porque fica igual à posição final e o resultado
    % fica errado. 💀💀💀💀💀💀💀💀💀💀💀💀💀💀💀💀💀💀💀💀💀💀💀💀💀💀💀💀💀💀💀

    % check if the selected slot contains a piece that is a tower
    isTower(Piece),

    % check if initial, behind and final slots are whithin the board's range
    insideBoard(Board, ColI-RowI),
    insideBoard(Board, ColBeh-RowBeh),
    insideBoard(Board, ColF-RowF),

    % check if the selected starting piece belongs to the player    
    pieceBelongsToPlayer(Piece, Player),

    (isTwoSquaresAway(ColI-RowI, ColF-RowF) ->
        % check if the destinations belong to the player or are empty
        targetAvailable(Board, ColBeh-RowBeh, Player),
        targetAvailable(Board, ColF-RowF, Player)
    ;
    isOneSquareAway(ColI-RowI, ColF-RowF) ->
        ((PieceFinal = ' 1') ; (PieceFinal = ' I')),
        \+isTower(PieceFinal),
        \+pieceBelongsToPlayer(PieceFinal, Player)
    ).