:- consult('game.pl').
:- consult('utils.pl').
:- consult('board.pl').
%:- consult('configurations.pl').
:- dynamic isbot/2.

other_player(player1, player2).
other_player(player2, player1).

piece_info(numeral, player1).
piece_info(roman, player2).

symbol(numeral, ' 2').
symbol(numeral, ' 1').
symbol(roman, ' I').
symbol(roman, 'II').

isbot(player1, 0). % 0 = false
isbot(player2, 0). % 0 = false

greetings :-
    write('--------------------------------------\n'),
    write('      Welcome to Murus Gallicus       \n'),
    write('--------------------------------------\n\n').

main_menu :-
    clear_console,
    greetings,
    write('Main Menu\n'),
    write('1. Play\n'),
    write('2. Help\n'),
    write('3. Quit\n'),
    read_option(Choice),
    process_choice(Choice).

read_option(Choice) :-
    write('Enter your choice (1, 2, or 3): '),
    read(Choice),
    validate_choice(Choice).

% Cut to prevent backtracking
validate_choice(Choice) :-
    (Choice = 1 ; Choice = 2 ; Choice = 3),
    !.

validate_choice(_) :-
    write('Invalid choice. Please try again.'), nl,
    read_option(Choice).

process_choice(1) :-
    display_play_menu. % type 1 for now to test
    %init_game('h', 'h', GameState),
	%display_game(GameState).
    %display_play_menu.

process_choice(2) :-
    % Need to maybe do another sub-menu after entering help menu to send back to main menu or quit the app.
    display_help_menu,
    main_menu.

process_choice(3) :-
    write('Thanks for playing!\n'),
    sleep(2),
    halt.

display_play_menu :-
    write('Choose how to play the game:\n'),
    write('1. Player vs Player\n'),
    write('2. Player vs Computer\n'),
    write('3. Computer vs Computer\n'),
    read_game_option(PlayChoice),
    process_play_choice(PlayChoice).

read_game_option(PlayChoice) :-
    write('Enter your choice (1, 2, or 3): '),
    read(PlayChoice),
    validate_choice(PlayChoice).

process_play_choice(1) :-
    write('Player vs Player game ...\n').
    % asserta(player_name(player1, 'João')), 
    % asserta(player_name(player2, 'Pedro')). % testing
    % read_name(player1), 
    % read_name(player2).

process_play_choice(2) :-
    write('Player vs Computer game ...\n'),
    retract(isbot(player2, 0)),
    asserta(isbot(player2, 1)). 

process_play_choice(3) :-
    write('Computer vs Computer game ...\n'),
    retract(isbot(player1, 0)), 
    retract(isbot(player2, 0)), 
    asserta(isbot(player1, 1)),
    asserta(isbot(player2, 1)).

process_play_choice(_) :-
    write('Invalid play mode. Please try again.\n'),
    display_play_menu.

display_help_menu :-
    clear_console,
    title,
    write('\nDescription:\n\n'),
    write('Murus Gallicus is a game where players strategically conquer territories to defeat opponents.\n\n'),
    write('Rules and Notes:\n\n'),
    write('1. Towers are formed by stacking two pieces (2 or II).\n'),
    write('2. The decimal pieces start to play.\n'),
    write('3. Only towers are allowed to move.\n'),
    write('4. To move, select one of your towers and click two spaces away from it in any direction. One piece will move to the clicked spot, and the other stays in the middle space.\n'),
    write('5. You cannot move to a space that already has two of your pieces or one opponent\'s piece (either a single piece or a tower).\n'),
    write('6. If you have a tower with an isolated piece, moving it will result in one tower and one piece in the middle space.\n'),
    write('7. If the opponent has an isolated piece next to one of your towers, you can sacrifice one of your pieces to "capture" the opponent\'s piece.\n'),
    write('8. At no point in the game should one player have more pieces than the other. When one player loses a piece, the other must sacrifice one as well, maintaining balance.\n'),
    write('9. A player wins under two conditions: by reaching the opponent\'s side of the board or by forcing a stalemate (where the opponent has no legal moves left).\n\n'),
    write('You will be redirected to the main menu in 10 seconds. Enjoy the game!!\n'),
    sleep(10).

title :-
    write('   -----------------------------------------\n'),
    write('                 Murus Gallicus             \n'),
    write('   -----------------------------------------\n').

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

game_over(GameState) :-
    valid_moves(GameState, ListOfMoves),
    length(ListOfMoves, 0).

game_cycle(GameState):-
    [_, Winner, TotalMoves] = GameState,
    game_over(GameState), !,
    format('~a won with ~d moves!', [Winner, TotalMoves]).
game_cycle(GameState):-
    second_element(GameState, Player),
    format('~w\'s turn\n', [Player]),
    % testing random bot 
    % randomBotMove(GameState, Col11-Row11-Col21-Row21),
    % format('Random bot chose move: ColI:~w-RowI:~w-ColF:~w-RowF:~w\n', [Col11, Row11, Col21, Row21]),
    (isbot(Player, 1) ->
        randomBotMove(GameState, Col11-Row11-Col21-Row21),
        move(GameState,  Col11-Row11-Col21-Row21, NewGameState),
        format('Random bot chose move: ColI:~w-RowI:~w-ColF:~w-RowF:~w\n', [Col11, Row11, Col21, Row21]),
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

play :-
    gamestate(GameState), !,
    game_cycle(GameState),
    clear_data.
