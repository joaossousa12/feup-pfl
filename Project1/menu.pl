:- consult('game.pl').
:- consult('utils.pl').
:- consult('board.pl').
%:- consult('configurations.pl').

other_player(player1, player2).
other_player(player2, player1).

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
    write('Player vs Computer game ...\n').

process_play_choice(3) :-
    write('Computer vs Computer game ...\n').

process_play_choice(_) :-
    write('Invalid play mode. Please try again.\n'),
    display_play_menu.

display_help_menu :-
    % Just to visualize the help menu for now
    write('Murus Gallicus is a game where players strategically conquer territories to defeat opponents.').

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

move(GameState, ColI-RowI-ColF-RowF, NewGameState) :-
    % aqui já se assume que a move é válida, certo?
    [Board, Player, TotalMoves] = GameState,
    position(Board, ColI-RowI, Piece), % get the piece at the initial position
    move_piece(Board, ColI-RowI-ColF-RowF, Piece, NewBoard),
    other_player(Player, NewPlayer),
    NewTotalMoves is TotalMoves + 1,
    NewGameState = [NewBoard, NewPlayer, NewTotalMoves].

% game_cycle(GameState):-
% for game over
game_cycle(GameState):-
    second_element(GameState, Player),
    format('~w\'s turn\n', [Player]),
    get_move(Board, Col1-Row1-Col2-Row2),
    move(GameState, Col1-Row1-Col2-Row2, NewGameState),
    first_element(NewGameState, Board),
    clear_console,
    title,
    print_board(Board),
    game_cycle(NewGameState).

% TODO checker for legal moves vai estar no game_cycle quando
% se for a pedir o move 

play :-
    gamestate(GameState), !,
    game_cycle(GameState),
    clear_data.
