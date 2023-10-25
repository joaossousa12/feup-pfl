:- consult('game.pl').
:- consult('utils.pl').
:- consult('board.pl').
%:- consult('configurations.pl').

main_menu :-
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
    write('Player vs Player game ...\n'),
    % asserta(player_name(player1, 'João')), 
    % asserta(player_name(player2, 'Pedro')). % testing
    read_name(player1), 
    read_name(player2).

process_play_choice(2) :-
    write('Player vs Computer game ...\n').

process_play_choice(3) :-
    write('Computer vs Computer game ...\n').

process_play_choice(_) :-
    write('Invalid play mode. Please try again.\n'),
    display_play_menu.

display_help_menu :-
    % Just to visualize the help menu for now
    write('Murus Galicius is a game where players strategically conquer territories to defeat opponents.').

greetings :-
    write('--------------------------------------\n'),
    write('      Welcome to Murus Galicius       \n'),
    write('--------------------------------------\n\n').

% initialize gamestate with board, first player is player1 and 0 totalmoves
gamestate([Board, player1, 0]) :-
    greetings,
    main_menu,
    initial_board(Board), print_board(Board).

% game_cycle(GameState):-
    % for game over
game_cycle(GameState):-
    first_element(GameState, Board),
    second_element(GameState, Player),
    format('~w\'s turn\n', [Player]),
    write('insert move pls\n'),   % for testing, later use
    read(ABC),                    % real functions instead
    print_board(Board),
    game_cycle(NewGameState).

play :-
    gamestate(GameState),
    game_cycle(GameState),
    clear_data.
