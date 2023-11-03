:- consult('game.pl').
:- consult('board.pl').
:- consult('utils.pl').

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
    write('Press any key to go back to main menu. Enjoy the game!!\n'),
    read(Quit).

play :-
    gamestate(GameState), !,
    game_cycle(GameState),
    clear_data.
