<h1 align="center">Murus Gallicus Grupo 4 Turma 6</h1>

<h2 align="center">Contribuidores</h2>

<p align="center"> João Pedro Sousa (up202106996@fe.up.pt). Contribuição: 50% </p>
<p align="center">Emanuel Maia (up202107486@fe.up.pt). Contribuição: 50% </p>

<h2 align="center">Instalação e Execução</h2>

> De modo a proceder a uma correta execução do nosso jogo em _Windows_ e em _Linux_ devemos proceder a instalação do [SICStus Prolog](https://sicstus.sics.se/). Após feita a instalação devemos fazer _download_ da pasta _src_ que contém o _source code_ do nosso jogo. De seguida através do terminal ou ao entramos no _SICStus Prolog_ damos _consult_ ao ficheiro ```main.pl``` e iniciamos o nosso jogo com o predicado ```play/0```.

```prolog
| ?- play.
```

<h2 align="center">Descrição do jogo</h2>

> _Murus Gallicus_ é um jogo de estratégia abstrata criado em 2009 por _Phil Leduc_. O nome _Murus Gallicus_ faz referência às muralhas de pedra usadas nas Guerras Gálicas que ocorreram na Gália, atualmente França. O jogo possui duas condições de vitória que imitam a estratégia de Júlio César de cercar os gauleses. A primeira é alcançar o outro lado do tabuleiro, e a segunda é o _stalemate_ que consiste em colocar o oponente numa posição em que não pode fazer nenhuma jogada legal.
> <br><br>Regras e notas sobre o jogo:
> - _Stacks_ de duas peças são torres
> - Peças no alfabeto romano (*) começam a jogar
> - Só as torres é que se podem mexer
> - Começamos por selecionar uma das nossas torres e temos de _clickar_ a dois espaços dela em qualquer direção e uma das peças vai para esse sitio onde _clickamos_ e a outra fica no espaço do meio e transformam-se em paredes ou peças individuais.
> - Não podemos calhar em cima de um espaço que tenha já duas das nossas peças ou alguma do oponente.
> - Se com a nossa torre _clickarmos_ numa peça individual que tenhamos vamos ficar com uma torre e uma peça no espaço do meio.
> - Se o oponente tiver um peça isolada a beira de uma torre nossa nós podemos sacrificar uma das nossas peças e "comer" a peça do oponente.
> - Em nenhuma altura do jogo um jogador vai ter mais peças que o outro, quando um perde uma peça o outro também perde. Esta técnica costuma ser usada para obter um melhor posicionamento.

> (*) Para a nossa implementação deste jogo em vez de peças brancas e pretas usamos peças em numeração romana e árabe respetivamente.
> <br> De modo a encontrarmos informações sobre o nosso jogo consultamos os seguintes _websites_: [BoardGameGeek](https://boardgamegeek.com/boardgame/55131/murus-gallicus), [Wikipedia](https://en.wikipedia.org/wiki/Murus_Gallicus_(game)) e para aprendermos a parte prática do jogo usamos [Iggamecenter](https://www.iggamecenter.com/en/rules/murusgallicus).

<h2 align="center">Lógica do Jogo</h2>

<h3 align="center">Representação Interna do Estado do Jogo</h3>

> A representação interna do estado do jogo envolve o tabuleiro (```Board```), o jogador atual (```Player```) e o número total de _moves_ (```TotalMoves```). O tabuleiro é representado como uma lista de listas, onde cada elemento da matriz representa uma posição no tabuleiro. Diferentes átomos são usados para representar as peças no tabuleiro, como números romanos ('I' e 'II') e números árabes ('1' e '2'). O jogador atual é representado como 'player1' ou 'player2', e o número total de _moves_ é mantido para controlar o andamento do jogo.<br><br>
> Aqui estão exemplos de representação dos estados do jogo:

<h4 align="center">Estado inicial do jogo</h4>

> No estado inicial do jogo vai começar a jogar o ```player1``` ou seja vai ser esse o valor de ```Player```.  ```TotalMoves``` vai ser 0 uma vez que ainda não foi feito nenhum _move_. A ```Board``` inicial está representada da seguinte maneira:

```prolog
initial_board([
    [' 2', ' 2', ' 2', ' 2', ' 2', ' 2', ' 2', ' 2'],
    ['  ', '  ', '  ', '  ', '  ', '  ', '  ', '  '],
    ['  ', '  ', '  ', '  ', '  ', '  ', '  ', '  '],
    ['  ', '  ', '  ', '  ', '  ', '  ', '  ', '  '],
    ['  ', '  ', '  ', '  ', '  ', '  ', '  ', '  '],
    ['  ', '  ', '  ', '  ', '  ', '  ', '  ', '  '],
    ['II', 'II', 'II', 'II', 'II', 'II', 'II', 'II']
]).
```
<p align="center">board.pl</p>

<h4 align="center">Estado Intermediário do Jogo</h4>

> Num estado intermédio do jogo, ```Player``` pode ser ou o ```player1``` ou o ```player2```. ```TotalMoves``` vai ser um número maior que 0 correspondente ao número total de jogadas até este estado. Uma ```Board``` num estado intermédio poderia estar, por exemplo, da seguinte maneira:

```prolog
['  ', ' 2', '  ', '  ', ' 2', ' 2', ' 2', ' 2'],
[' 1', ' 1', '  ', '  ', ' 1', '  ', '  ', '  '],
[' 2', '  ', '  ', '  ', '  ', ' 1', '  ', '  '],
['  ', '  ', '  ', '  ', '  ', '  ', '  ', '  '],
['  ', '  ', 'II', '  ', '  ', ' I', ' I', '  '],
['  ', ' I', ' I', '  ', ' I', '  ', ' I', '  '],
['  ', 'II', '  ', '  ', 'II', 'II', '  ', 'II']
```

<h4 align="center">Estado Final do Jogo</h4>

> Num estado final do jogo, ```Player``` pode ser ou o ```player1``` ou o ```player2```. ```TotalMoves``` vai ser o número total de _moves_ do jogo inteiro pelos dois jogadores que depois vai ser usado para calcular o número de jogadas feitas pelo vencedor. Uma ```Board``` num estado final poderia estar, por exemplo, da seguinte maneira:

```prolog
[' 2', '  ', '  ', '  ', '  ', '  ', '  ', '  '],
['  ', ' 1', '  ', '  ', '  ', '  ', ' 1', ' 1'],
[' I', ' 1', '  ', ' 2', ' 2', '  ', ' 2', ' 2'],
[' 1', ' I', '  ', '  ', '  ', '  ', '  ', ' 1'],
[' 1', '  ', '  ', ' I', ' I', '  ', '  ', ' I'],
['  ', '  ', '  ', ' I', ' I', ' I', '  ', '  '],
['  ', ' I', '  ', '  ', ' I', ' I', ' I', '  ']
```

<h3 align="center">Visualização do Estado do Jogo</h3>

> Ao iniciar o jogo, o utilizador vai escolher se deseja jogar(```Play```), ir para o menu de ```Help``` ou sair do jogo (```Quit```). Se escolher jogar vai ser apresentado outro menu onde o utilizador escolhe em que modo quer jogar _Murus Gallicus_. Após isto se tiver escolhido alguma daas opções que involvam pelo menos um _bot_ vai aparecer outro menu para escolher entre os _bots greedy_ ou _random_.

```prolog
--------------------------------------
      Welcome to Murus Gallicus       
--------------------------------------

Main Menu
1. Play
2. Help
3. Quit
Enter your choice (1, 2, or 3): 1. % play selected
Choose how to play the game:
1. Player vs Player
2. Player vs Computer
3. Computer vs Computer
Enter your choice (1, 2, or 3): |: 3. % bot vs bot mode selected
Select the type of game:
1. Greedy vs. Greedy
2. Random vs. Random
3. Random vs. Greedy
4. Greedy vs. Random
```
<p align="center"> main.pl </p>

> A leitura do _input_ do utilizador é feita com os predicatos ```read_option/1``` e ```read_game_option/1``` e subsequentemente validade e processada por outros predicados contidos neles. 

```prolog
read_option(Choice) :-
    write('Enter your choice (1, 2, or 3): '),
    read(Choice),
    validate_choice(Choice).
```
<p align="center"> main.pl </p>

> No menu de escolha de bot (_greedy_ ou _random_), dependendo da escolha do utilizador vai mudar o valor dos factos ```isGreedyBot``` ou ```isRandomBot``` para um ```player1``` ou ```player1``` para _true_ (1), fazendo com que esse jogador seja interpretado como um _bot_. Tome-se o exemplo abaixo:

```prolog
GameTypeChoice = 2 ->
            retract(isRandomBot(player2, 0)),
            asserta(isRandomBot(player2, 1)),
            asserta(player_name(player2, 'Random Bot'))
```

> Após isto é inicializado o estado inicial com o predicado ```initial_state(+GameState)```.

```prolog
% initial_state(+GameState)
% initialize gamestate with initial board, player1 and 0 total moves
initial_state([Board, player1, 0]) :-
    main_menu,
    clear_console,
    title,
    initial_board(Board), display_game([Board, player1, 0]).
```

> Depois de termos o GameState inicializado vai ser chamado o predicado ```display_game(+GameState)``` que vai dar _print_ á _board_ com as peças nas posições corretas no ecrã do utilizador, usando os predicado ```print_board_separator``` e ```print_rows(+Rows, -RowNumber)``` .

```prolog
display_game(GameState) :-
    nl,
    print_board_separator,
    [Board, _, _] = GameState,
    print_rows(Board, 1).
```

> Também foram desenvolvidos dois predicados ```piece_info(+PieceType, -Player)``` e ```symbol(+PieceType, -Piece)``` para saber a que jogador é que cada peça pertence e a que peça cada símbolo corresponde.

```prolog
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
```

<h3 align="center">Validação e Execução de Movimentos</h3>

> Qualquer jogada que se pretenda executar requer, em primeiro lugar, uma validação cuidadosa que assegure a conformidade com as regras do jogo. Apenas após passar por esta verificação, poderá a jogada ser executada.

> A verificação consiste nos seguintes passos:
> - Verificar se a peça que se pretende mover é uma torre
> - Verificar se a posição inicial, final, e intermédia (da peça que se move atrás) se encontram todas dentro dos limites do tabuleiro
> - Verificar se a peça que se pretende mover pertence, de facto, ao jogador que a está a tentar mover
> - Se o destino final se econtra, exatamente, a duas casas de distância:
>   - Verificar se no destino final e no destino intermédio se encontram espaços vazios ou peças singulares pertencentes ao jogador
> - Se o destino final se encontra, exatamente, a uma casa de distância:
>   - Verificar se a peça de destino é uma peça unitária e pertencente ao oponente


<h3 align="center">Lista de Movimentos Válidos</h3>

> Para obtermos a lista de movimentos válidos usamos o predicado ```valid_moves(+GameState, -ListOfMoves)``` que usando os predicados ```findall\3``` e ```validate(+GameState, +InitialPosition, +FinalPosition)``` encontra todos os movimentos legais apartir de qualquer posição dentro do tabuleiro. 

```prolog
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
```

> Usamos este predicado para o desenvolvimento dos _bots_. Para o _bot random_ selecionava dentro da lista de _moves_ possíveis um _random move_ e para o _bot greedy_ selecionava o melhor _move_ tendo em conta vários fatores.

<h3 align="center">Fim de Jogo</h3>

> Em cada ciclo do *gameloop* é feita a contagem do número de torres que cada jogador tem. Caso um jogador fique com 0 torres disponíveis, ou seja, fique sem jogadas possíveis, o seu oponente é declarado vencedor. Também é possível ganhar o jogo chegando à base do oponente, pelo que esta verificação também é feita após cada jogada, sendo também adequadamente declarado um vencedor para este caso. 

<h3 align="center">Avaliação do Estado do Jogo</h3>

> O estado atual do jogo é avaliado através do predicado `value/3`, que tem em conta o número de torres de ambos os jogadores, bem como o valor posicional (o quão perto a peça está da base inimiga) de cada jogada. Esta avaliação é feita para jogadas do *bot* *greedy*, e consiste na seguinte fórmula: `Value = PositionValue + 100 * (TowerCount - EnemyTowerCount)`. Esta fórmula permite um equilíbrio entre jogadas conservadoras (i.e. não desperdiçar torres) e tentar alcançar a base inimiga, o que em termos práticos se observa quando o *bot* ganha, quase sempre, por *stalemate*.

<h3 align="center">Jogada do computador</h3>

> Para os bots decidirem qual movimento realizar, foram realizados dois métodos:
_random_ e _greedy_.
> <br><br> Para o _bot random_ usamos o predicado ```valid_moves(+GameState, -ListOfMoves)``` de modo a obtermos a lista de todos os _moves_ possíveis e depois basta nos fazer ```random_member/2``` de modo a escolher aleatoriamente um desses _moves_.

```prolog
choose_move(GameState, ColI-RowI-ColF-RowF) :-
    [_, Player, _] = GameState,
    isRandomBot(Player, 1),
    valid_moves(GameState, ListOfMoves),
    random_member(ColI-RowI-ColF-RowF, ListOfMoves).
```

> Para o _greedy bot_ utilizamos o predicado ```value\3``` em conjunto com o algoritmo _minimax_ para avaliar todas as possíveis jogadas futuras e selecionar aquela que oferece a maior vantagem.
> <br><br> O predicado ```minimax(+GameState, +Player, +Type, -Value)``` é uma implementação recursiva do algoritmo _Minimax_ para avaliar o valor de um estado de jogo em relação a um jogador específico. Ele alterna entre os modos "min" e "max" ao longo de uma _search tree_, limitada a uma profundidade de 2 níveis (devido a questões de eficiência). Para cada jogada válida no estado atual do jogo, o algoritmo calcula o valor do novo estado resultante, considerando jogadas futuras. O resultado final é determinado pelo tipo de nó (min ou max) e representa a melhor jogada possível para o jogador em questão.

```prolog
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
```

> Se dois ou mais movimentos têm o mesmo valor de avaliação, um deles é selecionado aleatoriamente para evitar preferência em relação às jogadas.

<h2 align="center">Conclusões</h2>

> Implementos com sucesso o jogo _Murus Gallicus_. O jogo pode ser jogado em vários modos sendo estes os seguintes: _Player vs Player_, _Player vs Computer_ e _Computer vs Computer_. Todas as interações são sólidas, e verificamos constantemente o estado do jogo para garantir a validade deste.
> <br><br> Ao longo do desenvolvimento deste jogo deparamo-nos com alguns problemas sendo estes os seguintes: verificação de erros nos _inputs_ (especialmente nas jogadas) e limpeza de dados. Com uma melhor gestão do tempo podiamos ter melhorado o nosso _bot Greedy_, fazendo com que fosse mais eficaz e mais efetivo.

<h2 align="center">Bibliografia</h2>

> Ao longo do desenvolvimento deste projeto usamos os seguintes recursos:
> 
> - [BoardGameGeek](https://boardgamegeek.com/boardgame/55131/murus-gallicus)
> - [Wikipedia](https://en.wikipedia.org/wiki/Murus_Gallicus_(game))
> - [Iggamecenter](https://www.iggamecenter.com/en/rules/murusgallicus)
