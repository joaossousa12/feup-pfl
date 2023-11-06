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
> A visualização do estado do jogo é implementada no predicado ```display_game(+GameState)```. 

<h3 align="center">Validação e Execução de Movimentos</h3>

<h3 align="center">Lista de Movimentos Válidos</h3>

<h3 align="center">Fim de Jogo</h3>

<h3 align="center">Avaliação do Estado do Jogo</h3>

<h2 align="center">Conclusões</h2>

> Implementos com sucesso o jogo _Murus Gallicus_. O jogo pode ser jogado em vários modos sendo estes os seguintes: _Player vs Player_, _Player vs Computer_ e _Computer vs Computer_. Todas as interações são sólidas, e verificamos constantemente o estado do jogo para garantir a validade deste.
> <br><br> Ao longo do desenvolvimento deste jogo deparamo-nos com alguns problemas sendo estes os seguintes: verificação de erros nos _inputs_ (especialmente nas jogadas) e limpeza de dados. Com uma melhor gestão do tempo podiamos ter melhorado o nosso _bot Greedy_, fazendo com que fosse mais eficaz e mais efetivo.

<h2 align="center">Bibliografia</h2>

> Ao longo do desenvolvimento deste projeto usamos os seguintes recursos:
> 
> - [BoardGameGeek](https://boardgamegeek.com/boardgame/55131/murus-gallicus)
> - [Wikipedia](https://en.wikipedia.org/wiki/Murus_Gallicus_(game))
> - [Iggamecenter](https://www.iggamecenter.com/en/rules/murusgallicus)




