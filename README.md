## Distributed Tic-Tac-Toe

Distributed Tic-Tac-Toe kata in Erlang.

The goal of this repository is to grow an Erlang project from `git init` to a nice end-to-end experience, passing from a raw code base to a more robust and OTP compliant one.

The inspiration for the Tic-Tac-Toe comes from this blog post of [_Jason Gorman_](http://codemanship.co.uk/parlezuml/blog/?postid=1196).

These are the instructions:

_Write a program that allows two players to have a game of Tic-Tac-Toe remotely. The program should know whose turn it is and enforce taking it in turns to make moves. The program should know whether a move made by a player is legal (i.e., is that square empty?), and be capable of detecting when the game is won or drawn. The outcomes should be reported to both players along with the current state of the game (e.g., by refreshing the screen)._

### TODO:
* modules `game` is not automatically tested [90% done, there is some refactoring to do and to test the over state]
* Test player join with QuickCheck or [PropEr](http://proper.softlab.ntua.gr/doc/overview-summary.html)

### USAGE
The usage of the game is really raw in this first phase of the project:

```erlang
83> c(board).
{ok,board}
94> c(dummy_player).
{ok,dummy_player}
95> c(game).
{ok,game}
96> G = game:start(dummy_player:start("Darth Vader"), dummy_player:start("Luke Skywalker")).
<0.676.0>
It is <0.675.0> turn...
Player "Luke Skywalker" is choosing {1,1}
The new board is [[<0.675.0>,' ',' '],[' ',' ',' '],[' ',' ',' ']]
It is <0.674.0> turn...
Player "Darth Vader" is choosing {1,2}
The new board is [[<0.675.0>,<0.674.0>,' '],[' ',' ',' '],[' ',' ',' ']]
It is <0.675.0> turn...
Player "Luke Skywalker" is choosing {1,3}
The new board is [[<0.675.0>,<0.674.0>,<0.675.0>],[' ',' ',' '],[' ',' ',' ']]
It is <0.674.0> turn...
Player "Darth Vader" is choosing {2,1}
The new board is [[<0.675.0>,<0.674.0>,<0.675.0>],
                  [<0.674.0>,' ',' '],
                  [' ',' ',' ']]
It is <0.675.0> turn...
Player "Luke Skywalker" is choosing {2,2}
The new board is [[<0.675.0>,<0.674.0>,<0.675.0>],
                  [<0.674.0>,<0.675.0>,' '],
                  [' ',' ',' ']]
It is <0.674.0> turn...
Player "Darth Vader" is choosing {2,3}
The new board is [[<0.675.0>,<0.674.0>,<0.675.0>],
                  [<0.674.0>,<0.675.0>,<0.674.0>],
                  [' ',' ',' ']]
It is <0.675.0> turn...
Player "Luke Skywalker" is choosing {3,1}
The new board is [[<0.675.0>,<0.674.0>,<0.675.0>],
                  [<0.674.0>,<0.675.0>,<0.674.0>],
                  [<0.675.0>,' ',' ']]
Player <0.675.0> has won this tic tac toe
```
