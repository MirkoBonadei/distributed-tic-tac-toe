distributed-tic-tac-toe
=======================

Distributed Tic-Tac-Toe kata in Erlang.

The goal of this repository is to grow an Erlang project from `git init` to a nice end-to-end experience, passing from a raw code base to a more robust and OTP compliant one.

The inspeiration for the Tic-Tac-Toe comes from this blog post of [_Jason Gorman_](http://codemanship.co.uk/parlezuml/blog/?postid=1196).

These are the instructions:

_Write a program that allows two players to have a game of Tic-Tac-Toe remotely. The program should know whose turn it is and enforce taking it in turns to make moves. The program should know whether a move made by a player is legal (i.e., is that square empty?), and be capable of detecting when the game is won or drawn. The outcomes should be reported to both players along with the current state of the game (e.g., by refreshing the screen)._

###to-do list:
* complete the illegal move detection (what if a player makes a move which is not inside the board?)
* add game drawn detection
* the outcome is not reported to both players at the moment
* modules `game` and `dummy_player` are not automatically tested
* board is 3x3 and would be really difficult to make it NxN. Refactoring is needed
