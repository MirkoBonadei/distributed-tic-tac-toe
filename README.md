[![Build Status](https://travis-ci.org/MirkoBonadei/distributed-tic-tac-toe.svg?branch=master)](https://travis-ci.org/MirkoBonadei/distributed-tic-tac-toe)
## Distributed Tic-Tac-Toe
Distributed Tic-Tac-Toe kata in Erlang.

The goal of this repository is to grow an Erlang project from `git init` to a nice end-to-end experience, passing from a raw code base to a more robust and OTP compliant one.

The inspiration for the Tic-Tac-Toe comes from this blog post of [_Jason Gorman_](http://codemanship.co.uk/parlezuml/blog/?postid=1196).

These are the instructions:

_Write a program that allows two players to have a game of Tic-Tac-Toe remotely. The program should know whose turn it is and enforce taking it in turns to make moves. The program should know whether a move made by a player is legal (i.e., is that square empty?), and be capable of detecting when the game is won or drawn. The outcomes should be reported to both players along with the current state of the game (e.g., by refreshing the screen)._

### TODO
* modules `game` is not automatically tested `[90% done, there is some refactoring to do and to test the over state]`
* acceptance test
* player module test
* supervision and worst case management
* application
* release
* HTTP api and easy web interface
* test the FSM with QuickCheck or [PropEr](http://proper.softlab.ntua.gr/doc/overview-summary.html)
* write a usage section for this README
