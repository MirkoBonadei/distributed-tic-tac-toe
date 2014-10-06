-module(board_tests).
-include_lib("eunit/include/eunit.hrl").

%% row_of_test() ->
%%   ?assertEqual(1, board:row_of(0)),
%%   ?assertEqual(1, board:row_of(1)),
%%   ?assertEqual(1, board:row_of(2)),
%%   ?assertEqual(2, board:row_of(3)),
%%   ?assertEqual(2, board:row_of(4)),
%%   ?assertEqual(2, board:row_of(5)),
%%   ?assertEqual(3, board:row_of(6)),
%%   ?assertEqual(3, board:row_of(7)),
%%   ?assertEqual(3, board:row_of(8)).

%% column_of_test() ->
%%   ?assertEqual(1, board:column_of(0)),
%%   ?assertEqual(2, board:column_of(1)),
%%   ?assertEqual(3, board:column_of(2)),
%%   ?assertEqual(1, board:column_of(3)),
%%   ?assertEqual(2, board:column_of(4)),
%%   ?assertEqual(3, board:column_of(5)),
%%   ?assertEqual(1, board:column_of(6)),
%%   ?assertEqual(2, board:column_of(7)),
%%   ?assertEqual(3, board:column_of(8)).

%% position_of_test() ->
%%   ?assertEqual(0, board:position_of(1, 1)),
%%   ?assertEqual(1, board:position_of(1, 2)),
%%   ?assertEqual(2, board:position_of(1, 3)),
%%   ?assertEqual(3, board:position_of(2, 1)),
%%   ?assertEqual(4, board:position_of(2, 2)),
%%   ?assertEqual(5, board:position_of(2, 3)),
%%   ?assertEqual(6, board:position_of(3, 1)),
%%   ?assertEqual(7, board:position_of(3, 2)),
%%   ?assertEqual(8, board:position_of(3, 3)).

%% move_test() ->
%%   ?assertEqual(
%%      [x, none, none, none, none, none, none, none, none],
%%      board:move(board:create(), {1, 1, x})
%%   ),
%%   ?assertEqual(
%%      [none, x, none, none, none, none, none, none, none],
%%      board:move(board:create(), {1, 2, x})
%%   ),
%%   ?assertEqual(
%%      [none, none, none, none, none, none, none, none, x],
%%      board:move(board:create(), {3, 3, x})
%%   ),
%%   ?assertEqual(
%%      [none, none, none, none, x, none, none, none, none],
%%      board:move(board:create(), {2, 2, x})
%%   ).

%% all_moves_are_available_at_the_beginning_test() ->
%%   Board = board:create(),
%%   Moves = board:available_moves(Board),
%%   ?assertEqual(
%%      [{1,1},{1,2},{1,3},{2,1},{2,2},{2,3},{3,1},{3,2},{3,3}],
%%      Moves
%%   ).

%% all_moves_are_available_but_one_after_first_move_test() ->
%%   Board = board:move(board:create(), {1, 1, x}),
%%   Moves = board:available_moves(Board),
%%   ?assertEqual(
%%      [{1,2},{1,3},{2,1},{2,2},{2,3},{3,1},{3,2},{3,3}],
%%      Moves
%%   ).

%% only_last_move_test() ->
%%   Board = lists:foldl(
%%     fun(Move, Board) -> board:move(Board, Move) end,
%%     board:create(),
%%     [{1,1,x}, {1,2,o}, {1,3,x}, {2,1,x}, {2,2,o}, {2,3,x}, {3,1,o}, {3,2,x}]
%%   ),
%%   Moves = board:available_moves(Board),
%%   ?assertEqual([{3,3}], Moves).

%% after_created_the_board_is_open_test() ->
%%   ?assertEqual(open, board:check(board:create())).

%% check_win_for_x_test() ->
%%   ?assertEqual({win, x}, board:check([
%%                                 x,x,x,
%%                                 none,none,none,
%%                                 none,none,none
%%                                ])),
%%   ?assertEqual({win, x}, board:check([
%%                                 x,none,none,
%%                                 x,none,none,
%%                                 x,none,none
%%                                ])),
%%   ?assertEqual({win, x}, board:check([
%%                                 x,none,none,
%%                                 none,x,none,
%%                                 none,none,x
%%                                ])),
%%   pass.

%% check_tie_test() ->
%%   ?assertEqual(tie, board:check([x,o,x,
%%                            x,o,o,
%%                            o,x,o
%%                           ])).

