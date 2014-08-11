-module(board_tests).

-include_lib("eunit/include/eunit.hrl").

-define(EMPTY_CELL, ' ').

should_return_new_board_test() ->
  ?assertEqual(
     [
      [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL],
      [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL],
      [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL]
     ],
     board:new()
  ).

should_be_possible_to_make_legal_move_one_the_first_row_test() ->
  Board = board:new(),
  ?assertEqual(
     [
      ['X', ?EMPTY_CELL, ?EMPTY_CELL],
      [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL],
      [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL]
     ],
     board:make_move(Board, 'X', 1, 1)
  ).

should_be_possible_to_make_legal_move_one_the_second_row_test() ->
  Board = board:new(),
  ?assertEqual(
     [
      [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL],
      [?EMPTY_CELL, 'X', ?EMPTY_CELL],
      [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL]
     ],
     board:make_move(Board, 'X', 2, 2)
  ).

should_be_possible_to_make_legal_move_one_the_third_row_test() ->
  Board = board:new(),
  ?assertEqual(
     [
      [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL],
      [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL],
      [?EMPTY_CELL, ?EMPTY_CELL, 'X']
     ],
     board:make_move(Board, 'X', 3, 3)
  ).

should_be_possible_to_make_second_legal_move_test() ->
  Board = [
           ['Y', ?EMPTY_CELL, ?EMPTY_CELL],
           [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL],
           [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL]
          ],
  ?assertEqual(
     [
      ['Y', 'X', ?EMPTY_CELL],
      [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL],
      [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL]
     ],
     board:make_move(Board, 'X', 1, 2)
  ).

should_reject_illegal_move_test() ->
  BoardTime0 = board:new(),
  BoardTime1 = board:make_move(BoardTime0, 'X', 3, 3),
  ?assertThrow(
     illegal_move,
     board:make_move(BoardTime1, 'Y', 3, 3)
  ).

x_must_be_within_the_borders_of_the_board_wneh_a_move_is_made_test() ->
  Board = board:new(),
  ?assertThrow(
     illegal_move,
     board:make_move(Board, 'Y', 4, 1)
  ).

y_must_be_within_the_borders_of_the_board_wneh_a_move_is_made_test() ->
  Board = board:new(),
  ?assertThrow(
     illegal_move,
     board:make_move(Board, 'Y', 1, 4)
  ).

should_detect_winning_row_test() ->
  Board1 = [['X', 'X', 'X'],
            [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL],
            [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL]],
  Board2 = [[?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL],
            ['X', 'X', 'X'],
            [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL]],
  Board3 = [[?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL],
            [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL],
            ['X', 'X', 'X']],
  ?assert(board:has_been_won_by(Board1, 'X')),
  ?assert(board:has_been_won_by(Board2, 'X')),
  ?assert(board:has_been_won_by(Board3, 'X')).

should_detect_winning_column_test() ->
  Board1 = [['X', ?EMPTY_CELL, ?EMPTY_CELL],
            ['X', ?EMPTY_CELL, ?EMPTY_CELL],
            ['X', ?EMPTY_CELL, ?EMPTY_CELL]],
  Board2 = [[?EMPTY_CELL, 'X', ?EMPTY_CELL],
            [?EMPTY_CELL, 'X', ?EMPTY_CELL],
            [?EMPTY_CELL, 'X', ?EMPTY_CELL]],
  Board3 = [[?EMPTY_CELL, ?EMPTY_CELL, 'X'],
            [?EMPTY_CELL, ?EMPTY_CELL, 'X'],
            [?EMPTY_CELL, ?EMPTY_CELL, 'X']],
  ?assert(board:has_been_won_by(Board1, 'X')),
  ?assert(board:has_been_won_by(Board2, 'X')),
  ?assert(board:has_been_won_by(Board3, 'X')).

should_detect_winning_diagonal_test() ->
  Board1 = [['X', ?EMPTY_CELL, ?EMPTY_CELL],
            [?EMPTY_CELL, 'X', ?EMPTY_CELL],
            [?EMPTY_CELL, ?EMPTY_CELL, 'X']],
  Board2 = [[?EMPTY_CELL, ?EMPTY_CELL, 'X'],
            [?EMPTY_CELL, 'X', ?EMPTY_CELL],
            ['X', ?EMPTY_CELL, ?EMPTY_CELL]],
  ?assert(board:has_been_won_by(Board1, 'X')),
  ?assert(board:has_been_won_by(Board2, 'X')).

should_detect_winning_minor_diagonal_with_a_busy_board_test() ->
  Board = [['X', 'Y', 'X'],
           ['Y', 'X', 'Y'],
           ['X', ?EMPTY_CELL, ?EMPTY_CELL]],
  ?assert(board:has_been_won_by(Board, 'X')).

should_return_available_positions_test() ->
  Board = [['X', ?EMPTY_CELL, ?EMPTY_CELL],
            [?EMPTY_CELL, 'X', ?EMPTY_CELL],
            [?EMPTY_CELL, ?EMPTY_CELL, 'X']],
  ?assertEqual(
     [{1, 2}, {1, 3}, {2, 1}, {2, 3}, {3, 1}, {3, 2}],
     board:available_positions(Board)
  ).
