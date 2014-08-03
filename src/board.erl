-module(board).
-export([new/0, make_move/4, has_been_won_by/2, available_positions/1]).

-include_lib("eunit/include/eunit.hrl").

-define(EMPTY_CELL, ' ').
-define(EMPTY_ROW, [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL]).
-define(NUMBER_OF_ROWS, 3).
-define(NUMBER_OF_COLUMNS, 3).

%%% public functions

new() -> [?EMPTY_ROW, ?EMPTY_ROW, ?EMPTY_ROW].

make_move(Board, Player, X, Y) when (X =< ?NUMBER_OF_ROWS) and (Y =< ?NUMBER_OF_COLUMNS) ->
  make_move_helper(Board, Player, X, Y);
make_move(_Board, _Player, _X, _Y) ->
  throw(illegal_move).

has_been_won_by(Board, Player) ->
  is_there_a_winning_row_for_player(Board, Player) orelse
  is_there_a_winning_column_for_player(Board, Player) orelse
  is_there_a_winning_diagonal_for_player(Board, Player).

available_positions([Row1, Row2, Row3]) ->
  available_positions_by_row(1, Row1) ++ available_positions_by_row(2, Row2) ++ available_positions_by_row(3, Row3).

%%% private functions

make_move_helper([Row1, Row2, Row3], Player, 1, Y) ->
  NewRow1 = update_row(Row1, Player, Y),
  [NewRow1, Row2, Row3];
make_move_helper([Row1, Row2, Row3], Player, 2, Y) ->
  NewRow2 = update_row(Row2, Player, Y),
  [Row1, NewRow2, Row3];
make_move_helper([Row1, Row2, Row3], Player, 3, Y) ->
  NewRow3 = update_row(Row3, Player, Y),
  [Row1, Row2, NewRow3].

available_positions_by_row(RowNumber, Row) ->
  available_positions_by_row_acc(RowNumber, Row, [], 1).

available_positions_by_row_acc(_RowNumber, [], Acc, _Position) ->
  lists:reverse(Acc);
available_positions_by_row_acc(RowNumber, [?EMPTY_CELL|Tail], Acc, Position) ->
  available_positions_by_row_acc(RowNumber, Tail, [{RowNumber, Position}|Acc], Position + 1);
available_positions_by_row_acc(RowNumber, [_Head|Tail], Acc, Position) ->
  available_positions_by_row_acc(RowNumber, Tail, Acc, Position + 1).

is_there_a_winning_row_for_player(Board, Player) ->
  lists:any(fun(Row) -> lists:all(fun(Pos) -> Pos == Player end, Row) end, Board).

is_there_a_winning_column_for_player(Board, Player) ->
  lists:any(
    fun(Column) -> lists:all(
                     fun(Row) -> lists:nth(Column, Row) == Player end,
                     Board
                    )
    end,
    [1, 2, 3]
  ).

is_there_a_winning_diagonal_for_player(Board, Player) ->
  is_main_diagonal_winning(Board, Player) orelse
  is_minor_diagonal_winning(Board, Player).

is_main_diagonal_winning([[Player, _, _], [_, Player, _], [_, _, Player]], Player) ->
  true;
is_main_diagonal_winning(_Board, _Player) ->
  false.

is_minor_diagonal_winning([[_, _, Player], [_, Player, _], [Player, _, _]], Player) ->
  true;
is_minor_diagonal_winning(_Board, _Player) ->
  false.


update_row(Row, Player, Position) ->
  update_row_acc(Row, Player, Position, [], 1).

update_row_acc([], _Player, _Position, Acc, _PosCounter) ->
  lists:reverse(Acc);
update_row_acc([?EMPTY_CELL|Tail], Player, Position, Acc, PosCounter) when Position == PosCounter ->
  update_row_acc(Tail, Player, Position, [Player|Acc], PosCounter + 1);
update_row_acc([Head|_Tail], _Player, Position, _Acc, PosCounter) when (Position == PosCounter) and (Head =/= ?EMPTY_CELL) ->
  throw(illegal_move);
update_row_acc([Head|Tail], Player, Position, Acc, PosCounter) ->
  update_row_acc(Tail, Player, Position, [Head|Acc], PosCounter + 1).


-ifdef(TEST).

should_return_new_board_test() ->
  ?assertEqual(
     [[?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL], [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL], [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL]],
     board:new()
  ).

should_be_possible_to_make_legal_move_one_the_first_row_test() ->
  Board = board:new(),
  ?assertEqual(
     [['X', ?EMPTY_CELL, ?EMPTY_CELL], [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL], [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL]],
     board:make_move(Board, 'X', 1, 1)
  ).

should_be_possible_to_make_legal_move_one_the_second_row_test() ->
  Board = board:new(),
  ?assertEqual(
     [[?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL], [?EMPTY_CELL, 'X', ?EMPTY_CELL], [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL]],
     board:make_move(Board, 'X', 2, 2)
  ).

should_be_possible_to_make_legal_move_one_the_third_row_test() ->
  Board = board:new(),
  ?assertEqual(
     [[?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL], [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL], [?EMPTY_CELL, ?EMPTY_CELL, 'X']],
     board:make_move(Board, 'X', 3, 3)
  ).

should_be_possible_to_make_second_legal_move_test() ->
  Board = [['Y', ?EMPTY_CELL, ?EMPTY_CELL], [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL], [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL]],
  ?assertEqual(
     [['Y', 'X', ?EMPTY_CELL], [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL], [?EMPTY_CELL, ?EMPTY_CELL, ?EMPTY_CELL]],
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

-endif.