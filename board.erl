-module(board).
-export([new/0, make_move/4, has_been_won_by/2]).

-include_lib("eunit/include/eunit.hrl").

-define(EMPTY_ROW, [' ', ' ', ' ']).

new() -> [?EMPTY_ROW, ?EMPTY_ROW, ?EMPTY_ROW].

make_move([Row1, Row2, Row3], Player, 1, Y) ->
  NewRow1 = update_row(Row1, Player, Y),
  [NewRow1, Row2, Row3];
make_move([Row1, Row2, Row3], Player, 2, Y) ->
  NewRow2 = update_row(Row2, Player, Y),
  [Row1, NewRow2, Row3];
make_move([Row1, Row2, Row3], Player, 3, Y) ->
  NewRow3 = update_row(Row3, Player, Y),
  [Row1, Row2, NewRow3].

has_been_won_by(Board, Player) ->
  is_there_a_winning_row_for_player(Board, Player) orelse
  is_there_a_winning_column_for_player(Board, Player) orelse
  is_there_a_winning_diagonal_for_player(Board, Player).

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
update_row_acc([' '|Tail], Player, Position, Acc, PosCounter) when Position == PosCounter ->
  update_row_acc(Tail, Player, Position, [Player|Acc], PosCounter + 1);
update_row_acc([Head|Tail], Player, Position, Acc, PosCounter) when Head == ' ' ->
  update_row_acc(Tail, Player, Position, [Head|Acc], PosCounter + 1);
update_row_acc(_Board, _Player, _Position, _Acc, _PosCounter) ->
  throw(illegal_move).


-ifdef(TEST).

should_return_new_board_test() ->
  ?assertEqual(
     [[' ', ' ', ' '], [' ', ' ', ' '], [' ', ' ', ' ']],
     board:new()
  ).

should_be_possible_to_make_legal_move_one_the_first_row_test() ->
  Board = board:new(),
  ?assertEqual(
     [['X', ' ', ' '], [' ', ' ', ' '], [' ', ' ', ' ']],
     board:make_move(Board, 'X', 1, 1)
  ).

should_be_possible_to_make_legal_move_one_the_second_row_test() ->
  Board = board:new(),
  ?assertEqual(
     [[' ', ' ', ' '], [' ', 'X', ' '], [' ', ' ', ' ']],
     board:make_move(Board, 'X', 2, 2)
  ).

should_be_possible_to_make_legal_move_one_the_third_row_test() ->
  Board = board:new(),
  ?assertEqual(
     [[' ', ' ', ' '], [' ', ' ', ' '], [' ', ' ', 'X']],
     board:make_move(Board, 'X', 3, 3)
  ).

should_reject_illegal_move_test() ->
  BoardTime0 = board:new(),
  BoardTime1 = board:make_move(BoardTime0, 'X', 3, 3),
  ?assertThrow(
     illegal_move,
     board:make_move(BoardTime1, 'Y', 3, 3)
  ).

should_detect_winning_row_test() ->
  Board1 = [['X', 'X', 'X'],
            [' ', ' ', ' '],
            [' ', ' ', ' ']],
  Board2 = [[' ', ' ', ' '],
            ['X', 'X', 'X'],
            [' ', ' ', ' ']],
  Board3 = [[' ', ' ', ' '],
            [' ', ' ', ' '],
            ['X', 'X', 'X']],
  ?assert(board:has_been_won_by(Board1, 'X')),
  ?assert(board:has_been_won_by(Board2, 'X')),
  ?assert(board:has_been_won_by(Board3, 'X')).

should_detect_winning_column_test() ->
  Board1 = [['X', ' ', ' '],
            ['X', ' ', ' '],
            ['X', ' ', ' ']],
  Board2 = [[' ', 'X', ' '],
            [' ', 'X', ' '],
            [' ', 'X', ' ']],
  Board3 = [[' ', ' ', 'X'],
            [' ', ' ', 'X'],
            [' ', ' ', 'X']],
  ?assert(board:has_been_won_by(Board1, 'X')),
  ?assert(board:has_been_won_by(Board2, 'X')),
  ?assert(board:has_been_won_by(Board3, 'X')).

should_detect_winning_diagonal_test() ->
  Board1 = [['X', ' ', ' '],
            [' ', 'X', ' '],
            [' ', ' ', 'X']],
  Board2 = [[' ', ' ', 'X'],
            [' ', 'X', ' '],
            ['X', ' ', ' ']],
  ?assert(board:has_been_won_by(Board1, 'X')),
  ?assert(board:has_been_won_by(Board2, 'X')).

-endif.
