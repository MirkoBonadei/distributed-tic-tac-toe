-module(board).
-export([new/0, make_move/4, has_been_won_by/2, available_positions/1]).

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
