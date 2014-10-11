-module(board).
-export([create/0, display/1]).
-export([available_moves/1, move/2, check/1]).

create() ->
    [none, none, none, none, none, none, none, none, none].

display(Board) ->
    io:format("Board: ~p~n", [Board]).

available_moves(Board) ->
    lists:reverse(
      lists:foldl(
        fun
            ({none, Position}, Moves) ->
                         [{row_of(Position), column_of(Position)} | Moves];
            ({_, _}, Moves) -> Moves
                 end,
        [],
        lists:zip(Board, lists:seq(0, length(Board) - 1))
       )
     ).

move(Board, {Row, Column, Symbol}) when (Symbol =:= x) or (Symbol =:= o) ->
    Position = position_of(Row, Column),
    case lists:nth(Position + 1, Board) of
        none -> lists:sublist(Board, Position) ++ [Symbol] ++ lists:nthtail(Position + 1, Board);
        _ -> Board
    end.

                                                % symbol_at(Board, {Row, Column}) ->
                                                %   lists:nth(position_of(Row, Column) + 1, Board).

check([X,X,X,_,_,_,_,_,_]) when (X =:= x) or (X =:= o)-> {win, X};
check([_,_,_,X,X,X,_,_,_]) when (X =:= x) or (X =:= o)-> {win, X};
check([_,_,_,_,_,_,X,X,X]) when (X =:= x) or (X =:= o)-> {win, X};
check([X,_,_,X,_,_,X,_,_]) when (X =:= x) or (X =:= o)-> {win, X};
check([_,X,_,_,X,_,_,X,_]) when (X =:= x) or (X =:= o)-> {win, X};
check([_,_,X,_,_,X,_,_,X]) when (X =:= x) or (X =:= o)-> {win, X};
check([X,_,_,_,X,_,_,_,X]) when (X =:= x) or (X =:= o)-> {win, X};
check([_,_,X,_,X,_,X,_,_]) when (X =:= x) or (X =:= o)-> {win, X};
check(Board) ->
    case lists:all(fun(X) -> (X =:= x) or (X =:= o) end, Board) of
        true -> tie;
        _ -> open
    end.


row_of(Position) when (Position >= 0) and (Position =< 8) ->
    (Position div 3) + 1.

column_of(Position) when (Position >= 0) and (Position =< 8) ->
    (Position rem 3) + 1.

position_of(Row, Column) when (Row >= 1) and (Row =< 3) and (Column >= 1) and (Column =< 3) ->
    (Row - 1) * 3 + (Column - 1).

%% internal funcions testing
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

row_of_test() ->
    ?assertEqual(1, row_of(0)),
    ?assertEqual(1, row_of(1)),
    ?assertEqual(1, row_of(2)),
    ?assertEqual(2, row_of(3)),
    ?assertEqual(2, row_of(4)),
    ?assertEqual(2, row_of(5)),
    ?assertEqual(3, row_of(6)),
    ?assertEqual(3, row_of(7)),
    ?assertEqual(3, row_of(8)).

column_of_test() ->
    ?assertEqual(1, column_of(0)),
    ?assertEqual(2, column_of(1)),
    ?assertEqual(3, column_of(2)),
    ?assertEqual(1, column_of(3)),
    ?assertEqual(2, column_of(4)),
    ?assertEqual(3, column_of(5)),
    ?assertEqual(1, column_of(6)),
    ?assertEqual(2, column_of(7)),
    ?assertEqual(3, column_of(8)).

position_of_test() ->
    ?assertEqual(0, position_of(1, 1)),
    ?assertEqual(1, position_of(1, 2)),
    ?assertEqual(2, position_of(1, 3)),
    ?assertEqual(3, position_of(2, 1)),
    ?assertEqual(4, position_of(2, 2)),
    ?assertEqual(5, position_of(2, 3)),
    ?assertEqual(6, position_of(3, 1)),
    ?assertEqual(7, position_of(3, 2)),
    ?assertEqual(8, position_of(3, 3)).

-endif.
