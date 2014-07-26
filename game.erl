-module(game).
-export([start/2, stop/1]).
-export([init/2]).

-include_lib("eunit/include/eunit.hrl").

-define(INTERVAL_MS, 1000).

%%% public functions

start(PlayerOne, PlayerTwo) ->
  spawn(?MODULE, init, [PlayerOne, PlayerTwo]).

init(PlayerOne, PlayerTwo) ->
  loop(1, [PlayerOne, PlayerTwo], board:new()).

stop(Pid) ->
  call(Pid, stop).

%%% private functions

loop(Turn, [PlayerOne, PlayerTwo], Board) ->
  Players = [PlayerOne, PlayerTwo],
  receive
    {request, Pid, stop} ->
      io:format("The game between ~p and ~p is now closed.~n The board is ~p~n", [PlayerOne, PlayerTwo, Board]),
      reply(Pid, ok)
  after
    ?INTERVAL_MS ->
      AvailablePositions = board:available_positions(Board),

      case AvailablePositions of
        [] ->
          io:format("The game between ~p and ~p is a draw~n", [PlayerOne, PlayerTwo]);
        _ ->
          PlayerInCharge = lists:nth((Turn rem 2) + 1, Players),
          io:format("It is ~p turn...~n", [PlayerInCharge]),
          {X, Y} = dummy_player:next_move(PlayerInCharge, AvailablePositions),
          NewBoard = board:make_move(Board, PlayerInCharge, X, Y),
          io:format("The new board is ~p~n", [NewBoard]),
          case board:has_been_won_by(NewBoard, PlayerInCharge) of
            true ->
              io:format("Player ~p has won this tic tac toe~n", [PlayerInCharge]);
            false ->
              loop(Turn + 1, Players, NewBoard)
          end
      end
  end.

call(Pid, Msg) ->
  Pid ! {request, self(), Msg},
  receive
    {reply, Reply} -> Reply
  end.

reply(Pid, Reply) ->
  Pid ! {reply, Reply}.

-ifdef(TEST).

-endif.
