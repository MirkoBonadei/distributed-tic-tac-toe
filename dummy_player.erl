-module(dummy_player).
-export([start/1, stop/1, next_move/2]).
-export([init/1]).

-include_lib("eunit/include/eunit.hrl").

% public functions

start(Name) ->
  spawn(?MODULE, init, [Name]).

init(Name) ->
  loop(Name).

stop(PlayerPid) ->
  call(PlayerPid, stop).

next_move(PlayerPid, AvailableMoves) ->
  call(PlayerPid, {next_move, AvailableMoves}).

% private functions

loop(Name) ->
  receive
    {request, Pid, stop} ->
      io:format("Player ~p is going down~n", [Name]),
      reply(Pid, ok);
    {request, Pid, {next_move, AvailableMoves}} ->
      Move = random_move(AvailableMoves),
      io:format("Player ~p is choosing ~p~n", [Name, Move]),
      reply(Pid, Move),
      loop(Name)
  end.

call(Pid, Msg) ->
  Pid ! {request, self(), Msg},
  receive
    {reply, Reply} -> Reply
  end.

reply(Pid, Reply) ->
  Pid ! {reply, Reply}.

random_move(AvailableMoves) ->
  <<A1, A2, A3>> = crypto:strong_rand_bytes(3),
  random:seed(A1, A2, A3),
  lists:nth(random:uniform(length(AvailableMoves)), AvailableMoves).


-ifdef(TEST).
-endif.
