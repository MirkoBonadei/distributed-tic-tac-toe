-module(game).
-export([create/0, destroy/1, join/2]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2]).

-behaviour(gen_server).

-define(TICK_TIME, 2000).
-define(TICK_MESSAGE, "tick").

-define(WAITING_FOR_PLAYERS, "waiting_for_players").
-define(ONGOING, "ongoing").
-define(TERMINATED, "terminated").

%%% Game API

create() ->
  gen_server:start_link(?MODULE, [], []).

destroy(GamePid) ->
  gen_server:cast(GamePid, stop).

join(GamePid, PlayerPid) ->
  gen_server:call(GamePid, {join, PlayerPid}).

%%% gen_server callbacks

init([]) ->
  {ok, [?WAITING_FOR_PLAYERS, [], board:new()]}.

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_call({join, PlayerPid}, _From, [?WAITING_FOR_PLAYERS, [], Board]) ->
  {reply, {ok, welcome}, [?WAITING_FOR_PLAYERS, [PlayerPid], Board]};
handle_call({join, Pid}, _From, [State, [Pid], Board]) ->
  {reply, {error, already_joined}, [State, [Pid], Board]};
handle_call({join, PlayerPid}, _From, [?WAITING_FOR_PLAYERS, [PlayerOne], Board]) ->
  Timer = erlang:send_after(?TICK_TIME, self(), ?TICK_MESSAGE),
  {reply, {ok, welcome}, [?ONGOING, [PlayerOne, PlayerPid], Board, Timer]};
handle_call({join, Pid}, _From, [State, Players = [Pid, _], Board, Timer]) ->
  {reply, {error, already_joined}, [State, Players, Board, Timer]};
handle_call({join, Pid}, _From, [State, Players = [_, Pid], Board, Timer]) ->
  {reply, {error, already_joined}, [State, Players, Board, Timer]};
handle_call({join, _Pid}, _From, [State, Players, Board, Timer]) when length(Players) =:= 2 ->
  {reply, {error, full}, [State, Players, Board, Timer]}.

handle_info(?TICK_MESSAGE, [?ONGOING, [NextPlayer, PausedPlayer], Board, OldTimer]) ->
  erlang:cancel_timer(OldTimer),
  {X, Y} = player:next_move(NextPlayer, Board),
  NewBoard = board:make_move(Board, NextPlayer, X, Y),
  io:format("~p~n", [NewBoard]),
  case board:has_been_won_by(NewBoard, NextPlayer) of
    true ->
      player:winner(NextPlayer),
      player:loser(PausedPlayer),
      {stop, normal, [?TERMINATED, [PausedPlayer, NextPlayer], NewBoard]};
    false ->
      case board:available_positions(NewBoard) of
        [] ->
          player:draw(NextPlayer),
          player:draw(PausedPlayer),
          {stop, normal, [?TERMINATED, [PausedPlayer, NextPlayer], NewBoard]};
        _ ->
          NewTimer = erlang:send_after(?TICK_TIME, self(), ?TICK_MESSAGE),
          {noreply, [?ONGOING, [PausedPlayer, NextPlayer], NewBoard, NewTimer]}
      end
  end.

terminate(_Reason, _State) ->
  ok.
