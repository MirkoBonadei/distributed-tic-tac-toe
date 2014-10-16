-module(game).
-behaviour(gen_fsm).

-export([join/2, stop/0, start_link/0]).
-export([init/1, terminate/3, handle_info/3, handle_event/3, handle_sync_event/4, code_change/4]).
-export([waiting_for_players/3, play/2, over/2]).
-export([reply/2, ticker/0]).

-define(TICK_TIME, 2000).
-define(TICK_MESSAGE, tick).

% Public

-spec start_link() -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link() ->
  gen_fsm:start_link(
    ?MODULE,
    nil,
    []
  ).

-spec join(pid(), pid()) -> ok.
join(GamePid, PlayerPid) ->
  gen_fsm:sync_send_event(GamePid, {join, PlayerPid}).

-spec stop() -> ok.
stop() ->
  ok.

% gen_fsm Callbacks

init(_) ->
  InitialLoopData = #{
    next_player => nil, 
    other_player => nil, 
    board => board:create()
  },
  {ok, waiting_for_players, InitialLoopData}.

terminate(_Reason, _State, _LoopData) ->
  ok.

handle_info(tick, State, LoopData) ->
  gen_fsm:send_event(self(), ?TICK_MESSAGE),
  {next_state, State, LoopData}.

handle_event(_Event, State, LoopData) ->
  {next_state, State, LoopData}.

handle_sync_event(_Event, _From, State, LoopData) ->
  {next_state, State, LoopData}.

code_change(_OldVsn, State, LoopData, _Extra) ->
  {ok, State, LoopData}.


% gen_fsm States

-spec waiting_for_players({join, pid()}, {pid(), reference()}, map()) ->
  {next_state, waiting_for_players | play, map()}.
waiting_for_players({join, Pid}, From = {Pid, _Ref}, S = #{next_player := nil}) ->
  {next_state, waiting_for_players, S#{next_player => Pid, next_player_from => From}};
waiting_for_players({join, Pid}, {Pid, _Ref}, S = #{next_player := Pid}) ->
  {next_state, waiting_for_players, S};
waiting_for_players(
  {join, SecondPlayerPid}, 
  OtherPlayerFrom = {SecondPlayerPid, _Tag}, 
  #{
    next_player := NextPlayerPid, 
    other_player := nil,
    next_player_from := NextPlayerFrom,
    board := Board
  }) when NextPlayerPid =/= SecondPlayerPid ->
    ?MODULE:reply(NextPlayerFrom, ok),
    ?MODULE:reply(OtherPlayerFrom, ok),
    ?MODULE:ticker(),
    LoopData = #{next_player => NextPlayerPid, other_player => SecondPlayerPid, board => Board},
    {next_state, play, LoopData}.

-spec play(tick, map()) -> {next_state, play | over, map()}.
play(tick, S = #{next_player := PlayerOne, other_player := PlayerTwo, board := Board}) ->
  UpdatedBoard = player:move(PlayerOne, Board),
  case board:check(UpdatedBoard) of
    open ->
      ?MODULE:ticker(),
      {next_state, play, S#{next_player => PlayerTwo, other_player => PlayerOne, board => UpdatedBoard}};
    {win, _} ->
      player:winner(PlayerOne),
      player:loser(PlayerTwo),
      {next_state, over, S#{board => UpdatedBoard}};
    tie ->
      player:draw(PlayerOne),
      player:draw(PlayerTwo),
      {next_state, over, S#{board => UpdatedBoard}}
  end.

% TODO: what should we do here?
over(_Msg, _LoopData) ->
  ok.


% Private

reply(Pid, Msg) ->
  gen_fsm:reply(Pid, Msg).

ticker() ->
  erlang:send_after(?TICK_TIME, self(), ?TICK_MESSAGE).
