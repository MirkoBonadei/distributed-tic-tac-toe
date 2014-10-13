-module(game).
-behaviour(gen_fsm).

-export([join/2, stop/0, start_link/0]).
-export([init/1, terminate/3, handle_info/3, handle_event/3, handle_sync_event/4, code_change/4]).
-export([waiting_for_players/3, play/2, over/2]).
-export([reply/2, ticker/0]).

-define(TICK_TIME, 2000).
-define(TICK_MESSAGE, "tick").

% Public

start_link() ->
  gen_fsm:start_link(
    ?MODULE,
    #{next_player => nil, other_player => nil, board => nil},
    []
  ).

join(GamePid, PlayerPid) ->
  gen_fsm:sync_send_event(GamePid, {join, PlayerPid}).

stop() ->
  ok.

% gen_fsm Callbacks

init(LoopData) ->
  {ok, waiting_for_players, LoopData}.

terminate(_Reason, _State, _LoopData) ->
  ok.

handle_info(_Info, State, LoopData) ->
  {next_state, State, LoopData}.

handle_event(_Event, State, LoopData) ->
  {next_state, State, LoopData}.

handle_sync_event(_Event, _From, State, LoopData) ->
  {next_state, State, LoopData}.

code_change(_OldVsn, State, LoopData, _Extra) ->
  {ok, State, LoopData}.


% gen_fsm States

waiting_for_players({join, Pid}, Pid, S = #{next_player := nil}) ->
  {next_state, waiting_for_players, S#{next_player => Pid}};
waiting_for_players({join, Pid}, Pid, S = #{next_player := Pid}) ->
  {next_state, waiting_for_players, S};
waiting_for_players({join, Pid2}, Pid2, S= #{next_player := Pid, other_player := nil}) ->
  ?MODULE:reply(Pid, ok),
  ?MODULE:reply(Pid2, ok),
  ?MODULE:ticker(),
  {next_state, play, S#{other_player => Pid2}}.

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

over(_Msg, _LoopData) ->
  ok.


% Private

reply(Pid, Msg) ->
  gen_fsm:reply(Pid, Msg).

ticker() ->
  erlang:send_after(?TICK_TIME, self(), ?TICK_MESSAGE).
