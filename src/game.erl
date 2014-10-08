-module(game).
-behaviour(gen_fsm).

-export([start_link/0, init/0]).
-export([stop/0, terminate/3]).
-export([waiting_for_players/3, play/2, over/2, handle_info/3]).
-export([reply/2, ticker/0]).
-export([code_change/4]).

-define(TICK_TIME, 2000).
-define(TICK_MESSAGE, "tick").

reply(Pid, Msg) ->
    gen_fsm:reply(Pid, Msg).

ticker() ->
    erlang:send_after(?TICK_TIME, self(), ?TICK_MESSAGE).

start_link() ->
    gen_fsm:start_link(
      ?MODULE, 
      [], 
      #{next_player => nil, other_player => nil, board => nil}
    ).

%% API
join(GamePid, PlayerPid) ->
    gen_fsm:sync_send_event(GamePid, {join, PlayerPid}).

init() ->
    {ok, waiting_for_players, []}.

stop() ->
    ok.

terminate(_Reason, _State, _LoopData) ->
    ok.

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

handle_info(_Info, _State, _LoopData) ->
    ok.

code_change(_, _, _, _) ->
    ok.
