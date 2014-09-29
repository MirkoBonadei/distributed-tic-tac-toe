-module(game).
-behaviour(gen_fsm).

-export([start_link/0, init/0]).
-export([stop/0, terminate/3]).
-export([waiting_for_players/3, play/2, over/2, handle_info/3]).
-export([reply/2, ticker/0]).

-define(TICK_TIME, 2000).
-define(TICK_MESSAGE, "tick").

reply(Pid, Msg) ->
  gen_fsm:reply(Pid, Msg).

ticker() ->
  erlang:send_after(?TICK_TIME, self(), ?TICK_MESSAGE).

start_link() ->
  gen_fsm:start_link(?MODULE, [], []).

%% API
join(GamePid, PlayerPid) ->
  gen_fsm:sync_send_event(GamePid, {join, PlayerPid}).

init() ->
  {ok, waiting_for_players, []}.

stop() ->
  ok.

terminate(_Reason, _State, _LoopData) ->
  ok.

waiting_for_players({join, Pid}, Pid, []) ->
  {next_state, waiting_for_players, [Pid]};
waiting_for_players({join, Pid}, Pid, [Pid]) ->
  {next_state, waiting_for_players, [Pid]};
waiting_for_players({join, Pid2}, Pid2, [Pid]) ->
  ?MODULE:reply(Pid, ok),
  ?MODULE:reply(Pid2, ok),
  TickerPid = ?MODULE:ticker(),
  %% TODO: should I keep the ticket pid?
  {next_state, play, [Pid, Pid2]}.

play(tick, [PlayerOne, PlayerTwo]) ->
  player:move(PlayerOne),
  {next_state, play, [PlayerTwo, PlayerOne]}.

over(_Msg, _LoopData) ->
  ok.

handle_info(_Info, _State, _LoopData) ->
  ok.


% ===========================================================
% -module(game).
% -export([create/0, destroy/1, join/2]).
% -export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2]).

% -behaviour(gen_server).

% -define(TICK_TIME, 2000).
% -define(TICK_MESSAGE, "tick").

% -define(WAITING_FOR_PLAYERS, "waiting_for_players").
% -define(ONGOING, "ongoing").
% -define(TERMINATED, "terminated").

%%% Game API

% create() ->
%   gen_server:start_link(?MODULE, [], []).

% destroy(GamePid) ->
%   gen_server:cast(GamePid, stop).

% join(GamePid, PlayerPid) ->
%   gen_server:call(GamePid, {join, PlayerPid}).

%%% gen_server callbacks

% init([]) ->
%   {ok, [?WAITING_FOR_PLAYERS, [], board:new()]}.

% handle_cast(stop, State) ->
%   {stop, normal, State}.

% handle_call({join, PlayerPid}, _From, [?WAITING_FOR_PLAYERS, [], Board]) ->
%   {reply, {ok, welcome}, [?WAITING_FOR_PLAYERS, [PlayerPid], Board]};
% handle_call({join, Pid}, _From, [State, [Pid], Board]) ->
%   {reply, {error, already_joined}, [State, [Pid], Board]};
% handle_call({join, PlayerPid}, _From, [?WAITING_FOR_PLAYERS, [PlayerOne], Board]) ->
%   Timer = erlang:send_after(?TICK_TIME, self(), ?TICK_MESSAGE),
%   {reply, {ok, welcome}, [?ONGOING, [PlayerOne, PlayerPid], Board, Timer]};
% handle_call({join, Pid}, _From, [State, Players = [Pid, _], Board, Timer]) ->
%   {reply, {error, already_joined}, [State, Players, Board, Timer]};
% handle_call({join, Pid}, _From, [State, Players = [_, Pid], Board, Timer]) ->
%   {reply, {error, already_joined}, [State, Players, Board, Timer]};
% handle_call({join, _Pid}, _From, [State, Players, Board, Timer]) when length(Players) =:= 2 ->
%   {reply, {error, full}, [State, Players, Board, Timer]}.

% handle_info(?TICK_MESSAGE, [?ONGOING, [NextPlayer, PausedPlayer], Board, OldTimer]) ->
%   erlang:cancel_timer(OldTimer),
%   {X, Y} = player:next_move(NextPlayer, Board),
%   NewBoard = board:make_move(Board, NextPlayer, X, Y),
%   io:format("~p~n", [NewBoard]),
%   case board:has_been_won_by(NewBoard, NextPlayer) of
%     true ->
%       player:winner(NextPlayer),
%       player:loser(PausedPlayer),
%       {stop, normal, [?TERMINATED, [PausedPlayer, NextPlayer], NewBoard]};
%     false ->
%       case board:available_positions(NewBoard) of
%         [] ->
%           player:draw(NextPlayer),
%           player:draw(PausedPlayer),
%           {stop, normal, [?TERMINATED, [PausedPlayer, NextPlayer], NewBoard]};
%         _ ->
%           NewTimer = erlang:send_after(?TICK_TIME, self(), ?TICK_MESSAGE),
%           {noreply, [?ONGOING, [PausedPlayer, NextPlayer], NewBoard, NewTimer]}
%       end
%   end.

% terminate(_Reason, _State) ->
%   ok.
