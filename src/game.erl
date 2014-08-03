-module(game).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/testing_macros.hrl").

-export([create/0, destroy/1, join/2]).
-export([
         init/1,
         handle_cast/2,
         handle_call/3,
         handle_info/2,
         terminate/2
        ]).
-behaviour(gen_server).

-define(TEST_TIMEOUT, 1000).

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
handle_call({join, PlayerPid}, _From, [?WAITING_FOR_PLAYERS, [PlayerOne], Board]) ->
  Timer = erlang:send_after(?TICK_TIME, self(), ?TICK_MESSAGE),
  {reply, {ok, welcome}, [?ONGOING, [PlayerOne, PlayerPid], Board, Timer]}.

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

-ifdef(TEST).

game_should_be_created_and_destroyed_test() ->
  {ok, GamePid} = game:create(),
  ?assert(erlang:is_process_alive(GamePid)),
  ?assertProcessDownAfter(
     GamePid,
     ?TEST_TIMEOUT,
     fun() -> game:destroy(GamePid) end
  ).

game_should_accept_join_request_from_players_test_() ->
  {setup,
   fun() ->
       {ok, Game} = game:create(),
       {ok, PlayerOne} = player:create("player one"),
       {ok, PlayerTwo} = player:create("player two"),
       [Game, PlayerOne, PlayerTwo]
   end,
   fun([Game, PlayerOne, PlayerTwo]) ->
       game:destroy(Game),
       player:destroy(PlayerOne),
       player:destroy(PlayerTwo)
   end,
   fun([Game, PlayerOne, PlayerTwo]) ->
       [?_assertMatch({ok, welcome}, game:join(Game, PlayerOne)),
        ?_assertMatch({ok, welcome}, game:join(Game, PlayerTwo))]
   end
  }.

-endif.
