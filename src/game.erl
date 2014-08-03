-module(game).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/testing_macros.hrl").

-export([create/0, destroy/1, join/2]).
-export([
         init/1,
         handle_cast/2,
         handle_call/3,
         terminate/2
        ]).
-behaviour(gen_server).

-define(TEST_TIMEOUT, 1000).

%%% Game API

create() ->
  gen_server:start_link(?MODULE, [], []).

destroy(GamePid) ->
  gen_server:cast(GamePid, stop).

join(GamePid, PlayerPid) ->
  gen_server:call(GamePid, {join, PlayerPid}).

%%% gen_server callbacks

init([]) ->
  {ok, [[], board:new()]}.

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_call({join, PlayerPid}, _From, [[], Board]) ->
  {reply, {ok, welcome}, [[PlayerPid], Board]}.

terminate(normal, State) ->
  io:format(
    "game has been destroyed with state ~p~n",
    [State]
  );
terminate(Reason, State) ->
  io:format(
    "game has been terminated with reason ~p and with state~p~n",
    [Reason, State]
  ).

-ifdef(TEST).

game_should_be_created_and_destroyed_test() ->
  {ok, GamePid} = game:create(),
  ?assert(erlang:is_process_alive(GamePid)),
  ?assertProcessDownAfter(
     GamePid,
     ?TEST_TIMEOUT,
     fun() -> game:destroy(GamePid) end
  ).

game_should_accept_join_request_from_player_test_() ->
  {setup,
   fun() -> {ok, GamePid} = game:create(), GamePid end,
   fun(GamePid) -> game:destroy(GamePid) end,
   fun(GamePid) ->
       [?_assertMatch({ok, welcome}, game:join(GamePid, self()))]
   end
  }.

-endif.
