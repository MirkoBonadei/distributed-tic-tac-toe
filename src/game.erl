-module(game).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/testing_macros.hrl").

-export([create/1, destroy/1]).
-export([
         init/1,
         handle_cast/2,
         terminate/2
        ]).
-behaviour(gen_server).

-define(TEST_TIMEOUT, 1000).

%%% Game API

create(Name) ->
  gen_server:start_link(?MODULE, [Name], []).

destroy(GamePid) ->
  gen_server:cast(GamePid, stop).

%%% gen_server callbacks

init([Name]) ->
  {ok, Name}.

handle_cast(stop, Name) ->
  {stop, normal, Name}.

terminate(normal, Name) ->
  io:format(
    "game ~p has been destroyed~n",
    [Name]
  );
terminate(Reason, Name) ->
  io:format(
    "game ~p has been terminated with reason ~p~n",
    [Name, Reason]
  ).

-ifdef(TEST).

game_should_be_created_and_destroyed_test() ->
  {ok, GamePid} = game:create("star-wars"),
  ?assert(erlang:is_process_alive(GamePid)),
  ?assertProcessDownAfter(
     GamePid,
     ?TEST_TIMEOUT,
     fun() -> game:destroy(GamePid) end
  ).

-endif.
