-module(player).
-include_lib("eunit/include/eunit.hrl").

-export([create/1, destroy/1]).
-export([
         init/1,
         handle_cast/2,
         terminate/2
        ]).
-behaviour(gen_server).

-define(TEST_TIMEOUT, 1000).

%%% Player API

create(Name) ->
  gen_server:start_link(?MODULE, [Name], []).

destroy(PlayerPid) ->
  gen_server:cast(PlayerPid, stop).

%%% gen_server callbacks

init([Name]) ->
  {ok, Name}.

handle_cast(stop, Name) ->
  {stop, normal, Name}.

terminate(normal, Name) ->
  io:format(
    "player ~p has been destroyed~n",
    [Name]
  );
terminate(Reason, Name) ->
  io:format(
    "player ~p has been terminated with reason ~p~n",
    [Name, Reason]
  ).

-ifdef(TEST).

player_should_be_created_and_destroied_test() ->
  {ok, PlayerPid} = player:create("darth-vader"),
  ?assert(erlang:is_process_alive(PlayerPid)),

  Ref = erlang:monitor(process, PlayerPid),
  player:destroy(PlayerPid),
  receive
    {'DOWN', Ref, process, PlayerPid, normal} ->
      ?assert(true)
  after
    ?TEST_TIMEOUT ->
      ?assert(false)
  end.


-endif.
