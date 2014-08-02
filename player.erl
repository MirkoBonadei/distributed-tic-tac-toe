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
-define(assertProcessDownAfter(Pid, Timeout, Callback),
        begin
        ((fun () ->
            Ref = erlang:monitor(process, Pid),
            Callback(),
            receive
              {'DOWN', Ref, process, Pid, normal} ->
                ok
            after
              Timeout ->
                erlang:error({assertion_failed,
                              [{module, ?MODULE},
                               {line, ?LINE},
                               {expression, (??Callback)},
                               {expected, process_terminated},
                               {value, process_is_still_alive}]})
            end
          end)())
        end).

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
  ?assertProcessDownAfter(
     PlayerPid,
     ?TEST_TIMEOUT,
     fun() -> player:destroy(PlayerPid) end
  ).

-endif.
