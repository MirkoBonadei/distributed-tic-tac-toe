-module(player_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("include/testing_macros.hrl").

-define(TEST_TIMEOUT, 1000).

player_should_be_created_and_destroyed_test() ->
  {ok, PlayerPid} = player:create("darth-vader"),
  ?assert(erlang:is_process_alive(PlayerPid)),
  ?assertProcessDownAfter(
     PlayerPid,
     ?TEST_TIMEOUT,
     fun() -> player:destroy(PlayerPid) end
  ).

player_should_ask_game_to_join_test_() ->
  {setup,
   fun() ->
       {ok, PlayerPid} = player:create("luke"),
       ok = meck:new(game, [passthrough]),
       {ok, GamePid} = game:create(),
       [GamePid, PlayerPid]
    end,
   fun([GamePid, PlayerPid]) ->
       ok = meck:unload(game),
       game:destroy(GamePid),
       player:destroy(PlayerPid)
   end,
   fun([GamePid, PlayerPid]) ->
      meck:expect(game, join, fun(P1, P2) when is_pid(P1) and is_pid(P2) -> {ok, welcome} end),
      player:join(PlayerPid, GamePid),
      [
          ?_assert(meck:called(game, join, [GamePid, PlayerPid])),
          ?_assert(meck:validate(game))
      ]
   end
  }.
