-module(game_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("include/testing_macros.hrl").

-define(TEST_TIMEOUT, 1000).

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
