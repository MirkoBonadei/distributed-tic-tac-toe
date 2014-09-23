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

player_should_be_able_to_join_only_once_test_() ->
  {setup,
   fun() ->
       {ok, Game} = game:create(),
       {ok, PlayerOne} = player:create("player one"),
       [Game, PlayerOne]
   end,
   fun([Game, PlayerOne]) ->
       game:destroy(Game),
       player:destroy(PlayerOne)
   end,
   fun([Game, PlayerOne]) ->
       [?_assertMatch({ok, welcome}, game:join(Game, PlayerOne)),
        ?_assertMatch({error, already_joined}, game:join(Game, PlayerOne))]
   end
  }.

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

player_who_has_already_joined_could_not_re_join_when_the_game_is_ongoing_test_() ->
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
        ?_assertMatch({ok, welcome}, game:join(Game, PlayerTwo)),
        ?_assertMatch({error, already_joined}, game:join(Game, PlayerOne))]
   end
  }.

game_should_accept_two_players_max_test_() ->
  {setup,
   fun() ->
       {ok, Game} = game:create(),
       {ok, PlayerOne} = player:create("player one"),
       {ok, PlayerTwo} = player:create("player two"),
       {ok, PlayerThree} = player:create("player two"),
       [Game, PlayerOne, PlayerTwo, PlayerThree]
   end,
   fun([Game, PlayerOne, PlayerTwo, PlayerThree]) ->
       game:destroy(Game),
       player:destroy(PlayerOne),
       player:destroy(PlayerTwo),
       player:destroy(PlayerThree)
   end,
   fun([Game, PlayerOne, PlayerTwo, PlayerThree]) ->
       [?_assertMatch({ok, welcome}, game:join(Game, PlayerOne)),
        ?_assertMatch({ok, welcome}, game:join(Game, PlayerTwo)),
        ?_assertMatch({error, full}, game:join(Game, PlayerThree))]
   end
  }.
