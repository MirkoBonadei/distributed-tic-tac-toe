-module(player_tests).
-include_lib("eunit/include/eunit.hrl").

when_player_is_asked_to_join_the_request_is_forwarded_to_the_game_test() ->
  meck:new(game),
  meck:expect(game, join, fun(_, _) -> ok end),
  GamePid = generate_pid(),
  Result = player:handle_call(
    {join, GamePid}, 
    {self(), erlang:make_ref()},
    {'a-name', x}
  ),
  ?assertEqual(1, meck:num_calls(game, join, '_')),
  ?assertMatch(
    {reply, ok, {'a-name', x}},
    Result
  ),
  meck:unload().

%% Private
generate_pid() ->
  Pid = spawn(fun() -> ok end),
  Pid.
