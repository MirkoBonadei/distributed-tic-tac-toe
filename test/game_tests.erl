-module(game_tests).
-include_lib("eunit/include/eunit.hrl").

%%% WAITING_FOR_PLAYERS
when_in_waiting_for_players_a_player_can_join_test() ->
  Pid = self(),
  ExpectedLoopData = #{next_player => Pid, other_player => nil},
  ?assertMatch(
     {next_state, waiting_for_players, ExpectedLoopData},
     game:waiting_for_players(
       {join, Pid}, 
       Pid, 
       #{next_player => nil, other_player => nil}
     )
  ).

when_in_waiting_for_players_and_a_second_player_join_then_fsm_go_to_play_state_test() ->
  Pid = 'a-pid',
  Pid2 = 'a-second-pid',
  meck:new(game, [passthrough]),
  meck:expect(game, reply, fun(_, _) -> ok end),
  meck:expect(game, ticker, fun() -> ok end),
  ExpectedLoopData = #{next_player => Pid, other_player => Pid2},

  ?assertMatch(
     {next_state, play, ExpectedLoopData},
     game:waiting_for_players(
       {join, Pid2}, 
       Pid2, 
       #{next_player => Pid, other_player => nil}
     )
  ),
  ?assertEqual(2, meck:num_calls(game, reply, '_')),
  ?assertEqual(1, meck:num_calls(game, ticker, '_')),

  meck:unload(game).

when_in_waiting_for_players_the_same_player_cannot_join_twice_test() ->
  Pid = self(),
  ExpectedLoopData = #{next_player => Pid, other_player => nil},
  ?assertMatch(
     {next_state, waiting_for_players, ExpectedLoopData},
     game:waiting_for_players(
       {join, Pid}, 
       Pid, 
       #{next_player => Pid, other_player => nil}
     )
  ).

%%% PLAY
when_in_play_tick_ask_next_player_to_move_test() ->
  meck:new(player, [non_strict]),
  meck:expect(player, move, fun(_) -> ok end),
  CurrentLoopData = #{
    next_player => 'player-1-pid', 
    other_player => 'player-2-pid'
  },
  ExpectedLoopData = #{
    next_player => 'player-2-pid', 
    other_player => 'player-1-pid'
  },

  ?assertMatch(
     {next_state, play, ExpectedLoopData},
     game:play(tick, CurrentLoopData)
  ),
  ?assertEqual(1, meck:num_calls(player, move, ['player-1-pid'])),

  meck:unload(player).
