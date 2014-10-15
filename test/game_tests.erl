-module(game_tests).
-include_lib("eunit/include/eunit.hrl").

-define(EMPTY_BOARD, board:create()).

%%% INITIALIZATION
state_is_correct_right_after_the_initialization_test() ->
  ExpectedState = #{
    next_player => nil, 
    other_player => nil, 
    board => ?EMPTY_BOARD
  },
  ?assertMatch(
     {ok, waiting_for_players, ExpectedState},
     game:init(something_we_do_not_care)
  ).

%%% WAITING_FOR_PLAYERS
when_in_waiting_for_players_a_player_can_join_test() ->
    From = {Pid = self(), erlang:make_ref()},
    ExpectedLoopData = #{
      next_player => Pid, 
      other_player => nil,
      board => ?EMPTY_BOARD,
      next_player_from => From
    },
    ?assertMatch(
       {next_state, waiting_for_players, ExpectedLoopData},
       game:waiting_for_players(
         {join, Pid}, 
         From, 
         #{
           next_player => nil, 
           other_player => nil, 
           board => ?EMPTY_BOARD,
           next_player_from => From
         }
       )
    ).

when_in_waiting_for_players_and_a_second_player_join_then_fsm_go_to_play_state_test() ->
    NextPlayerFrom = {Pid = 'a-pid', erlang:make_ref()},
    SecondPlayerFrom = {Pid2 = 'a-second-pid', erlang:make_ref()},
    meck:new(game, [passthrough]),
    meck:expect(game, reply, fun(_, _) -> ok end),
    meck:expect(game, ticker, fun() -> ok end),
    ExpectedLoopData = #{
      next_player => Pid, 
      other_player => Pid2,
      board => ?EMPTY_BOARD
    },
    ?assertMatch(
       {next_state, play, ExpectedLoopData},
       game:waiting_for_players(
         {join, Pid2}, 
         SecondPlayerFrom, 
         #{
           next_player => Pid, 
           other_player => nil,
           next_player_from => NextPlayerFrom,
           board => ?EMPTY_BOARD
         }
       )
    ),
    ?assertEqual(2, meck:num_calls(game, reply, '_')),
    ?assertEqual(1, meck:num_calls(game, ticker, '_')),
    meck:unload().

when_in_waiting_for_players_the_same_player_cannot_join_twice_test() ->
    From = {Pid = self(), erlang:make_ref()},
    ExpectedLoopData = #{
      next_player => Pid, 
      other_player => nil,
      board => ?EMPTY_BOARD,
      next_player_from => From
    },
    ?assertMatch(
       {next_state, waiting_for_players, ExpectedLoopData},
       game:waiting_for_players(
         {join, Pid}, 
         From, 
         #{
           next_player => Pid, 
           other_player => nil,
           board => ?EMPTY_BOARD,
           next_player_from => From
         }
       )
    ).

%%% PLAY
when_in_play_tick_ask_next_player_to_move_test() ->
    meck:new(player, [non_strict]),
    meck:new(game, [passthrough]),
    meck:new(board),
    meck:expect(player, move, fun(player_one, board_before_move) -> board_after_move end),
    meck:expect(board, check, fun(board_after_move) -> open end),
    meck:expect(game, ticker, fun() -> ok end),
    CurrentLoopData = #{
      next_player => player_one, 
      other_player => player_two,
      board => board_before_move
    },
    ExpectedLoopData = #{
      next_player => player_two, 
      other_player => player_one,
      board => board_after_move
    },
    ?assertMatch(
       {next_state, play, ExpectedLoopData},
       game:play(tick, CurrentLoopData)
    ),
    ?assertEqual(1, meck:num_calls(player, move, [player_one, board_before_move])),
    ?assertEqual(1, meck:num_calls(game, ticker, [])),
    meck:unload().

when_in_play_and_a_player_wins_the_game_test() ->
    meck:new(player, [non_strict]),
    meck:new(board),
    meck:expect(player, move, fun(player_one, board_before_move) -> board_after_move end),
    meck:expect(player, winner, fun(player_one) -> ok end),
    meck:expect(player, loser, fun(player_two) -> ok end),
    meck:expect(board, check, fun(board_after_move) -> {win, player_one} end),
    CurrentLoopData = #{
      next_player => player_one, 
      other_player => player_two,
      board => board_before_move
    },
    ExpectedLoopData = #{
      next_player => player_one, 
      other_player => player_two,
      board => board_after_move
    },
    ?assertMatch(
       {next_state, over, ExpectedLoopData},
       game:play(tick, CurrentLoopData)
    ),
    ?assertEqual(1, meck:num_calls(player, move, [player_one, board_before_move])),
    ?assertEqual(1, meck:num_calls(player, winner, [player_one])),
    ?assertEqual(1, meck:num_calls(player, loser, [player_two])),
    meck:unload().

when_in_play_and_the_game_is_tied_test() ->
    meck:new(player, [non_strict]),
    meck:new(board),
    meck:expect(player, move, fun(player_one, board_before_move) -> board_after_move end),
    meck:expect(player, draw, fun(_) -> ok end),
    meck:expect(board, check, fun(board_after_move) -> tie end),
    CurrentLoopData = #{
      next_player => player_one, 
      other_player => player_two,
      board => board_before_move
    },
    ExpectedLoopData = #{
      next_player => player_one, 
      other_player => player_two,
      board => board_after_move
    },
    ?assertMatch(
       {next_state, over, ExpectedLoopData},
       game:play(tick, CurrentLoopData)
    ),
    ?assertEqual(1, meck:num_calls(player, move, [player_one, board_before_move])),
    ?assertEqual(1, meck:num_calls(player, draw, [player_one])),
    ?assertEqual(1, meck:num_calls(player, draw, [player_two])),
    meck:unload().
