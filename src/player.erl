-module(player).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/testing_macros.hrl").

-export([
         create/1,
         destroy/1,
         join/2,
         next_move/2,
         winner/1,
         loser/1,
         draw/1
        ]).
-export([
         init/1,
         handle_cast/2,
         handle_call/3,
         terminate/2
        ]).
-behaviour(gen_server).

-define(TEST_TIMEOUT, 1000).

%%% Player API

create(Name) ->
  gen_server:start_link(?MODULE, [Name], []).

destroy(PlayerPid) ->
  gen_server:cast(PlayerPid, stop).

join(PlayerPid, GamePid) ->
  game:join(GamePid, PlayerPid).

next_move(PlayerPid, Board) ->
  gen_server:call(PlayerPid, {next_move, Board}).

winner(PlayerPid) ->
  gen_server:cast(PlayerPid, winner).

loser(PlayerPid) ->
  gen_server:cast(PlayerPid, loser).

draw(PlayerPid) ->
  gen_server:cast(PlayerPid, draw).

%%% gen_server callbacks

init([Name]) ->
  {ok, Name}.

handle_cast(stop, Name) ->
  {stop, normal, Name};
handle_cast(winner, Name) ->
  io:format("~p: I am the winner~n", [self()]),
  {noreply, Name};
handle_cast(loser, Name) ->
  io:format("~p: I am the loser~n", [self()]),
  {noreply, Name};
handle_cast(draw, Name) ->
  io:format("~p: I am not a winner and not a loser~n", [self()]),
  {noreply, Name}.

handle_call({next_move, Board}, _From, Name) ->
  AvailablePositions = board:available_positions(Board),
  {reply, random_move(AvailablePositions), Name}.

terminate(_Reason, _Name) ->
  ok.

%%% private functions

random_move(AvailableMoves) ->
  <<A1, A2, A3>> = crypto:strong_rand_bytes(3),
  random:seed(A1, A2, A3),
  lists:nth(random:uniform(length(AvailableMoves)), AvailableMoves).

-ifdef(TEST).

player_should_be_created_and_destroyed_test() ->
  {ok, PlayerPid} = player:create("darth-vader"),
  ?assert(erlang:is_process_alive(PlayerPid)),
  ?assertProcessDownAfter(
     PlayerPid,
     ?TEST_TIMEOUT,
     fun() -> player:destroy(PlayerPid) end
  ).

player_should_ask_game_to_join_test() ->
    {ok, GamePid} = game:create(),
    {ok, PlayerPid} = player:create("luke"),

    ok = meck:new(game, [passthrough]),
    meck:expect(
      game,
      join,
      fun(PidOne, PidTwo) when is_pid(PidOne) and is_pid(PidTwo) ->
          {ok, welcome};
         (_, _) -> error(wrong_parameter_type)
      end
    ),

    player:join(PlayerPid, GamePid),

    ?assert(meck:called(game, join, [GamePid, PlayerPid])),
    ?assert(meck:validate(game)),

    ok = meck:unload(game),
    game:destroy(GamePid),
    player:destroy(PlayerPid).
  % {setup,
  %  fun() ->
  %      {ok, GamePid} = game:create(),
  %      {ok, PlayerPid} = player:create("luke"),
  %      ok = meck:new(game, [passthrough]),
  %      [GamePid, PlayerPid]
  %   end,
  %  fun([GamePid, PlayerPid]) ->
  %      ok = meck:unload(game),
  %      game:destroy(GamePid),
  %      player:destroy(PlayerPid)
  %  end,
  %  fun([GamePid, PlayerPid]) ->
  %      meck:expect(game, join, fun(_, _) -> {ok, welcome} end),
  %      player:join(PlayerPid, GamePid),
  %      ?_assert(meck:called(game, join, [GamePid, PlayerPid])),
  %      ?_assert(meck:validate(game))
  %  end
  % }.

-endif.
