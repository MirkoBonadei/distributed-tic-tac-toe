-module(player).
-export([create/1, destroy/1, join/2, next_move/2,
         winner/1, loser/1, draw/1]).
-export([init/1, handle_cast/2, handle_call/3, terminate/2]).
-behaviour(gen_server).

%%% Player API

create(Name) ->
    gen_server:start_link(?MODULE, [Name], []).

-spec destroy(pid()) -> ok.
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
    AvailableMoves = board:available_moves(Board),
    {reply, random_move(AvailableMoves), Name}.

terminate(_Reason, _Name) ->
    ok.

%%% private functions

random_move(AvailableMoves) ->
    <<A1, A2, A3>> = crypto:strong_rand_bytes(3),
    random:seed(A1, A2, A3),
    lists:nth(random:uniform(length(AvailableMoves)), AvailableMoves).
