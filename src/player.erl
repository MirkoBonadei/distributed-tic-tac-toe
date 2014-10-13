-module(player).
-export([start_link/2, stop/1, join/2, move/2, winner/1, loser/1, draw/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-behaviour(gen_server).

% Public

-spec start_link(string(), board:symbol()) -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link(Name, Symbol) ->
  gen_server:start_link(?MODULE, {Name, Symbol}, []).

-spec stop(pid()) -> ok.
stop(PlayerPid) ->
  gen_server:cast(PlayerPid, stop).

-spec join(pid(), pid()) -> ok.
join(PlayerPid, GamePid) ->
  game:join(GamePid, PlayerPid).

-spec move(pid(), board:board()) -> board:board().
move(PlayerPid, Board) ->
  gen_server:call(PlayerPid, {next_move, Board}).

-spec winner(pid()) -> ok.
winner(PlayerPid) ->
  gen_server:cast(PlayerPid, winner).

-spec loser(pid()) -> ok.
loser(PlayerPid) ->
  gen_server:cast(PlayerPid, loser).

-spec draw(pid()) -> ok.
draw(PlayerPid) ->
  gen_server:cast(PlayerPid, draw).


% gen_server Callbacks

-spec init({string(), board:symbol()}) -> {ok, {string(), board:symbol()}}.
init({Name, Symbol}) ->
  {ok, {Name, Symbol}}.

-spec handle_cast(stop, {string(), board:symbol()}) -> {stop, normal, {string(), board:symbol()}};
                 (winner, {string(), board:symbol()}) -> {noreply, {string(), board:symbol()}};
                 (loser, {string(), board:symbol()}) -> {noreply, {string(), board:symbol()}};
                 (draw, {string(), board:symbol()}) -> {noreply, {string(), board:symbol()}}.
handle_cast(stop, {Name, Symbol}) ->
  {stop, normal, {Name, Symbol}};
handle_cast(winner, {Name, Symbol}) ->
  io:format("~p: I am the winner~n", [self()]),
  {noreply, {Name, Symbol}};
handle_cast(loser, {Name, Symbol}) ->
  io:format("~p: I am the loser~n", [self()]),
  {noreply, {Name, Symbol}};
handle_cast(draw, {Name, Symbol}) ->
  io:format("~p: I am not a winner and not a loser~n", [self()]),
  {noreply, {Name, Symbol}}.

-spec handle_call({next_move, board:board()}, {pid(), reference()}, {string(), board:symbol()}) ->
  {reply, board:board(), {string(), board:symbol()}}.
handle_call({next_move, Board}, _From, {Name, Symbol}) ->
  {X,Y} = random_move(board:available_moves(Board)),
  BoardAfterMove = board:move(Board, {X, Y, Symbol}),
  {reply, BoardAfterMove, {Name, Symbol}}.

-spec handle_info(info | term(), {string(), board:symbol()}) ->
  {noreply, {string(), board:symbol()}}.
handle_info(_Info, State) ->
  {noreply, State}.

-spec code_change(term() | {down, term()}, {string(), board:symbol()}, term()) ->
  {ok, {string(), board:symbol()}}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

-spec terminate(normal | shutdown | {shutdown, term()} | term(), {string(), board:symbol()}) -> ok.
terminate(_Reason, _State) ->
  ok.


% Private

-spec random_move([board:position(), ...]) -> board:position().
random_move(AvailableMoves) ->
  <<A1, A2, A3>> = crypto:strong_rand_bytes(3),
  random:seed(A1, A2, A3),
  lists:nth(random:uniform(length(AvailableMoves)), AvailableMoves).
