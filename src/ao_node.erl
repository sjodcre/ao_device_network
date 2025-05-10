-module(ao_node).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ao_node}, ?MODULE, [], []).

init([]) ->
    io:format("~n[ao_node] Ready to receive messages~n"),
    {ok, #{}}.

handle_info(Msg, State) ->
    io:format("[ao_node] Received: ~p~n", [Msg]),
    {noreply, State}.

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.
