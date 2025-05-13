-module(device_logger).
-export([keys/0, call/2]).

keys() -> [<<"log">>].

call(<<"log">>, Data) ->
    io:format("[device_logger] Log: ~p~n", [Data]),
    {ok, <<"logged">>};

call(<<"ping">>, Data) ->
    io:format("[device_logger] Ping: ~p~n", [Data]),
    {ok, <<"pong">>};

call(_, _) ->
    {error, unknown_key}.
