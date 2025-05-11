-module(device_logger).
-export([call/2]).

call(<<"log">>, Data) ->
    io:format("[device_logger] Log: ~p~n", [Data]),
    {ok, <<"logged">>};

call(_, _) ->
    {error, unknown_key}.
