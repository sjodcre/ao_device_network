-module(device_registry).
-export([start/0, lookup/1]).

-define(TABLE, device_registry).

start() ->
    % Create ETS table if not exists
    case ets:info(?TABLE) of
        undefined ->
            Table = ets:new(?TABLE, [named_table, public, set]),
            load_devices([device_logger, device_math]),
            {ok, Table};
        _ -> ok
    end.

lookup(Key) when is_binary(Key) ->
    case ets:lookup(?TABLE, Key) of
        [{Key, Module}] -> {ok, Module};
        [] -> error
    end.

load_devices(Modules) ->
    lists:foreach(fun register_device/1, Modules).

register_device(Module) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            Keys = Module:keys(),
            [ets:insert(?TABLE, {Key, Module}) || Key <- Keys],
            io:format("[device_registry] ~p supports ~p~n", [Module, Keys]);
        _ ->
            io:format("[device_registry] Failed to load ~p~n", [Module])
    end.
