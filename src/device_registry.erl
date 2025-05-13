-module(device_registry).
-export([start/0, lookup/1, register/2, unregister/1, list/0]).

-define(TABLE, device_registry).

start() ->
    % Create ETS table if not exists
    case ets:info(?TABLE) of
        undefined ->
            Table = ets:new(?TABLE, [named_table, public, set]),
            % load_devices([device_logger, device_math]),
            % load_devices([device_math]),
            {ok, Table};
        _ -> ok
    end.

lookup(Key) when is_binary(Key) ->
    case ets:lookup(?TABLE, Key) of
        [{Key, Module}] -> {ok, Module};
        [] -> error
    end.

register(Key, Module) when is_binary(Key), is_atom(Module) ->
    ets:insert(?TABLE, {Key, Module}),
    io:format("[device_registry] Registered ~p => ~p~n", [Key, Module]),
    ok.

unregister(Key) when is_binary(Key) ->
    ets:delete(?TABLE, Key),
    io:format("[device_registry] Unregistered ~p~n", [Key]),
    ok.

list() ->
    ets:tab2list(?TABLE).

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
