-module(device_math).
-export([keys/0, call/2]).

keys() -> [<<"add">>, <<"mul">>]. %% example for device_math

call(<<"add">>, Bin) ->
    case parse_numbers(Bin) of
        {ok, Ns} -> {ok, lists:sum(Ns)};
        Err -> Err
    end;

call(<<"mul">>, Bin) ->
    case parse_numbers(Bin) of
        {ok, Ns} -> {ok, lists:foldl(fun(X, Acc) -> X * Acc end, 1, Ns)};
        Err -> Err
    end;

call(_, _) ->
    {error, unknown_key}.

parse_numbers(Bin) when is_binary(Bin) ->
    try
        List = binary:split(Bin, <<" ">>, [global]),
        Numbers = [binary_to_integer(S) || S <- List],
        {ok, Numbers}
    catch
        _:_ -> {error, bad_input}
    end.
