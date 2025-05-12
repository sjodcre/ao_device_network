-module(ao_node).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2, code_change/3, send/2]).

start_link() ->
    code:ensure_loaded(device_logger),
    code:ensure_loaded(device_math),
    gen_server:start_link({local, ao_node}, ?MODULE, [], []).

init([]) ->
    io:format("~n[ao_node] Ready to receive messages~n"),
    {ok, #{}}.

handle_info(#{from := From, to := ToBin, key := Key, data := Data} = Msg, State) ->
    io:format("[ao_node] Routing ~p~n", [Msg]),
    case parse_device(ToBin) of
        {ok, Module} ->
            % Result = try Module:call(Key, Data)
            %          catch _:Reason -> {error, Reason}
            %          end,
            Result = try apply(Module, call, [Key, Data])
                catch _:Reason -> {error, Reason}
                end,
            From ! {device_response, Result};
        error ->
            From ! {device_response, {error, unknown_device}}
    end,
    {noreply, State};

handle_info(Msg, State) ->
    io:format("[ao_node] Invalid message: ~p~n", [Msg]),
    {noreply, State}.

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.

send(NodeShort, Text) ->
    Node = resolve_full_node(NodeShort),
    Self = self(),
    % Msg = #{
    %     from => Self,
    %     to => <<"device_logger@1.0">>,
    %     key => <<"log">>,
    %     data => unicode:characters_to_binary(Text)
    % },
    [CmdBin | Rest] = binary:split(unicode:characters_to_binary(Text), <<" ">>, [global]),
    Msg = #{
        from => Self,
        to => device_for(CmdBin),
        key => CmdBin,
        data => unicode:characters_to_binary(string:trim(string:join([binary_to_list(S) || S <- Rest], " ")))
    },
    {ao_node, Node} ! Msg,
    receive
        {device_response, Result} ->
            io:format("[ao_node] Reply: ~p~n", [Result]),
            Result
    after 3000 ->
        io:format("[ao_node] Timeout~n"),
        timeout
    end.

device_for(<<"add">>) -> <<"device_math@1.0">>;
device_for(<<"mul">>) -> <<"device_math@1.0">>;
device_for(_)         -> <<"device_logger@1.0">>.

resolve_full_node(Short) ->
    [_, Host] = string:split(atom_to_list(node()), "@"),
    list_to_atom(atom_to_list(Short) ++ "@" ++ Host).

parse_device(Bin) when is_binary(Bin) ->
    case binary:split(Bin, <<"@">>, [global]) of
        [ModBin, _Ver] ->
            try list_to_existing_atom(binary_to_list(ModBin)) of
                Mod -> {ok, Mod}
            catch _:_ -> error
            end;
        _ -> error
    end.