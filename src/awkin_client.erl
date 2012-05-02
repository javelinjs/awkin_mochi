-module(awkin_client).
-compile(export_all).

send_cmd_nonblocking(Host, Port, Cmd) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
    gen_tcp:send(Socket, Cmd ++ "\r\n").
send_cmd(Host, Port, Cmd, EndSignal) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
    ok = gen_tcp:send(Socket, Cmd ++ "\r\n"),
    Data = receive_data(Socket, [], EndSignal),
    gen_tcp:close(Socket),
    Data.

receive_data(Socket, SoFar, END) ->
    receive 
        {tcp, Socket, Bin} ->
            BinList = binary_to_list(Bin),
            Found = string:rstr(BinList, END),
            if 
            Found > 0 ->
                case lists:split(Found-1, BinList) of
                    {Res, END} -> 
                        BinRes = list_to_binary(Res),
                        list_to_binary(lists:reverse([BinRes|SoFar]));
                    _ -> 
                        receive_data(Socket, [Bin|SoFar], END)
                end;
            true ->
                receive_data(Socket, [Bin|SoFar], END)
            end;
        {tcp_closed, Socket} ->
            []
    after 4000 ->
        []
    end.

%TODO authorization
json_auth(Username, Pwd) ->
    {struct, [{<<"user">>, Username}, {<<"pwd">>, Pwd}]}.

