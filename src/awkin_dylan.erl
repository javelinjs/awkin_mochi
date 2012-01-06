-module(awkin_dylan).
-compile(export_all).

send_cmd(Host, Port, Cmd) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
    ok = gen_tcp:send(Socket, Cmd ++ "\r\n"),
    Data = receive_data(Socket, [], "{:DYLAN:END:}\n"),
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

cmd_get_item(Auth, User, Item) ->
    Data = {struct, [{<<"user">>, User},
                {<<"item">>, Item}
    ]},
    Cmd = {struct, [{<<"auth">>, Auth},
                {<<"cmd">>, <<"get_item">>},
                {<<"data">>, Data}
    ]},
    mochijson:binary_encode(Cmd).

%TODO authorization
json_auth(Username, Pwd) ->
    {struct, [{<<"user">>, Username}, {<<"pwd">>, Pwd}]}.

% items infomation
% NumOfItem: How many items should got
% BaseId: Items rank should larger than Item[BaseId]'s rank
% ReplyNContent: First of the highest ranked item needs content
json_items(NumOfItem, ReplyNContent, BaseId) ->
    {struct, [{<<"set_size">>, NumOfItem}, 
                {<<"base_id">>, BaseId},
                {<<"reply_n_content">>, ReplyNContent}
            ]}.
json_items(NumOfItem, ReplyNContent) ->
    json_items(NumOfItem, ReplyNContent, <<"">>).
json_items(NumOfItem) ->
    json_items(NumOfItem, <<"-1">>, <<"">>).
json_items() ->
    json_items(<<"-1">>, <<"-1">>, <<"">>).

%user infomation
json_user(UserId) ->
    {struct, [{<<"user_id">>, UserId}]}.

