-module(awkin_dylan).
-compile(export_all).

send_cmd(Host, Port, Cmd) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
    ok = gen_tcp:send(Socket, Cmd ++ "\r\n"),
    Data = receive_data(Socket, []),
    gen_tcp:close(Socket),
    Data.

receive_data(Socket, SoFar) ->
    receive 
        {tcp, Socket, "{:END:}"} ->
            list_to_binary(lists:reverse(SoFar));
        {tcp, Socket, Bin} ->
            END = "{:END:}\n",
            ENDLength = string:len(END),
            RevBin = lists:reverse(binary_to_list(Bin)),
            Found = string:rstr(RevBin, lists:reverse(END)),
            if 
            Found > 0 ->
                Res = lists:reverse(lists:nthtail(ENDLength, RevBin)),
                BinRes = list_to_binary(Res),
                list_to_binary(lists:reverse([BinRes|SoFar]));
            true ->
                receive_data(Socket, [Bin|SoFar])
            end;
        {tcp_closed, Socket} ->
            list_to_binary(lists:reverse(SoFar))
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
    json_items(NumOfItem, ReplyNContent, <<"-1">>).
json_items(NumOfItem) ->
    json_items(NumOfItem, <<"-1">>, <<"-1">>).
json_items() ->
    json_items(<<"-1">>, <<"-1">>, <<"-1">>).

%user infomation
json_user(UserId) ->
    {struct, [{<<"user_id">>, UserId}]}.

