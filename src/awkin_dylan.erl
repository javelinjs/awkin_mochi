-module(awkin_dylan).
-compile(export_all).

send_cmd(Host, Port, Cmd) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
    ok = gen_tcp:send(Socket, Cmd ++ "\r\n"),
    receive_data(Socket, []).

receive_data(Socket, SoFar) ->
    receive 
        {tcp, Socket, Bin} ->
            receive_data(Socket, [Bin|SoFar]);
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
    mochijson2:encode(Cmd).

%TODO authorization
json_auth(Username, Pwd) ->
    {struct, [{<<"user">>, Username}, {<<"pwd">>, Pwd}]}.

% items infomation
% NumOfItem: How many items should got
% BaseId: Items rank should larger than Item[BaseId]'s rank
json_items(NumOfItem, BaseId) ->
    {struct, [{<<"set_size">>, NumOfItem}, {<<"base_id">>, BaseId}]}.
json_items(NumOfItem) ->
    json_items(NumOfItem, <<0>>).
json_items() ->
    json_items(<<0>>, <<0>>).

%user infomation
json_user(UserId) ->
    {struct, [{<<"user_id">>, UserId}]}.

