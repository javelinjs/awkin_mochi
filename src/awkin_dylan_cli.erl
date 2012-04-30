-module(awkin_dylan_cli).
-compile(export_all).

-include("hrldir/config.hrl").

send_cmd(Host, Port, Cmd) ->
    awkin_client:send_cmd(Host, Port, Cmd, ?DylanEnd).

cmd_get_item(Auth, User, Item) ->
    Data = {struct, [{<<"user">>, User},
                {<<"item">>, Item}
    ]},
    Cmd = {struct, [{<<"auth">>, Auth},
                {<<"cmd">>, <<"get_item">>},
                {<<"data">>, Data}
    ]},
    mochijson:binary_encode(Cmd).

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

