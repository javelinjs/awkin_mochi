-module(awkin_cj_cli).
-compile(export_all).

-include("hrldir/config.hrl").

send_cmd(Host, Port, Cmd) ->
    awkin_client:send_cmd(Host, Port, Cmd, ?CJEnd).

cmd_click(Auth, User, Item) ->
    cmd(Auth, User, Item, <<"click">>).
cmd_favor(Auth, User, Item) ->
    cmd(Auth, User, Item, <<"favor">>).
cmd_like(Auth, User, Item) ->
    cmd(Auth, User, Item, <<"like">>).
cmd_dislike(Auth, User, Item) ->
    cmd(Auth, User, Item, <<"dislike">>).
cmd_share(Auth, User, Item) ->
    cmd(Auth, User, Item, <<"share">>).
cmd(Auth, User, Item, Action) ->
    Data = {struct, [{<<"user">>, User},
                {<<"item">>, Item}
    ]},
    Cmd = {struct, [{<<"auth">>, Auth},
                {<<"cmd">>, Action},
                {<<"data">>, Data}
    ]},
    mochijson:binary_encode(Cmd).

% items infomation
json_item(ItemId) ->
    {struct, [{<<"item_id">>, ItemId}]}.
%user infomation
json_user(UserId) ->
    {struct, [{<<"user_id">>, UserId}]}.

