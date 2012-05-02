-module(awkin_cj_cli).
-compile(export_all).

-include("hrldir/config.hrl").

send_cmd(Cmd) ->
    awkin_client:send_cmd_nonblocking(?CJHost, ?CJPort, Cmd).

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

