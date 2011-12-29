-module(awkin_web_opr).
-compile(export_all).
-include("hrldir/config.hrl").

read(S) ->
    ID = struct:get_value(<<"item">>, S),
    {struct, [{<<"status">>, <<"ok">>}, {<<"id">>, ID}]}.

%Get new items
news(S) ->
    UserId = struct:get_value(<<"user">>, S),
    <<Limit>> = struct:get_value(<<"limit">>, S),
    NumOfItem = if 
        is_integer(Limit), Limit > 0 -> <<Limit>>; 
        true -> ?SizeItemSetDef
    end,
    Item = awkin_dylan:json_items(NumOfItem),
    User = awkin_dylan:json_user(UserId),
    Auth = awkin_dylan:json_auth("test", "test"),
    Cmd = awkin_dylan:cmd_get_item(Auth, User, Item),
    % send request
    Res = awkin_dylan:send_cmd(?DylanHost, ?DylanPort, Cmd),
    io:format("~s", [Res]),
    mochijson2:decode(Res).
