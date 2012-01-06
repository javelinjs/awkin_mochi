-module(awkin_web_opr).
-compile(export_all).
-include("hrldir/config.hrl").

read(S) ->
    ID = struct:get_value(<<"item">>, S),
    {struct, [{<<"status">>, <<"ok">>}, {<<"id">>, ID}]}.

%Get new items
news(S) ->
    UserId = struct:get_value(<<"user">>, S),
    LimitStr = binary_to_list(struct:get_value(<<"limit">>, S)),
    NumOfItem = 
        try list_to_integer(LimitStr) of
            Limit ->
                case Limit > 0 of
                    true -> Limit;
                    false -> list_to_binary(integer_to_list(?SizeItemSetDef))
                end
        catch _:_ -> 
            list_to_binary(integer_to_list(?SizeItemSetDef))
        end,
    Item = awkin_dylan:json_items(NumOfItem, <<"2">>),
    %Item = awkin_dylan:json_items(NumOfItem),
    User = awkin_dylan:json_user(UserId),
    Auth = awkin_dylan:json_auth("test", "test"),
    Cmd = awkin_dylan:cmd_get_item(Auth, User, Item),
    % send request
    Res = awkin_dylan:send_cmd(?DylanHost, ?DylanPort, Cmd),
    %io:format("~s", [Res]),
    ResJ = mochijson2:decode(Res),
    {struct, [{<<"status">>, <<"ok">>}, {<<"data">>, ResJ}]}.

%Get item content according to the id
content(S) ->
    IDStr = binary_to_list(struct:get_value(<<"id">>, S)),
    try list_to_integer(IDStr, 16) of
        ID -> 
            IDB = <<ID:96>>,
            Content = awkin_db:get_content_of_item(IDB),
            {struct, [{<<"status">>, <<"ok">>}, {<<"data">>, Content}]}
    catch _:_ ->
        {struct, [{<<"status">>, <<"error">>}]}
    end.
