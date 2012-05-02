-module(awkin_web_opr).
-export([news/1, content/1, click/1, unclick/1, favor/1, unfavor/1, 
            like/1, unlike/1, dislike/1, undislike/1, share/1]).
-include("hrldir/config.hrl").

%Feedback
feedback(S, Action) ->
    Item = struct:get_value(<<"item">>, S),
    User = struct:get_value(<<"user">>, S),

    Auth = awkin_client:json_auth(<<"test">>, <<"test">>),
    JUser = awkin_cj_cli:json_user(User),
    JItem = awkin_cj_cli:json_item(Item),
    Cmd = awkin_cj_cli:cmd(Auth, JUser, JItem, Action),
    awkin_cj_cli:send_cmd(Cmd),
    {struct, [{<<"status">>, <<"ok">>}]}.
    %{struct, [{<<"status">>, <<"ok">>}, {<<"id">>, ID}]}.
click(S) ->
    feedback(S, <<"click">>).
unclick(S) ->
    feedback(S, <<"unclick">>).
favor(S) ->
    feedback(S, <<"favor">>).
unfavor(S) ->
    feedback(S, <<"unfavor">>).
like(S) ->
    feedback(S, <<"like">>).
unlike(S) ->
    feedback(S, <<"unlike">>).
dislike(S) ->
    feedback(S, <<"dislike">>).
undislike(S) ->
    feedback(S, <<"undislike">>).
share(S) ->
    feedback(S, <<"share">>).

%Get new items
news(S) ->
    UserId = struct:get_value(<<"user">>, S),
    LimitStr = binary_to_list(struct:get_value(<<"limit">>, S)),
    BaseId = struct:get_value(<<"base_id">>, S, <<"">>),
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
    Item = awkin_dylan_cli:json_items(NumOfItem, <<"2">>, BaseId),
    %Item = awkin_dylan_cli:json_items(NumOfItem),
    User = awkin_dylan_cli:json_user(UserId),
    Auth = awkin_client:json_auth(<<"test">>, <<"test">>),
    Cmd = awkin_dylan_cli:cmd_get_item(Auth, User, Item),
    % send request
    Res = awkin_dylan_cli:send_cmd(?DylanHost, ?DylanPort, Cmd),
    %io:format("~s", [Res]),
    ResJ = mochijson2:decode(Res),
    {struct, [{<<"status">>, <<"ok">>}, {<<"data">>, ResJ}]}.

%Get item content according to the id
content(S) ->
    IDStr = binary_to_list(struct:get_value(<<"id">>, S)),
    case awkin_db:list_to_mongoid(IDStr) of
    {ok, IDB} -> 
        Content = awkin_db:get_content_of_item(IDB),
        {struct, [{<<"status">>, <<"ok">>}, {<<"data">>, Content}]};
    {fail, _} ->
        {struct, [{<<"status">>, <<"error">>}]}
    end.
