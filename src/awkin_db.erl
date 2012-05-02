-module(awkin_db).
-compile(export_all).
-include("hrldir/config.hrl").

% How to turn string into _id
%A = list_to_integer("4ed60ef7d481ff1cd53f20bb", 16).
%{<<A:96>>}

mongoid_to_list(ID) ->
    {IDBinary} = ID,
    <<IDInteger:96>> = IDBinary,
    integer_to_list(IDInteger, 16).
list_to_mongoid(IDStr) ->
    try list_to_integer(IDStr, 16) of
    ID ->
        {ok, {<<ID:96>>}}
    catch _:_ ->
        {fail, {}}
    end.

%items related
get_content_of_item(ID) ->
    {ok, Conn} = mongo:connect({?MongoHost, ?MongoPort}),
    {ok, DocTuple} = mongo:do(unsafe, slave_ok, Conn, awkin,
                            fun() -> 
                                mongo:auth(?MongoUser, ?MongoPwd),
                                mongo:find_one(item, {'_id', ID})
                            end
                        ),
    mongo:disconnect(Conn),
    {Res} =
        case DocTuple of
            {Doc} -> 
                Content = bson:lookup(content, Doc),
                % if 'content' field is empty, then lookup the 'desc' field
                case Content of
                    {<<>>} -> bson:lookup(desc, Doc);
                    _ -> Content
                end;
            _ -> {<<"Empty">>}
        end,
    Res.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
items() ->
    {ok, Conn} = mongo:connect({?MongoHost, ?MongoPort}),
    {ok, Cursor} = mongo:do(unsafe, slave_ok, Conn, ?MongoDB, 
                            fun() -> 
                                mongo:auth(?MongoUser, ?MongoPwd),
                                mongo:find(item, {}) 
                            end),
    Items = get_items(Cursor),
    Res = lists:map(fun(Item)->get_channel_from_item(Item, Conn) end, Items),
    mongo:disconnect(Conn),
    Res.
    %Items.
    
get_channel_from_item(Item, Conn) ->
    ChannelID = proplists:get_value(channel_id, Item, 0), 
    {ok, DocTuple} = mongo:do(unsafe, slave_ok, Conn, awkin,
                            fun() -> 
                                mongo:auth(?MongoUser, ?MongoPwd),
                                mongo:find_one(channel, {'_id', ChannelID})
                            end
                        ),
    {Title} = 
        case DocTuple of
            {Doc} -> bson:lookup(title, Doc); 
            _ -> {"Unknown"}
        end,
    Item ++ [{channel_title, Title}].
    
get_items({}, Cursor) -> [];
get_items(DocTuple, Cursor) -> 
    %{{'_id', ID, channel, Channel, title, Title, desc, Desc, 
        %content, Content, link, Link, pubDate, PubDate}} = DocTuple,
    {Doc} = DocTuple,
    {ID} = bson:lookup('_id', Doc),
    {ChannelID} = bson:lookup(channel, Doc),
    {Title} = bson:lookup(title, Doc),
    {Desc} = bson:lookup(desc, Doc),
    {Content} = bson:lookup(content, Doc),
    {Link} = bson:lookup(link, Doc),
    {PubDate} = bson:lookup(pubDate, Doc),

    {IDBinary} = ID,
    %[First|_] = binary_to_list(IDBinary),
    %IDForDisp = lists:map(
                    %fun(X) -> 
                        %if 
                            %X=:=First -> lists:flatten(io_lib:format("~p", [X])); 
                            %true -> lists:flatten(io_lib:format("_~p", [X])) 
                        %end 
                    %end,  binary_to_list(IDBinary)),

    % See http://api.mongodb.org/erlang/bson/
    <<Time:32, MP:40, Count:24>> = IDBinary,
    <<IDForDisp:96>> = IDBinary,
    DocList = [{id, ID}, {id_for_disp, IDForDisp}, {channel_id, ChannelID},
        {title, Title}, {link, Link}, {desc, Desc}, {content, Content}, 
        {pubDate, calendar:now_to_local_time(PubDate)}],
    DocTupleNext = mongo:next(Cursor),
    [DocList | get_items(DocTupleNext, Cursor)].
get_items(Cursor) -> get_items(mongo:next(Cursor), Cursor).
