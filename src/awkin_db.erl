-module(awkin_db).
-export([get_items/1, get_items/2]).

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
    [First|_] = binary_to_list(IDBinary),
    IDForDisp = lists:map(
                    fun(X) -> 
                        if 
                            X=:=First -> lists:flatten(io_lib:format("~p", [X])); 
                            true -> lists:flatten(io_lib:format("_~p", [X])) 
                        end 
                    end,  binary_to_list(IDBinary)),

    DocList = [{id, ID}, {id_for_disp, IDForDisp}, {channel_id, ChannelID},
        {title, Title}, {link, Link}, {desc, Desc}, {content, Content}, 
        {pubDate, calendar:now_to_local_time(PubDate)}],
    DocTupleNext = mongo:next(Cursor),
    [DocList | get_items(DocTupleNext, Cursor)].

get_items(Cursor) -> get_items(mongo:next(Cursor), Cursor).
