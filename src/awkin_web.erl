%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for awkin.

-module(awkin_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).

%% External API
channels() ->
    {ok, Conn} = mongo:connect({localhost, 27017}),
    {ok, Cursor} = mongo:do(unsafe, slave_ok, Conn, awkin, 
                        fun() -> mongo:find(channel, {}) end),
    Doc = mongo:next(Cursor),
    {{'_id', ID, buildFreq, BuildFreq, desc, Desc, lastBuildDate, LastBuildDate, 
        link, Link, title, Title}} = Doc,
    [{id, ID}, {buildFreq, BuildFreq}].

get_channel_from_item(Item, Conn) ->
    ChannelID = proplists:get_value(channel_id, Item, 0), 
    {ok, DocTuple} = mongo:do(unsafe, slave_ok, Conn, awkin,
                            fun() -> 
                                mongo:find_one(channel, {'_id', ChannelID})
                            end
                        ),
    {Title} = 
        case DocTuple of
            {Doc} -> bson:lookup(title, Doc); 
            _ -> {"Unknown"}
        end,
    Item ++ [{channel_title, Title}].
    
    
items() ->
    {ok, Conn} = mongo:connect({localhost, 27017}),
    {ok, Cursor} = mongo:do(unsafe, slave_ok, Conn, awkin, 
                            fun() -> mongo:find(item, {}) end),
    Items = awkin_db:get_items(Cursor),
    lists:map(fun(Item)->get_channel_from_item(Item, Conn) end, Items).
    %Items.


start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                    "read" ->
                        Items = items(),
                        %Items = [{id, 1234}],
                        {ok, HTMLOutput} = read_dtl:render([{items, Items}]),
                        Req:respond({200, [{"Content-Type", "text/html"}],
                                HTMLOutput});
                    _ ->
                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                case Path of
                    _ ->
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
