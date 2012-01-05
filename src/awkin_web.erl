%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for awkin.

-module(awkin_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).

%% External API
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
                        %Items = awkin_db:items(),
                        %{ok, HTMLOutput} = read_dtl:render([{items, Items}]),
                        {ok, HTMLOutput} = read_dtl:render([{username, "javelinjs"}]),
                        Req:respond({200, [{"Content-Type", "text/html"}],
                                HTMLOutput});
                    _ ->
                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                case Path of
                    "operation" ->
                        PostData = Req:parse_post(),
                        %Username = proplists:get_value("username", PostData, "Anonymous"),
                        Json = proplists:get_value("data", PostData),
                        Struct = mochijson2:decode(Json),
                        A = struct:get_value(<<"action">>, Struct),

                        % take action according to the user input
                        Action = list_to_atom(binary_to_list(A)),
                        Result = 
                            try awkin_web_opr:Action(Struct) of
                                {struct, RList} -> {struct, RList}
                            catch
                                _:_ -> {struct, [{<<"status">>, <<"error">>}]}
                            end,

                        DataOut = mochijson2:encode(Result),

                        Req:ok({"application/json", [], [DataOut]});
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
