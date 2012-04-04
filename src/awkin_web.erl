%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for awkin.

-module(awkin_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).

-include("hrldir/config.hrl").
-include("hrldir/message.hrl").

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
                    {ok, HTMLOutput} = read_dtl:render([{username, "Guest"}]),
                    Req:respond({200, [?ContentType], HTMLOutput});
                "register" ->
                    {ok, HTMLOutput} = register_dtl:render([{hint, ""}, {email, ""}, {nickname, ""}]),
                    %%FIXME: TEST
                    Cookie = mochiweb_cookies:cookie("uid", "setUid", [{path, "/"}]),
                    Req:ok({"text/html", [Cookie], HTMLOutput});
                    %Req:respond({200, [?ContentType], HTMLOutput});
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
                "register" ->
                    PostData = Req:parse_post(),
                    Email = proplists:get_value("Email", PostData, ""),
                    Nickname = proplists:get_value("Nickname", PostData, Email),
                    Pwd = proplists:get_value("Pwd", PostData, ""),
                    PwdAgain = proplists:get_value("PwdAgain", PostData, ""),

                    %%FIXME:TEST
                    Uid = awkin_web_cookie:get_cookie_value(Req, "uid", "NotFound"),
                    io:format("~s\n", [Uid]),

                    {ok, HTMLOutput} = 
                        if 
                        Email =:= "" ->
                            register_dtl:render([{hint, ?REG_HINT_EmptyEmail}, {email, ""}, 
                                                    {nickname, ""}]);
                        Pwd =:= "" ->
                            register_dtl:render([{hint, ?REG_HINT_EmptyPwd}, {email, Email},
                                                    {nickname, Nickname}]);
                        Pwd =/= PwdAgain ->
                            register_dtl:render([{hint, ?REG_HINT_PwdNotMatch}, {email, Email},
                                                    {nickname, Nickname}]);
                        true ->
                            io:format("~s\n~s\n~s\n~s\n~s\n", [Email, Nickname, Pwd, 
                                        PwdAgain, awkin_db:sha_list(list_to_binary(Pwd))]),
                            {ok, null}
                        end,

                    case HTMLOutput of 
                    null ->
                        Req:respond({302, [{"Location", "/read"}, ?ContentType], ""}); 
                    _ ->
                        Req:respond({200, [?ContentType], HTMLOutput})
                    end; 
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
