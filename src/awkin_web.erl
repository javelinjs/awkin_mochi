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
        %Fetch cookies
        {{G_UserId, G_NickName}, {G_LoginDisp, G_LoginUrl}} = 
            awkin_user:fetch_cookie(
                awkin_web_cookie:get_cookie_value(Req, "user", not_found)
            ),
        G_Login = [{login_disp, G_LoginDisp}, {login_url, G_LoginUrl}],

        case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
            "read" ->
                {ok, HTMLOutput} = read_dtl:render(G_Login++[{username, G_NickName}]),
                Req:respond({200, [?ContentType], HTMLOutput});
            "logout" ->
                Cookie = awkin_user:cookie_clear(),
                Req:respond({302, [Cookie, {"Location", "/read"}, ?ContentType], ""});
            "login" ->
                {ok, HTMLOutput} = login_dtl:render([{hint, ""}]),
                Req:respond({200, [?ContentType], HTMLOutput});
            "register" ->
                {ok, HTMLOutput} = 
                    register_dtl:render([{hint, ""}, {email, ""}, {nickname, ""}]),
                Req:ok({"text/html", [?ContentType], HTMLOutput});
            _ ->
                Req:serve_file(Path, DocRoot)
            end;
        'POST' ->
            PostData = Req:parse_post(),
            case Path of
            "operation" ->
                Json = proplists:get_value("data", PostData),
                %Struct = mochijson2:decode(Json),
                Struct = struct:set_value(<<"user">>, list_to_binary(G_UserId), 
                                            mochijson2:decode(Json)),
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
                Email = proplists:get_value("Email", PostData, ""),
                Nickname = proplists:get_value("Nickname", PostData, Email),
                Pwd = proplists:get_value("Pwd", PostData, ""),
                PwdAgain = proplists:get_value("PwdAgain", PostData, ""),
                Req:respond(awkin_user:do_register(Email, Nickname, Pwd, PwdAgain));
            "login" ->
                Email = proplists:get_value("Email", PostData, ""),
                Pwd = proplists:get_value("Pwd", PostData, ""),
                {_Valid, Respond} = awkin_user:do_login(Email, Pwd),
                Req:respond(Respond);
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
