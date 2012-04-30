-module(awkin_user).
-compile(export_all).
-include("hrldir/config.hrl").
-include("hrldir/message.hrl").

%cookie related
cookie_auth_string(Uid, PwdEncoded, Salt) ->
    tools:sha_list(Salt ++ tools:sha_list(Uid++PwdEncoded++Salt) ++ Uid).

cookie_encode(Uid, Nickname, PwdEncoded, Salt) ->
    Uid ++ "||" ++ Nickname ++ "||" ++ cookie_auth_string(Uid, PwdEncoded, Salt).
cookie_decode(Cookie) ->
    Tokens = string:tokens(Cookie, "||"),
    [Uid, Nickname, AuthString] = Tokens,
    %proplists
    [{uid, Uid}, {nickname, Nickname}, {authstring, AuthString}].

cookie(UserInfoPropList, Expired) ->
    Uid = integer_to_list(proplists:get_value(id_for_disp, UserInfoPropList)),
    PwdEncoded = proplists:get_value(pwd, UserInfoPropList),
    Salt = proplists:get_value(salt2, UserInfoPropList),
    Nickname = proplists:get_value(nickname, UserInfoPropList),
    cookie(Uid, Nickname, PwdEncoded, Salt, Expired).
cookie(Uid, Nickname, PwdEncoded, Salt, Expired) ->
    User = cookie_encode(Uid, Nickname, PwdEncoded, Salt),
    mochiweb_cookies:cookie("user", User, [{path, "/"}, {max_age, Expired}]).
cookie_clear() ->
    mochiweb_cookies:cookie("user", "", [{path, "/"}]).
    

%db related
pwd_encode(Pwd, Salt) ->
    tools:sha_list(tools:sha_list(Pwd) ++ Salt).

exist(Email) ->
    {ok, Conn} = mongo:connect({?MongoHost, ?MongoPort}),
    {ok, Cursor} = mongo:do(unsafe, slave_ok, Conn, ?MongoDB, 
                            fun()->
                                mongo:auth(?MongoUser, ?MongoPwd),
                                mongo:find_one(user, {email, Email}) 
                            end
                   ),
    case Cursor of
        {} -> false;
        _ -> true 
    end.

create(Email, Nickname, Pwd) ->
    case exist(Email) of
    % first register
    false -> 
        Salt = tools:salt(),
        PwdEncoded = pwd_encode(Pwd, Salt),
        {ok, Conn} = mongo:connect({?MongoHost, ?MongoPort}),
        mongo:do(safe, master, Conn, ?MongoDB, fun()->
                    mongo:auth(?MongoUser, ?MongoPwd),
                    mongo:insert(user, {email, Email, nickname, Nickname, pwd, PwdEncoded,
                                        salt1, Salt, salt2, Salt}) 
                end),
        mongo:disconnect(Conn),
        true;
    true ->
        false
    end.

find_by_email(Email) ->
    {ok, Conn} = mongo:connect({?MongoHost, ?MongoPort}),
    {ok, DocTuple} = mongo:do(unsafe, slave_ok, Conn, ?MongoDB,
                                fun() ->
                                    mongo:auth(?MongoUser, ?MongoPwd),
                                    mongo:find_one(user, {email, Email})
                                end
                            ),
    mongo:disconnect(Conn),
    case DocTuple of
    {Doc} ->
        fetch_to_proplist(Doc);
    _ ->
        not_found
    end.
find_by_id(IDStr) ->
    %IDStr = binary_to_list(struct:get_value(<<"id">>, S)),
    try list_to_integer(IDStr, 16) of
    ID ->
        IDB = <<ID:96>>,
        {ok, Conn} = mongo:connect({?MongoHost, ?MongoPort}),
        {ok, DocTuple} = mongo:do(unsafe, slave_ok, Conn, awkin,
                                        fun() ->
                                            mongo:auth(?MongoUser, ?MongoPwd),
                                            mongo:find_one(user, {'_id', {IDB}})
                                        end
                                ),
        mongo:disconnect(Conn),
        case DocTuple of
        {Doc} ->
            fetch_to_proplist(Doc);
        _ ->
            not_found
        end
    catch _:_ ->
        not_found
    end.

fetch_to_proplist(Doc) ->
    {ID} = bson:lookup('_id', Doc),
    {Email} = bson:lookup(email, Doc),
    {Nickname} = bson:lookup(nickname, Doc),
    {Pwd} = bson:lookup(pwd, Doc),
    {Salt1} = bson:lookup(salt1, Doc),
    {Salt2} = bson:lookup(salt2, Doc),

    {IDBinary} = ID,
    <<IDForDisp:96>> = IDBinary,

    [{id, ID}, {id_for_disp, IDForDisp}, {email, Email}, {nickname, Nickname},
        {pwd, Pwd}, {salt1, Salt1}, {salt2, Salt2}].

%Opr related
fetch_cookie(Cookie) ->
    case Cookie of
    S when S =:= not_found; S =:= "" ->
        {{?GuestId, ?GuestName}, {?DISP_LOGIN, ?URL_LOGIN}};
    UserCookie ->
        UserList = awkin_user:cookie_decode(UserCookie),
        {{proplists:get_value(uid, UserList), proplists:get_value(nickname, UserList)},
            {?DISP_LOGOUT, ?URL_LOGIN}}
    end.

do_login(Email, Pwd) ->
    case find_by_email(Email) of
    not_found ->
        {ok, HTMLOutput} = login_dtl:render([{hint, ?LOGIN_HINT_NOTFOUND}]),
        {false, {200, [?ContentType], HTMLOutput}};
    UserInfo ->
        PwdInDB = proplists:get_value(pwd, UserInfo),        
        case pwd_encode(Pwd, proplists:get_value(salt1, UserInfo)) of
        PwdEncoded when PwdEncoded =:= PwdInDB ->
            Cookie = awkin_user:cookie(UserInfo, ?CookieSec),
            {true, {302, [Cookie, {"Location", "/read"}, ?ContentType], ""}};
        _ ->
            {ok, HTMLOutput} = login_dtl:render([{hint, ?LOGIN_HINT_VERIFY_FAIL}]),
            {false, {200, [?ContentType], HTMLOutput}}
        end
    end.
        
do_register(Email, Nickname, Pwd, PwdAgain) ->
    {DataValid, Dtl} = 
        if 
        Email =:= "" ->
            {false, [{hint, ?REG_HINT_EmptyEmail}, {email, ""}, {nickname, ""}]};
        Pwd =:= "" ->
            {false, [{hint, ?REG_HINT_EmptyPwd}, {email, Email}, {nickname, Nickname}]};
        Pwd =/= PwdAgain ->
            {false, [{hint, ?REG_HINT_PwdNotMatch}, {email, Email}, {nickname, Nickname}]};
        true ->
            {true, null}
        end,

    case DataValid of 
    true ->
        awkin_user:create(Email, Nickname, Pwd),
        case awkin_user:find_by_email(Email) of
        not_found ->
            {ok, HTMLOutput} = register_dtl:render(
                                [{hint, ?REG_HINT_InternalError}, 
                                    {email, Email},
                                    {nickname, Nickname}]),
            {200, [?ContentType], HTMLOutput};
        UserInfo ->
            Cookie = awkin_user:cookie(UserInfo, ?CookieSec),
            {302, [Cookie, {"Location", "/read"}, ?ContentType], ""}
        end;
    _ ->
        {ok, HTMLOutput} = register_dtl:render(Dtl),
        {200, [?ContentType], HTMLOutput}
    end.
