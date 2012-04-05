-module(awkin_user).
-compile(export_all).
-include("hrldir/config.hrl").

salt() ->
    {_, _, Salt} = erlang:now(),
    integer_to_list(Salt).

sha_list(Data) ->
    <<Mac:160/integer>> = crypto:sha(Data),
    lists:flatten(io_lib:format("~40.16.0b", [Mac])).

%cookie related
cookie_auth_string(Uid, PwdEncoded, Salt) ->
    sha_list(Salt ++ sha_list(Uid++PwdEncoded++Salt) ++ Uid).

cookie_encode(Uid, Nickname, PwdEncoded, Salt) ->
    Uid ++ "\t" ++ Nickname ++ "\t" ++ cookie_auth_string(Uid, PwdEncoded, Salt).
cookie_decode(Cookie) ->
    Tokens = string:tokens(Cookie, "\t"),
    [Uid, Nickname, AuthString] = Tokens,
    %proplists
    [{"uid", Uid}, {"nickname", Nickname}, {"authstring", AuthString}].

cookie(Uid, Nickname, PwdEncoded, Salt, Expired) ->
    User = Uid ++ "\t" ++ Nickname ++ "\t" ++ cookie_auth_string(Uid, PwdEncoded, Salt),
    mochiweb_cookies:cookie("user", User, [{path, "/"}, {max_age, Expired}]).

%db related
pwd_encode(Pwd, Salt) ->
    sha_list(sha_list(Pwd) ++ Salt).

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
        Salt = salt(),
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
                                    mongo:find_one(user, {email, {Email}})
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
