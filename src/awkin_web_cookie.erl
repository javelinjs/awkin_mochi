-module(awkin_web_cookie).
-compile(export_all).

get_cookie_value(Req, Key, Default) ->
    case Req:get_cookie_value(Key) of
        undefined -> Default;
        Value -> Value
    end.
