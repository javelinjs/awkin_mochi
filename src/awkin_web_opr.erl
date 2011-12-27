-module(awkin_web_opr).
-compile(export_all).

read(S) ->
    ID = struct:get_value(<<"item">>, S),
    {struct, [{<<"status">>, <<"ok">>}, {<<"id">>, ID}]}.
