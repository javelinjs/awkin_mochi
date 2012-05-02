-module(struct).
-compile(export_all).

get_value(Key, Struct, Def) ->
    {struct, JsonData} = Struct,
    proplists:get_value(Key, JsonData, Def).
get_value(Key, Struct) ->
    get_value(Key, Struct, <<"undefined">>).
set_value(Key, Value, Struct) ->
    {struct, PropList} = Struct,
    {struct, [{Key, Value}|PropList]}.
