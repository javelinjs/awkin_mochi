-module(struct).
-compile(export_all).

get_value(Key, Struct) ->
    {struct, JsonData} = Struct,
    proplists:get_value(Key, JsonData, <<"undefined">>).
