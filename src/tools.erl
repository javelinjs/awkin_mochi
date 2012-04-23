-module(tools).
-compile(export_all).

salt() ->
    {_, _, Salt} = erlang:now(),
    integer_to_list(Salt).

sha_list(Data) ->
    <<Mac:160/integer>> = crypto:sha(Data),
    lists:flatten(io_lib:format("~40.16.0b", [Mac])).

