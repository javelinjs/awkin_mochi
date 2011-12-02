%% @author Mochi Media <dev@mochimedia.com>
%% @copyright awkin Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the awkin application.

-module(awkin_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for awkin.
start(_Type, _StartArgs) ->
    awkin_deps:ensure(),
    awkin_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for awkin.
stop(_State) ->
    ok.
