%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc awkin.

-module(awkin).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the awkin server.
start() ->
    awkin_deps:ensure(),
    ensure_started(crypto),
    ensure_started(mongodb),
    application:start(awkin).


%% @spec stop() -> ok
%% @doc Stop the awkin server.
stop() ->
    application:stop(awkin).
