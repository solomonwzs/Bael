%% @author Mochi Media <dev@mochimedia.com>
%% @copyright bael Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the bael application.

-module(bael_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for bael.
start(_Type, _StartArgs) ->
    bael_deps:ensure(),
    bael_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for bael.
stop(_State) ->
    ok.
