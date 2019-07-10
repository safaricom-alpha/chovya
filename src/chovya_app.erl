%%%-------------------------------------------------------------------
%% @doc chovya public API
%% @end
%%%-------------------------------------------------------------------

-module(chovya_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    chovya_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
