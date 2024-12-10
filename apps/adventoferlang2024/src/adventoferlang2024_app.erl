%%%-------------------------------------------------------------------
%% @doc adventoferlang2024 public API
%% @end
%%%-------------------------------------------------------------------

-module(adventoferlang2024_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    adventoferlang2024_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
