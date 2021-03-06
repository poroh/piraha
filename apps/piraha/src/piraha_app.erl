%%%-------------------------------------------------------------------
%% @doc piraha public API
%% @end
%%%-------------------------------------------------------------------

-module(piraha_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    piraha_psip_callbacks:setup(),
    piraha_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
