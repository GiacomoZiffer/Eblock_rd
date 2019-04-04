%%%-------------------------------------------------------------------
%% @doc Eblock_rd public API
%% @end
%%%-------------------------------------------------------------------

-module(Eblock_rd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Eblock_rd_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
