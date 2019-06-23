-module(block_mng_sup).
-author("Giacomo").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  block_naming_mng:wait_for_handler(),

  RestartStrategy = rest_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Son1 = {block_resource_handler, {block_resource_handler, start_link, []},
    permanent, 2000, worker, [block_resource_handler]},
  Son2 = {block_filter_sup, {block_filter_sup, start_link, []},
    permanent, 2000, supervisor, [block_filter_sup]},

  {ok, {SupFlags, [Son1, Son2]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
