-module(block_filter_sup).
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
  RestartStrategy = rest_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Son1 = {block_filter, {block_filter, start_link, []},
    permanent, 2000, worker, [block_filter]},
  Son2 = {block_message_handler, {block_message_handler, start_link, []},
    permanent, 2000, worker, [block_message_handler]},
  Son3 = {block_r_gateway, {block_r_gateway, start_link, []},
    permanent, 2000, worker, [block_r_gateway]},
  Son4 = {block_r_sup, {block_r_sup, start_link, []},
    permanent, 2000, supervisor, [block_r_sup]},

  {ok, {SupFlags, [Son1, Son2, Son3, Son4]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
