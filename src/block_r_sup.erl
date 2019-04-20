-module(block_r_sup).
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

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
init([]) ->
  block_naming_hnd:notify_identity(self(), block_r_sup),

  RestartStrategy = simple_one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = temporary,
  Shutdown = 2000,
  Type = worker,

  AChild = {block_request, {block_request, start_link, []},
    Restart, Shutdown, Type, [block_request]},

  {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
