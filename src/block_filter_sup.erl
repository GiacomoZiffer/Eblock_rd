%%%-------------------------------------------------------------------
%%% @author Giacomo
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2019 19:47
%%%-------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
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
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
  RestartStrategy = rest_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Son1 = {block_proxy, {block_proxy, start_link, []},
    permanent, 2000, worker, [block_proxy]},
  Son2 = {block_filter, {block_filter, start_link, []},
    permanent, 2000, worker, [block_filter]},
  Son3 = {block_message_handler, {block_message_handler, start_link, []},
    permanent, 2000, worker, [block_message_handler]},
  Son4 = {block_r_gateway, {block_r_gateway, start_link, []},
    permanent, 2000, worker, [block_r_gateway]},
  Son5 = {block_r_sup, {block_r_sup, start_link, []},
    permanent, 2000, supervisor, [block_r_sup]},

  {ok, {SupFlags, [Son1, Son2, Son3, Son4, Son5]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
