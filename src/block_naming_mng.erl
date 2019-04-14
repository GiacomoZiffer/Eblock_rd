-module(block_naming_mng).
-author("Giacomo").

-behaviour(gen_server).

%% API
-export([start_link/1, wait_for_handler/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
wait_for_handler() ->
  case whereis(block_naming_hnd) of
    undefined ->
      timer:sleep(5),
      wait_for_handler();
    Pid -> Pid
  end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(Supervisor) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Supervisor], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Supervisor]) ->
  case whereis(block_naming_hnd) of
    undefined ->
      self() ! {startup, Supervisor};
    Pid  ->
      blcok_naming_hnd:reheir(Pid, self())
  end,
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call(Request, _From, State) ->
  io:format("BLOCK --- NAMING MANAGER: Unexpected call message: ~p~n", [Request]),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(Request, State) ->
  io:format("BLOCK --- NAMING MANAGER: Unexpected cast message: ~p~n", [Request]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({startup, Supervisor}, State) ->
  ETSHandler = {block_naming_hnd, {block_naming_hnd, start_link, []},
    permanent, 2000, worker, [block_naming_hnd]},
  Ret = supervisor:start_child(Supervisor, ETSHandler),
  case Ret of
    {ok, Handler} ->
      TableId = ets:new(block_naming_db, [set, public, named_table, {heir, self(), block_naming_db}]),
      ets:give_away(TableId, Handler, naming_db);
    {ok, Handler, _} ->
      TableId = ets:new(block_naming_db, [set, public, named_table, {heir, self(), block_naming_db}]),
      ets:give_away(TableId, Handler, block_naming_db)
  end,
  {noreply, State#state{}};

handle_info({'ETS-TRANSFER', TableId, Pid, Data}, State) ->
  io:format("BLOCK --- Warning TableId: ~p HandlerPid: ~p is dying~n"
  "Table is returning to Manager, in order to be passed to the new Handler~n", [TableId, Pid]),     %TODO remove this line
  ets:delete(block_naming_db, block_naming_hnd),
  Handler = wait_for_handler(),
  ets:give_away(TableId, Handler, Data),
  {noreply, State#state{}};

handle_info(Info, State) ->
  io:format("BLOCK --- NAMING MANAGER: Unexpected ! message: ~p~n", [Info]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================