-module(block_message_handler).
-author("Giacomo").

-behaviour(gen_server).

%% API
-export([start_link/0,
  handle_msg/3]).

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

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

handle_msg(Comm, From, Params) ->
  PID = block_naming_hnd:get_identity(msg_handler),
  gen_server:cast(PID, {handle, Comm, From, Params}).

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
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  block_naming_hnd:notify_identity(self(), msg_handler),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(Request, _From, State) ->
  io:format("BLOCK MESSAGE HANDLER: Unexpected call message: ~p~n", [Request]),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast({handle, Comm, From, Params}, State) ->
  handle_message(Comm, From, Params),
  {noreply, State};

handle_cast(Request, State) ->
  io:format("BLOCK MESSAGE HANDLER: Unexpected cast message: ~p~n", [Request]),
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
handle_info(Info, State) ->
  io:format("BLOCK MESSAGE HANDLER: Unexpected ! message: ~p~n", [Info]),
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
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
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
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_message(add, _From, Params) ->
  {Name, Data} = Params,
  ID = block_filter:get_res_id(Name),
  block_resource_handler:add(Name, ID, Data);

handle_message(ask_res, From, Name) ->
  Response = block_resource_handler:get(Name),
  case Response of
    {ok, Data} ->
      block_filter:send_response(res_reply, Name, Data, From);
    {error, _Reason} -> block_filter:send_response(no_res, Name, no_data, From)
  end;

handle_message(res_reply, _From, Params) ->
  block_r_gateway:send_response(Params, ask_res);

handle_message(no_res, _From, Params) ->
  block_r_gateway:send_response(Params, ask_res);

handle_message(delete, _From, Name) ->
  block_resource_handler:delete(Name);

handle_message(safe_add, From, Params) ->
  {Name, Data} = Params,
  ID = block_filter:get_res_id(Name),
  Result = block_resource_handler:safe_add(Name, ID, Data),
  block_filter:send_response(safe_add_reply, Name, atom_to_list(Result), From);

handle_message(safe_add_reply, _From, Params) ->
  block_r_gateway:send_response(Params, safe_add);

handle_message(safe_delete, From, Name) ->
  Result = block_resource_handler:delete(Name),
  block_filter:send_response(safe_delete_reply, Name, atom_to_list(Result), From);

handle_message(safe_delete_reply, _From, Params) ->
  block_r_gateway:send_response(Params, safe_delete);

handle_message(drop, _From, From) ->
  block_resource_handler:drop(From);

handle_message(add_many, _From, Resources) ->
  lists:map(fun(Res) -> handle_message(add, no_addr, Res) end, Resources);    %%TODO check if this works

handle_message(get_many, From, ID) ->
  ResList = block_resource_handler:get_many(ID),
  gen_server:reply(From, ResList);

handle_message(pop, From, Name) ->
  Response = block_resource_handler:get(Name),
  case Response of
    {ok, Data} ->
      block_filter:send_response(pop_reply, Name, Data, From);
    {error, _Reason} -> block_filter:send_response(no_pop, Name, no_data, From)
  end;

handle_message(pop_reply, _From, Params) ->
  block_r_gateway:send_response(Params, pop);

handle_message(no_pop, _From, Params) ->
  block_r_gateway:send_response(Params, pop).