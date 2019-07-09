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

handle_msg(Comm, From, Params) ->
  PID = block_naming_hnd:get_identity(msg_handler),
  gen_server:cast(PID, {handle, Comm, From, Params}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  block_naming_hnd:notify_identity(self(), msg_handler),
  {ok, #state{}}.


handle_call(Request, _From, State) ->
  io:format("BLOCK MESSAGE HANDLER: Unexpected call message: ~p~n", [Request]),
  {reply, ok, State}.


handle_cast({handle, Comm, From, Params}, State) ->
  block_naming_hnd:wait_service(path_ready),
  handle_message(Comm, From, Params),
  {noreply, State};

handle_cast(Request, State) ->
  io:format("BLOCK MESSAGE HANDLER: Unexpected cast message: ~p~n", [Request]),
  {noreply, State}.


handle_info(Info, State) ->
  io:format("BLOCK MESSAGE HANDLER: Unexpected ! message: ~p~n", [Info]),
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_message(add, _From, Params) ->
  {Name, Data} = Params,
  ID = block_filter:get_res_id(Name),
  {ok, Fd} = file:open(block_resource_handler:get_path(res) ++ Name, [write]),
  file:close(Fd),
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
  lists:map(fun(Res) -> handle_message(add, no_addr, Res) end, Resources);

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