-module(block_request).
-author("Giacomo").

-behaviour(gen_server).

%% API
-export([start_link/3,
  respond/3]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {requested, from, method}).

%%%===================================================================
%%% API
%%%===================================================================

respond(PID, Params, Method) ->
  gen_server:cast(PID, {response, Params, Method}).


start_link(Requested, From, List) ->
  gen_server:start_link(?MODULE, [Requested, From, List], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Requested, From, Method]) ->
  {ok, #state{requested = Requested, from = From, method = Method}};

init(_) ->
  {stop, badarg}.


handle_call(Request, _From, State) ->
  io:format("BLOCK REQUEST: Unexpected call message: ~p~n", [Request]),
  {reply, ok, State}.


handle_cast({response, Params, ask_res}, State) ->
  {Name, R} = Params,
  case R of
    "no_file" -> gen_server:reply(State#state.from, {ask_res, Name, no_file});
    Data ->
      OutputPath = block_resource_handler:get_path(output),
      {ok, Fd} = file:open(OutputPath ++ Name, [write]),
      file:write(Fd, Data),
      gen_server:reply(State#state.from, {ask_res, Name, found})
  end,
  {stop, normal, State};

handle_cast({response, Params, safe_add}, State) ->
  {Name, Result} = Params,
  gen_server:reply(State#state.from, {safe_add, Name, list_to_atom(Result)}),
  {stop, normal, State};

handle_cast({response, Params, safe_delete}, State) ->
  {Name, Result} = Params,
  gen_server:reply(State#state.from, {safe_delete, Name, list_to_atom(Result)}),
  {stop, normal, State};

handle_cast({response, Params, pop}, State) ->
  {Name, R} = Params,
  case R of
    "no_file" -> gen_server:reply(State#state.from, {pop, Name, no_file});
    Data ->
      OutputPath = block_resource_handler:get_path(output),
      {ok, Fd} = file:open(OutputPath ++ Name, [write]),
      file:write(Fd, Data),
      block_filter:delete(Name),
      gen_server:reply(State#state.from, {pop, Name, found})
  end,
  {stop, normal, State};

handle_cast(Request, State) ->
  io:format("BLOCK REQUEST: Unexpected cast message: ~p~n", [Request]),
  {noreply, State}.


handle_info(Info, State) ->
  io:format("BLOCK REQUEST: Unexpected ! message: ~p~n", [Info]),
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
