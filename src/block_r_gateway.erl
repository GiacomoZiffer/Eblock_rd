-module(block_r_gateway).
-author("Giacomo").

-behaviour(gen_server).

%% API
-export([start_link/0,
  add_request/3,
  send_response/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {requests}).

%%%===================================================================
%%% API
%%%===================================================================

add_request(Requested, From, Method) ->
  PID = block_naming_hnd:get_identity(block_r_gateway),
  gen_server:call(PID, {add, Requested, From, Method}).

send_response(Params, Method) ->
  PID = block_naming_hnd:get_identity(block_r_gateway),
  gen_server:cast(PID, {response, Params, Method}).


start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  self() ! startup,
  {ok, #state{}}.


handle_call({add, Requested, From, Method}, _From, State) ->
  Sup = block_naming_hnd:get_identity(block_r_sup),
  Ret = supervisor:start_child(Sup, [Requested, From, Method]),
  case Ret of
    {ok, PID} ->
      Monitor = erlang:monitor(process, PID),
      {reply, ok, #state{requests = [{PID, Requested, Method, Monitor} | State#state.requests]}};
    {ok, PID, _} ->
      Monitor = erlang:monitor(process, PID),
      {reply, ok, #state{requests = [{PID, Requested, Method, Monitor} | State#state.requests]}}
  end;

handle_call(Request, _From, State) ->
  io:format("BLOCK GATEWAY: Unexpected call message: ~p~n", [Request]),
  {reply, ok, State}.


handle_cast({response, Params, ask_res}, State) ->
  {Name, _} = Params,
  [block_request:respond(PID, Params, ask_res) || {PID, R, M, _} <- State#state.requests, R =:= Name, M =:= ask_res],
  {noreply, State};

handle_cast({response, Params, safe_add}, State) ->
  {Name, _} = Params,
  [block_request:respond(PID, Params, safe_add) || {PID, R, M, _} <- State#state.requests, R =:= Name, M =:= safe_add],
  {noreply, State};

handle_cast({response, Params, safe_delete}, State) ->
  {Name, _} = Params,
  [block_request:respond(PID, Params, safe_delete) || {PID, R, M, _} <- State#state.requests, R =:= Name, M =:= safe_delete],
  {noreply, State};

handle_cast({response, Params, pop}, State) ->
  {Name, _} = Params,
  [block_request:respond(PID, Params, pop) || {PID, R, M, _} <- State#state.requests, R =:= Name, M =:= pop],
  {noreply, State};

handle_cast(Request, State) ->
  io:format("BLOCK GATEWAY: Unexpected cast message: ~p~n", [Request]),
  {noreply, State}.


handle_info(startup, _State) ->
  block_naming_hnd:notify_identity(self(), block_r_gateway),
  {noreply, #state{requests = []}};

handle_info({'DOWN', Monitor, process, _PID, normal}, State) ->
  {noreply, #state{requests = [{PID, Req, Meth, Mon} || {PID, Req, Meth, Mon} <- State#state.requests, Mon =/= Monitor]}};

handle_info({'DOWN', Monitor, process, _PID, Reason}, State) ->
  Present = [X || {_, X, _, M} <- State#state.requests, M =:= Monitor],
  io:format("BLOCK GATEWAY: A request failed: Requested: ~p~nReason: ~p~n", [hd(Present), Reason]),
  {noreply, #state{requests = [{PID, Req, Meth, Mon} || {PID, Req, Meth, Mon} <- State#state.requests, Mon =/= Monitor]}};

handle_info(Info, State) ->
  io:format("BLOCK GATEWAY: Unexpected ! message: ~p~n", [Info]),
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
