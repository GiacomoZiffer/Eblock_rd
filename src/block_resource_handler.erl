-module(block_resource_handler).
-author("Giacomo").

-behaviour(gen_server).

%% API
-export([start_link/0,
  add/3,
  get/1,
  delete/1,
  get_name/1,
  get_data/1,
  drop/1,
  get_many/1,
  show_res/0,
  notify_path/2,
  get_path/1,
  safe_add/3]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {res_path, output_path, resources}).

%%%===================================================================
%%% API
%%%===================================================================

add(Name, ID, Data) ->
  PID = block_naming_hnd:get_identity(resource_handler),
  gen_server:call(PID, {add, Name, ID, Data}).

safe_add(Name, ID, Data) ->
  PID = block_naming_hnd:get_identity(resource_handler),
  gen_server:call(PID, {safe_add, Name, ID, Data}).

get(Name) ->
  PID = block_naming_hnd:get_identity(resource_handler),
  gen_server:call(PID, {get, Name}).

delete(Name) ->
  PID = block_naming_hnd:get_identity(resource_handler),
  gen_server:call(PID, {delete, Name}).

drop(From) ->
  PID = block_naming_hnd:get_identity(resource_handler),
  gen_server:call(PID, {drop, From}).

get_name(Path) ->
  [Name] = tl(string:split(Path, "/", trailing)),
  Name.

get_data(Path) ->
  {ok, Data} = file:read_file(Path),
  Data.

get_many(ID) ->
  PID = block_naming_hnd:get_identity(resource_handler),
  gen_server:call(PID, {get_many, ID}).

show_res() ->
  PID = block_naming_hnd:get_identity(resource_handler),
  gen_server:call(PID, show_res).

notify_path(Type, Path) ->
  PID = block_naming_hnd:get_identity(resource_handler),
  gen_server:call(PID, {notify_path, Type, Path}).

get_path(Type) ->
  PID = block_naming_hnd:get_identity(resource_handler),
  gen_server:call(PID, {get_path, Type}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  block_naming_hnd:notify_identity(self(), resource_handler),
  {ok, #state{resources = []}}.


handle_call({add, Name, ID, Data}, _From, State) ->
  try
    %{ok, Fd} = file:open(State#state.res_path ++ Name, [write]),
    %file:write(Fd, Data),
    %file:close(Fd)
    io:format("The output is: ~p~n", [file:write_file(State#state.res_path ++ Name, Data)])
  of
    _ ->
      io:format("^v^v^v^v^ RESOURCE HANDLER ^v^v^v^v^ The size is: ~p B~n", [byte_size(Data)]),
      ResList = State#state.resources,
      AdjList = [{N, ResID} || {N, ResID} <- ResList, N =/= Name],
      NewList = [{Name, ID} | AdjList],
      {reply, ok, State#state{resources = NewList}}
  catch _:_ ->
    io:format("Error while adding file, ~p~n", [Name]),
    {reply, error, State}
  end;

handle_call({safe_add, Name, ID, Data}, _From, State) ->
  case file:open(State#state.res_path ++ Name, [exclusive]) of
    {ok, Fd} ->
      try
        file:write(Fd, Data),
        file:close(Fd)
      of
        _ ->
          io:format("^v^v^v^v^ RESOURCE HANDLER ^v^v^v^v^ The size is: ~p B~n", [byte_size(Data)]),
          ResList = State#state.resources,
          NewList = [{Name, ID} | ResList],
          {reply, ok, State#state{resources = NewList}}
      catch Error ->
        {reply, Error, State}
      end;
    {error, Error} ->
      {reply, Error, State}
  end;

handle_call({get, Name}, _From, State) ->
  Response = file:read_file(State#state.res_path ++ Name),
  {reply, Response, State};

handle_call({delete, Name}, _From, State) ->
  case file:delete(State#state.res_path ++ Name) of
    ok ->
      NewList = [{N, ID} || {N, ID} <- State#state.resources, N =/= Name],
      {reply, ok, State#state{resources = NewList}};
    {error, Reason} ->
      {reply, Reason, State}
  end;

handle_call({drop, all_res}, _From, State) ->
  ResList = State#state.resources,
  [file:delete(State#state.res_path ++ Name) || {Name, _} <- ResList],
  {reply, ok, State#state{resources = []}};

handle_call({drop, From}, _From, State) ->
  ResList = State#state.resources,
  [file:delete(State#state.res_path ++ Name) || {Name, ID} <- ResList, ID =< From],
  NewList = [{N, ID} || {N, ID} <- ResList, ID > From],
  {reply, ok, State#state{resources = NewList}};

handle_call({get_many, all_res}, _From, State) ->
  ResList = State#state.resources,
  Resources = [{Name, get_data(State#state.res_path ++ Name)} || {Name, _} <- ResList],
  {reply, Resources, State};

handle_call({get_many, ID}, _From, State) ->
  ResList = State#state.resources,
  Resources = [{Name, get_data(State#state.res_path ++ Name)} || {Name, ResID} <- ResList, ResID =< ID],
  io:format("Name of resources: ~p~n", [[Name || {Name, _} <- Resources]]),
  {reply, Resources, State};

handle_call({notify_path, res, Path}, _From, State) ->
  {reply, ok, State#state{res_path = Path}};

handle_call({notify_path, output, Path}, _From, State) ->
  {reply, ok, State#state{output_path = Path}};

handle_call({get_path, res}, _From, State) ->
  {reply, State#state.res_path, State};

handle_call({get_path, output}, _From, State) ->
  {reply, State#state.output_path, State};

handle_call(show_res, _From, State) ->
  io:format("@@@@@ Resources: @@@@@ ~p~n", [State#state.resources]),
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast(_Request, State) ->
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
