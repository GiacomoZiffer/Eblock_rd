%%%-------------------------------------------------------------------
%%% @author Giacomo
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2019 19:48
%%%-------------------------------------------------------------------
-module(block_filter).
-author("Giacomo").

-behaviour(gen_server).
-behaviour(gen_bm).

%% API
-export([start_link/0,
  start/0,
  start/1,
  leave/0,
  add/1,
  get_res/1,
  delete/1,
  update/1,
  pop/1,
  receive_command/2,
  add_many_resources/1,
  get_local_resources/0,
  drop_many_resources/1,
  safe_add/1,
  safe_delete/1]).

%%TODO remove this export
-export([encode_command/4, decode_command/2]).

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

start() ->
  application_manager:create(8).

start(Address) ->
  application_manager:join(Address).

leave() ->
  application_manager:leave().

add(Path) ->
  PID = block_naming_hnd:get_identity(filter),
  gen_server:call(PID, {add, Path}).

safe_add(Path) ->
  PID = block_naming_hnd:get_identity(filter),
  gen_server:call(PID, {safe_add, Path}).

get_res(Name) ->
  PID = block_naming_hnd:get_identity(filter),
  gen_server:call(PID, {ask_res, Name}).

delete(Name) ->
  PID = block_naming_hnd:get_identity(filter),
  gen_server:call(PID, {delete, Name}).

safe_delete(Name) ->
  PID = block_naming_hnd:get_identity(filter),
  gen_server:call(PID, {safe_delete, Name}).

update(Name) ->
  PID = block_naming_hnd:get_identity(filter),
  gen_server:call(PID, {update, Name}).

pop(Name) ->
  PID = block_naming_hnd:get_identity(filter),
  gen_server:call(PID, {pop, Name}).

receive_command(From, Command) ->
  PID = block_naming_hnd:get_identity(filter),
  gen_server:call(PID, {rcv_command, From, Command}).

add_many_resources(Resources) ->
  PID = block_naming_hnd:get_identity(filter),
  gen_server:call(PID, {add_many, Resources}).

get_local_resources() ->
  PID = block_naming_hnd:get_identity(filter),
  gen_server:call(PID, get_all).

drop_many_resources(From) ->
  PID = block_naming_hnd:get_identity(filter),
  gen_server:call(PID, {drop, From}).

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
  self() ! startup,
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
handle_call({add, Path}, _From, State) ->
  Name = block_resource_handler:get_name(Path),
  Data = block_resource_handler:get_data(Path),
  Command = encode_command(add, Name, Data, no_addr),
  Res = application_manager:issue_command(Name, Command),
  case Res of
    out_of_network ->
      {reply, out_of_network, State};
    _ ->
      {reply, ok, State}
  end;

handle_call({safe_add, Path}, From, State) ->
  Name = block_resource_handler:get_name(Path),
  Data = block_resource_handler:get_data(Path),
  Address = application_manager:get_own_address(), %TODO check this function
  Command = encode_command(safe_add, Name, Data, Address),
  Res = application_manager:issue_command(Name, Command),
  case Res of
    out_of_network ->
      {reply, out_of_network, State};
    _ ->
      block_r_gateway:add_request(Name, From, safe_add),
      {noreply, State}
  end;

handle_call({ask_res, Name}, From, State) ->
  Address = application_manager:get_own_address(), %TODO check this function
  Command = encode_command(ask_res, Name, no_data, Address),
  Res = application_manager:issue_command(Name, Command),
  case Res of
    out_of_network ->
      {reply, out_of_network, State};
    _ ->
      block_r_gateway:add_request(Name, From, ask_res),
      {noreply, State}
  end;

handle_call({delete, Name}, _From, State) ->
  Command = encode_command(delete, Name, no_data, no_addr),
  Res = application_manager:issue_command(Name, Command),
  case Res of
    out_of_network ->
      {reply, out_of_network, State};
    _ ->
      {reply, ok, State}
  end;

handle_call({safe_delete, Name}, From, State) ->
  Address = application_manager:get_own_address(), %TODO check this function
  Command = encode_command(safe_delete, Name, no_data, Address),
  Res = application_manager:issue_command(Name, Command),
  case Res of
    out_of_network ->
      {reply, out_of_network, State};
    _ ->
      block_r_gateway:add_request(Name, From, safe_delete),
      {noreply, State}
  end;

handle_call({rcv_command, From, Command}, _From, State) ->
  <<NumComm:8/integer, Msg/binary>> = Command,
  Comm = translate(NumComm),
  Params = decode_command(Comm, Msg),
  handle_msg(Comm, From, Params),
  {reply, ok, State};

handle_call(Request, _From, State) ->
  io:format("BLOCK FILTER: Unexpected call message: ~p~n", [Request]),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(Request, State) ->
  io:format("BLOCK FILTER: Unexpected cast message: ~p~n", [Request]),
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
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(startup, State) ->
  naming_handler:wait_service(application_manager),
  block_naming_hnd:notify_identity(self(), filter),
  application_manager:connect(?MODULE),
  {noreply, State};

handle_info(Info, State) ->
  io:format("BLOCK FILTER: Unexpected ! message: ~p~n", [Info]),
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
encode_command(add, Name, Data, no_addr) ->
  BinName = list_to_binary(Name),
  Length = byte_size(BinName),
  <<1:8/integer, Length:8/integer, BinName:Length/binary, Data/binary>>;

encode_command(safe_add, Name, Data, Address) ->
  {Port, {IpA, IpB, IpC, IpD}} = Address,
  BinName = list_to_binary(Name),
  Length = byte_size(BinName),
  <<5:8/integer, Port:16, IpA:8, IpB:8, IpC:8, IpD:8, Length:8/integer, BinName:Length/binary, Data/binary>>;

encode_command(safe_add_reply, Name, Result, no_addr) ->
  BinName = list_to_binary(Name),
  Length = byte_size(BinName),
  BinRes = list_to_binary(Result),
  <<8:8/integer, Length:8/integer, Name:Length/binary, BinRes/binary>>;

encode_command(ask_res, Name, no_data, Address) ->
  {Port, {IpA, IpB, IpC, IpD}} = Address,
  BinName = list_to_binary(Name),
  <<2:8/integer, Port:16, IpA:8, IpB:8, IpC:8, IpD:8, BinName/binary>>;

encode_command(res_reply, Name, Data, no_addr) ->
  BinName = list_to_binary(Name),
  Length = byte_size(BinName),
  <<3:8/integer, Length:8/integer, BinName:Length/binary, Data/binary>>;

encode_command(delete, Name, no_data, no_addr) ->
  BinName = list_to_binary(Name),
  <<4:8/integer, BinName/binary>>;

encode_command(safe_delete, Name, no_data, Address) ->
  {Port, {IpA, IpB, IpC, IpD}} = Address,
  BinName = list_to_binary(Name),
  <<6:8/integer, Port:16, IpA:8, IpB:8, IpC:8, IpD:8, BinName/binary>>;

encode_command(safe_delete_reply, Name, Result, no_addr) ->
  BinName = list_to_binary(Name),
  Length = byte_size(BinName),
  BinRes = list_to_binary(Result),
  <<7:8/integer, Length:8/integer, Name:Length/binary, BinRes/binary>>.

decode_command(add, Msg) ->
  <<Length:8/integer, Rest/binary>> = Msg,
  <<BinName:Length/binary, Data/binary>> = Rest,
  Name = binary_to_list(BinName),
  {Name, Data};

decode_command(safe_add, Msg) ->
  <<Port:16, IpA:8, IpB:8, IpC:8, IpD:8, Length:8/integer, Rest/binary>> = Msg,
  <<BinName:Length/binary, Data/binary>> = Rest,
  Name = binary_to_list(BinName),
  Address = {Port, {IpA, IpB, IpC, IpD}},
  {Name, Data, Address};

decode_command(safe_add_reply, Msg) ->
  <<Length:8/integer, Rest/binary>> = Msg,
  <<BinName:Length/binary, Result/binary>> = Rest,
  Name = binary_to_list(BinName),
  {Name, Result};

decode_command(ask_res, Msg) ->
  <<Port:16, IpA:8, IpB:8, IpC:8, IpD:8, Message/binary>> = Msg,
  Name = binary_to_list(Message),
  Address = {Port, {IpA, IpB, IpC, IpD}},
  {Name, Address};

decode_command(res_reply, Msg) ->
  <<Length:8/integer, Rest/binary>> = Msg,
  <<BinName:Length/binary, Data/binary>> = Rest,
  Name = binary_to_list(BinName),
  {Name, Data};

decode_command(delete, Msg) ->
  binary_to_list(Msg);

decode_command(safe_delete, Msg) ->
  <<Port:16, IpA:8, IpB:8, IpC:8, IpD:8, Message/binary>> = Msg,
  Name = binary_to_list(Message),
  Address = {Port, {IpA, IpB, IpC, IpD}},
  {Name, Address};

decode_command(safe_delete_reply, Msg) ->
  <<Length:8/integer, Rest/binary>> = Msg,
  <<BinName:Length/binary, Result/binary>> = Rest,
  Name = binary_to_list(BinName),
  {Name, Result}.

handle_msg(add, _From, Params) ->
  {Name, Data} = Params,
  block_resource_handler:add(Name, Data);

handle_msg(safe_add, _From, Params) ->
  {Name, Data, Address} = Params,
  Res = block_resource_handler:add(Name, Data),
  Msg = encode_command(safe_add_reply, Name, Res, no_addr),
  send_msg_back_to_address; %TODO send message back

handle_msg(safe_add_reply, From, Params) ->
  block_r_gateway:send_response(Params, safe_add);

handle_msg(ask_res, From, Params) ->
  {Name, Address} = Params,
  Data = block_resource_handler:get(Name),
  Msg = encode_command(res_reply, Name, Data, no_addr);     %%TODO send message back with name and data NAMED res_reply

handle_msg(res_reply, _From, Params) ->
  block_r_gateway:send_response(Params, ask_res);

handle_msg(delete, From, Name) ->
  block_resource_handler:delete(Name);

handle_msg(safe_delete, From, Params) ->
  {Name, Address} = Params,
  Res = block_resource_handler:delete(Name),
  Msg = encode_command(safe_delete_reply, Name, Res, no_addr),
  send_msg_back_to_address; %TODO send message back

handle_msg(safe_delete_reply, From, Params) ->
  block_r_gateway:send_response(Params, safe_delete).

translate(1) -> add;
translate(2) -> ask_res;
translate(3) -> res_reply;
translate(4) -> delete;
translate(5) -> safe_add;
translate(6) -> safe_delete;
translate(7) -> safe_delete_reply;
translate(8) -> safe_add_reply.