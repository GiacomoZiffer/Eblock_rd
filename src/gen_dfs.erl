-module(gen_dfs).
-author("Giacomo").

-type ip_port() :: integer().
-type ip() :: {integer(), integer(), integer(), integer()}.
-type address() :: {ip_port(), ip()}.
-type successor_entry() :: {integer(), address()}.
-type successor_list() :: [] | [successor_entry() | successor_list()].

%%--------------------------------------------------------------------
%% @doc
%% Create a new Chord network with 8 number of bits as default
%% return
%%       ok -> when it successfully creates the network
%%       all_ports_are_already_used -> when no port is available to create a Chord network
%%
%% @see //echo_rd/application_manager:create/1
%% @see //echo_rd/application_manager:create_p/2
%%
%% @end
%%--------------------------------------------------------------------
-callback start() -> ok | all_ports_are_already_used.

%%--------------------------------------------------------------------
%% @doc
%% Join a Chord network through an address, with 6543 as the default port
%% where
%%      Address: is the address of a node already present in the Chord network
%% return
%%       ok -> when it successfully joins the network
%%       fail -> when it encounters some problem in the communication during the join the network
%%       used_id -> when the node's id is already present in the network
%%       connection_refused -> when the connection is refused
%%       address_not_reachable -> when the address is wrong or it doesn't belong to a Chord node
%%       all_ports_are_already_used -> when no port is available to join the network or
%%                                     when it encounters some problem in entering in the network
%%
%% @see //echo_rd/application_manager:join/1
%% @see //echo_rd/application_manager:join_p/2
%%
%% @end
%%--------------------------------------------------------------------
-callback start(Address :: address()) -> ok |
                                         fail |
                                         used_id |
                                         connection_refused |
                                         address_not_reachable |
                                         all_ports_are_already_used.

%%--------------------------------------------------------------------
%% @doc
%% Leave a Chord network
%% return
%%       ok -> when it successfully left the network
%%
%% @see //echo_rd/application_manager:leave/0
%%
%% @end
%%--------------------------------------------------------------------
-callback leave() -> ok.

%%--------------------------------------------------------------------
%% @doc
%% Add without any guarantee (cast mode) a new resource to the network
%% where
%%      Path: is the absolute path of the resource
%% return
%%       ok -> when it successfully sends the request to the network
%%       out_of_network -> when the node it hasn't joined a Chord network
%%
%% @end
%%--------------------------------------------------------------------
-callback add(Path :: string()) -> ok | out_of_network.

%%--------------------------------------------------------------------
%% @doc
%% Add with safe check (call mode) a new resource to the network
%% where
%%      Path: is the absolute path of the resource
%% return
%%       {safe_add, Name, ok} -> when it successfully stores the resource in the network
%%       {safe_add, Name, eexist} -> when the file is already present in the network
%%       {safe_add, Name, eacces} -> when permission for reading the file
%%        or searching one of the parent directories are missing
%%       {safe_add, Name, eisdir} -> when the named file is a directory
%%       {safe_add, Name, enotdir} -> when a component of the filename is not a directory
%%       {safe_add, Name, enospc} -> when there is no space left on the device
%%       out_of_network -> when the node it hasn't joined a Chord network
%%
%% @end
%%--------------------------------------------------------------------
-callback safe_add(Path :: string()) -> {safe_add, Name :: string(), ok} |
                                        {safe_add, Name :: string(), eexist} |
                                        {safe_add, Name :: string(), eacces} |
                                        {safe_add, Name :: string(), eisdir} |
                                        {safe_add, Name :: string(), enotdir} |
                                        {safe_add, Name :: string(), enospc} |
                                        out_of_network.

%%--------------------------------------------------------------------
%% @doc
%% Get a specific file present in the network
%% where
%%      Name: is the name of the resource
%% return
%%       {ask_res, Name, found} -> when it successfully finds the resource
%%       {ask_res, Name, no_file} -> when it doesn't find the resource
%%       out_of_network -> when the node it hasn't joined a Chord network
%%
%% @end
%%--------------------------------------------------------------------
-callback get_file(Name :: string()) -> {ask_res, Name :: string(), found} |
                                        {ask_res, Name :: string(), no_file} |
                                        out_of_network.

%%--------------------------------------------------------------------
%% @doc
%% Delete without any guarantee (cast mode) a resource to the network
%% where
%%      Path: is the absolute path of the resource
%% return
%%       ok -> when it successfully sends the request to the network
%%       out_of_network -> when the node it hasn't joined a Chord network
%%
%% @end
%%--------------------------------------------------------------------
-callback delete(Name :: string()) -> ok | out_of_network.

%%--------------------------------------------------------------------
%% @doc
%% Delete with safe check (call mode) a resource to the network
%% where
%%      Name: is the name of the resource
%% return
%%       {safe_delete, Name, ok} -> when it successfully deletes the resource in the network
%%       {safe_delete, Name, enoent} -> when the file doesn't exist in the network
%%       {safe_delete, Name, eacces} -> when permission for deleting the file
%%        or searching one of the parent directories are missing
%%       {safe_delete, Name, eperm} -> when the file is a directory and the user is not superuser
%%       {safe_delete, Name, einval} -> when there is no space left on the device
%%       out_of_network -> when the filename has an improper type, such as tuple
%%
%% @end
%%--------------------------------------------------------------------
-callback safe_delete(Name :: string()) ->  {safe_delete, Name :: string(), ok} |
                                            {safe_delete, Name :: string(), eexist} |
                                            {safe_delete, Name :: string(), eacces} |
                                            {safe_delete, Name :: string(), eisdir} |
                                            {safe_delete, Name :: string(), enotdir} |
                                            {safe_delete, Name :: string(), enospc} |
                                            out_of_network.

%%--------------------------------------------------------------------
%% @doc
%% Get a resource from the network, deleting it from the network
%% where
%%      Name: is the name of the resource
%% return
%%       {pop, Name, found} -> when it successfully finds the resource
%%       {pop, Name, no_file} -> when it doesn't find the resource
%%       out_of_network -> when the node it hasn't joined a Chord network
%%
%% @end
%%--------------------------------------------------------------------
-callback pop(Name :: string()) -> {pop, Name :: string(), found} |
                                    {pop, Name :: string(), no_file} | out_of_network.

%%--------------------------------------------------------------------
%% @doc
%% Get the id of a file's name
%% where
%%      Name: is the name of the file
%% return
%%       integer() -> the id of the file's name
%%
%% @see //echo_rd/application_manager:hash_name/0
%%
%% @end
%%--------------------------------------------------------------------
-callback get_file_id(Name :: string()) -> integer().

%%--------------------------------------------------------------------
%% @doc
%% Get the successor list of the node
%% return
%%       successor_list() -> the list of nodes present in the successor list
%%
%% @see //echo_rd/application_manager:get_successor_list/0
%%
%% @end
%%--------------------------------------------------------------------
-callback get_successor_list() -> successor_list().

%%--------------------------------------------------------------------
%% @doc
%% Print the finger tale of the node
%% return
%%        ok -> when the finger table has been printed.
%%
%% @see //echo_rd/application_manager:show_finger_table/0
%%
%% @end
%%--------------------------------------------------------------------
-callback show_finger_table() -> ok.

%%--------------------------------------------------------------------
%% @doc
%% Print all the resources locally stored in the node
%% return
%%        ok -> when the list of the resources has been printed.
%%
%% @end
%%--------------------------------------------------------------------
-callback show_local_files() -> ok.

%%--------------------------------------------------------------------
%% @doc
%% Get the predecessor of the node
%% return
%%       address() -> the address of the predecessor of the node
%%
%% @see //echo_rd/application_manager:get_predecessor/0
%%
%% @end
%%--------------------------------------------------------------------
-callback get_predecessor() -> address().

%%--------------------------------------------------------------------
%% @doc
%% Get the Chord id of the node
%% return
%%       integer() -> the Chord id of the node
%%
%% @see //echo_rd/application_manager:get_own_id/0
%%
%% @end
%%--------------------------------------------------------------------
-callback get_own_id() -> integer().

%%--------------------------------------------------------------------
%% @doc
%% Collect the statistics (Join time, Highest lookup time, Average lookup time,
%% Average lookup length, Number of lookup timeouts, FTable last refresh timings) of network
%% return
%%       ok -> all the statistics of the node have been collected and the message has been forwarded to the successor
%%
%% @see //echo_rd/application_manager:statistics_gather/0
%%
%% @end
%%--------------------------------------------------------------------
-callback get_statistics() -> ok.

%%--------------------------------------------------------------------
%% @doc
%% Run all the commands written in the file app.commands, using file:script/1
%% return
%%       ok -> when it runs all the commands
%%
%% @end
%%--------------------------------------------------------------------
-callback run_script() -> ok.