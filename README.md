Eblock_rd
=====

An Erlang library that implements a block service for resource handling, implementing a Chord P2P system.
Using as an underlying layer the following Erlang library: https://github.com/Robyroc/Echo_rd

Build
-----

    $ rebar3 compile
    
Interface
-----

The library allows to handle the resources and the communication of a Chord node.

An example of an application using this library can be seen here: https://github.com/GiacomoZiffer/Echor_dfs, which is 
a distributed file system.

The full list of methods complete with a brief explanation can be seen in the [gen_dfs.erl](src/gen_dfs.erl) file.

Modules
-----

- **block_filter**: it will handle the communication with the underlying level (application manager) and receives calls
from the upper level;
- **block_message_handler**: it will handle the communication coming from the application manager,
(used for receive_command, add_many_resources, get_local_resources, drop_many_resources);
- **block_naming_hnd**: it will handle all naming operations;
- **block_naming_mng**: it will handle the naming table when naming_handler doesn't exist or is restarting;
- **block_request**: it will handle the communication with the upper level, responding to the function call;
- **block_resource_handler**: it will handle all the operation on the resources that are stored locally;

Configuration
-----

Eblock_rd can be configured using 

    $ application:set_env(eblock_rd, #VAR#, #VAL#).
    
The following table shows the possible configurations (in square brackets default values):

| #VAR#          | #VAL#               | Meaning                                                                                       |
|----------------|---------------------|-----------------------------------------------------------------------------------------------|
| nbits          | NUMBER              | Set the number of bits for the Chord network                                                  |
| nbits          | [undefined]         | Number of bits equals to 8 for the Chord network                                              |
| starting_port  | NUMBER ([6490])     | Set the starting port from which create new nodes (useful for *multiple parallel execution*)  |
| num_of_ports   | NUMBER ([10])       | Set the range of ports in which create new nodes (useful for *multiple parallel execution*)   |

