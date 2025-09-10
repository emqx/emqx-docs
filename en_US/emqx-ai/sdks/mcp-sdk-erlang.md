# Erlang SDK

This document demonstrates how to use the [MCP over MQTT Erlang SDK](https://github.com/emqx/mcp-mqtt-erl) to create a simple MCP over MQTT server and client.

## Example

### Creating a Simple MCP Client

```erlang
-module(mcp_mqtt_erl_client_demo).

-behaviour(mcp_mqtt_erl_client_session).

-export([
    client_name/0,
    client_version/0,
    client_capabilities/0,
    received_non_mcp_message/3
]).
-export([start_link/0]).

%% The client name, version, and capabilities. These details will be sent to the server during MCP initialization.
client_name() ->
    <<"emqx_tools/cli_demo">>.

client_version() ->
    <<"1.0">>.

client_capabilities() -> #{}.

%% Callback for non-MCP messages
received_non_mcp_message(MqttClient, Msg, State) ->
    io:format("~p Received non-MCP message: ~p~n", [MqttClient, Msg]),
    State.

%% Start the MCP over MQTT client
start_link() ->
    mcp_mqtt_erl_client:start_link(
        #{
            server_name_filter => <<"#">>,
            callback_mod => ?MODULE,
            broker_address => {"127.0.0.1", 1883},
            mqtt_options => #{
                clientid => <<"emqx_tools_cli_demo">>,
                username => <<"emqx">>,
                password => <<"public">>
            }
        }).
```

Here, `server_name_filter` is used to subscribe to the MQTT topic filter for MCP servers, and `mqtt_options` are options passed to the underlying MQTT client.

### Creating a Simple MCP Server

Below is a simple MCP server implementation that supports two tools: `tool1` and `tool2`.

```erlang
-module(mcp_mqtt_erl_server_demo).

-behaviour(mcp_mqtt_erl_server_session).

-include_lib("mcp_mqtt_erl/include/mcp_mqtt_erl_types.hrl").
-include_lib("mcp_mqtt_erl/include/emqx_mcp_tools.hrl").

-export([
    start_link/2
]).

-export([
    server_name/0,
    server_id/2,
    server_version/0,
    server_capabilities/0,
    server_instructions/0,
    server_meta/0
]).

-export([
    initialize/2,
    list_resources/1,
    read_resource/2,
    call_tool/3,
    list_tools/1
]).

-type loop_data() :: #{
    server_id => binary(),
    client_info => map(),
    client_capabilities => map(),
    mcp_client_id => binary(),
    _ => any()
}.

-spec start_link(integer(), mcp_mqtt_erl_server:config()) -> gen_statem:start_ret().
start_link(Idx, Conf) ->
    mcp_mqtt_erl_server:start_link(Idx, Conf).

server_version() ->
    <<?PLUGIN_VSN>>.

server_name() ->
    <<"emqx_tools/info_apis">>.

server_id(ClientIdPrefix, Idx) ->
    Idx1 = integer_to_binary(Idx),
    Node = atom_to_binary(node()),
    <<ClientIdPrefix/binary, ":", Node/binary, ":", Idx1/binary>>.

server_capabilities() ->
    #{
        resources => #{
            subscribe => true,
            listChanged => true
        },
        tools => #{
            listChanged => true
        }
    }.

server_instructions() ->
    <<"">>.

server_meta() ->
    #{
        authorization => #{
            roles => [<<"admin">>, <<"user">>]
        }
    }.

-spec initialize(binary(), client_params()) -> {ok, loop_data()}.
initialize(ServerId, #{client_info := ClientInfo, client_capabilities := Capabilities, mcp_client_id := McpClientId}) ->
    io:format("initialize --- server_id: ~p, client_info: ~p, client_capabilities: ~p, mcp_client_id: ~p~n", [ServerId, ClientInfo, Capabilities, McpClientId]),
    {ok, #{
        server_id => ServerId,
        client_info => ClientInfo,
        client_capabilities => Capabilities,
        mcp_client_id => McpClientId
    }}.

-spec call_tool(binary(), map(), loop_data()) -> {ok, call_tool_result() | [call_tool_result()], loop_data()}.
call_tool(ToolName, Args, LoopData) ->
    io:format("call_tool --- tool_name: ~p, args: ~p~n", [ToolName, Args]),
    Result = #{
        type => text,
        text => <<"This is the result of the tool call">>
    },
    {ok, Result, LoopData}.

-spec list_tools(loop_data()) -> {ok, [tool_def()], loop_data()}.
list_tools(LoopData) ->
    io:format("list_tools --- ~n", []),
    Tools = [
        #{
            name => <<"tool1">>,
            description => <<"This is tool 1">>,
            inputSchema => #{
                type => <<"object">>,
                properties => #{
                    arg1 => #{
                        type => <<"string">>,
                        description => <<"Argument 1">>
                    },
                    arg2 => #{
                        type => <<"integer">>,
                        description => <<"Argument 2">>
                    }
                }
            }
        },
        #{
            name => <<"tool2">>,
            description => <<"This is tool 2">>,
            inputSchema => #{
                type => <<"object">>,
                properties => #{
                    arg1 => #{
                        type => <<"string">>,
                        description => <<"Argument 1">>
                    },
                    arg2 => #{
                        type => <<"boolean">>,
                        description => <<"Argument 2">>
                    }
                }
            }
        }
    ],
    {ok, Tools, LoopData}.
```
