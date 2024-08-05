# Command Line Interface

This page introduces all kinds of startup and administrative commands supported by EMQX and gives detailed introductions to ctl administrative commands.

## Startup Commands

EMQX supports some basic startup and administrative commands, which can be executed using the `emqx <command>` command.

Here are some commonly used startup and administrative commands:

| Command    | Description                                                  |
| ---------- | ------------------------------------------------------------ |
| start      | Start EMQX in daemon mode, without requiring an interactive shell during runtime. |
| console    | Start EMQX in Erlang or Elixir interactive shell. Used for debugging EMQX in a development environment, requiring interaction with EMQX. |
| foreground | Start EMQX in foreground mode, without using an interactive shell. Used to start EMQX in a development environment without running it in the background. |
| stop       | Stop the running EMQX node.                                  |
| ctl        | Manage and monitors EMQX. Executing `emqx ctl help` can get more detailed information. |

The following are advanced commands for development and debugging, and ordinary users usually don't need to care about them:

| Command        | Description                                                  |
| -------------- | ------------------------------------------------------------ |
| remote_console | Connect to the interactive shell of a remote EMQX node.      |
| attach         | Attach to a running EMQX node to perform interactive operations. |
| ertspath       | Retrieve the path of the EMQX Erlang library.                |
| root_dir       | Retrieve the path of the EMQX root directory.                |
| pid            | Retrieve the process ID of the running EMQX node.            |
| ping           | Check if the EMQX node is running.                           |
| check_config   | Validate if the EMQX configuration file is correct.          |
| console_clean  | Clear the output of the interactive shell console.           |
| escript        | Execute an Escript script on the EMQX node.                  |

## The ctl Commands

The EMQX `ctl` command provides multiple subcommands for managing and monitoring EMQX. The `ctl` command needs to be run after the EMQX service is started.

> EMQX also provides `emqx_ctl` command, which is an alias of `emqx ctl`.
> The `ctl` command remotely connects to the specified EMQX node by starting a hidden Erlang node, executes an Erlang remote call, and then prints the returned result. Therefore, it is advised to avoid excessive usage of the `ctl` command.

Below is a list of all the subcommands of the `ctl` command along with their brief descriptions. This section aims to introduce the functionality of the commands, while detailed parameter information can be viewed using the `help` command.

## status

This command is a quick inspection to see if the broker is up and running.

```bash
$ emqx ctl status
Node 'emqx@127.0.0.1' 5.0.3 is started
```

## broker

This command is to inspect the local broker running status, statistics and metrics.

```bash
$ emqx ctl broker
sysdescr  : EMQX Enterprise
version   : 5.0.3
datetime  : 2023-05-12T10:21:50.095047713+08:00
uptime    : 52 seconds
```

## observer

This command provides Erlang virtual machine insights including a realtime view like linux's `top` command. Subcommands are as follows:

| Command           | Description                                                  |
| ----------------- | ------------------------------------------------------------ |
| observer status   | Launch the observer in the current console, used to monitor and debug the status and activities of the EMQX node. |
| observer bin_leak | Force all processes to perform garbage collection and prints the top 100 processes that release the maximum amount of binary data, potentially revealing potential memory leak issues. |
| observer load Mod | Ensure that the specified module is loaded on all nodes in the EMQX cluster. This command can be used to load modules when it is necessary to ensure that they are available throughout the entire cluster. |

### observer status

```bash
emqx ctl observer status
```

### observer bin_leak

```bash
$ emqx ctl observer bin_leak
{<0.2140.0>,-48,
 [{current_function,{logger_std_h,file_ctrl_loop,1}},
  {initial_call,{erlang,apply,2}}]}
{<0.2093.0>,-29,
 [{current_function,{application_master,main_loop,2}},
  {initial_call,{proc_lib,init_p,5}}]}
{<0.2116.0>,-23,
 [user_drv,
  {current_function,{user_drv,server_loop,6}},
  {initial_call,{user_drv,server,2}}]}
...
```

### observer load Mod

```bash
$ emqx ctl observer load Mod
Loaded 'Mod' module on []: ok
```

## conf cluster_sync

This command is mostly for troubleshooting when there is something wrong with cluster-calls used to sync configuration changes between the nodes in the cluster. 

::: tip 

In EMQX 5.0.x, this command was named `cluster_call`. This old command is still available in EMQX 5.1 but it is not displayed in the help information.

:::

EMQX HTTP API can be used to modify many configurations. When an API is called, for example, through operations in the Dashboard, the node receiving the request first writes the modified content locally to `data/configs/cluster.hocon`. Then, the same operation is recorded in the database and asynchronously forwarded to other nodes in the cluster.

If for some reason, this replication can not apply in a peer node, this command can be used to inspect and even fix the replication so it can move forward.

EMQX generates an ID (tnxid) for each configuration modification within the cluster scope. This ID strictly increases within the cluster scope, and every modification, such as changing a configuration from the Dashboard, is recorded in the database. The following example shows viewing the content of the second modification (tnxid=2), which is an operation to enable a TLS listener.

```bash
$ emqx ctl conf cluster_sync inspect 2
{atomic,#{created_at => {{2022,6,21},{21,57,50}},
          initiator => 'emqx@127.0.0.1',
          mfa =>
              {emqx,update_config,
                    [[listeners,ssl,default],
                     {action,stop,#{<<"enabled">> => false}},
                     #{override_to => cluster,rawconf_with_defaults => true}]},
          tnx_id => 2}}
```

::: tip
The `skip` and `fast_forward` commands may result in diverged configs between the nodes in the cluster.
:::

## admins

The `admins` command can be used to create, update or delete administrative users. It has the following subcommands:

| Command                                        | Description                                       |
| ---------------------------------------------- | ------------------------------------------------- |
| admins add \<Username> \<Password> \<Description> | Add a Dashboard user.                             |
| admins passwd \<Username> \<Password>            | Reset the password for a specific Dashboard user. |
| admins del \<Username>                          | Delete a specific Dashboard user.                 |

### admins add \<Username> \<Password> \<Description>

```bash
$ emqx ctl admins add emqx_u EMQemq@1172
ok
```

### admins passwd \<Username> \<Password>

```bash
$ emqx ctl admins passwd emqx_u EMQemq@11721
ok
```

### admins del \<Username>

```bash
$ emqx ctl admins del emqx_u
ok
```

## retainer

The `retainer` command can be used to inspect or manage retained messages. It also comes with a `emqx ctl retainer reindex` command which can be used to create or update indices for retained messages.

| Command                        | Description                                                  |
| ------------------------------ | ------------------------------------------------------------ |
| retainer info                  | Display the number of retained messages.                     |
| retainer topics                | Display all topics with retained messages.                   |
| retainer clean                 | Clear all retained messages.                                 |
| retainer clean \<Topic>         | Clear retained messages according to the specific topic filter. |
| retainer reindex status        | Display the status of reindexing process.                    |
| retainer reindex start [force] | Generate a new index for retained message topics based on the configuration settings. Pass `true` as the `<force>` parameter to ignore any previously started reindexing process. |

### retainer info

```bash
$ emqx ctl retainer info
Number of retained messages: 3
```

### retainer topics

```bash
$ emqx ctl retainer topics
$SYS/brokers
$SYS/brokers/emqx@127.0.0.1/sysdescr
$SYS/brokers/emqx@127.0.0.1/version
```

### retainer clean

```bash
emqx ctl retainer clean
```

### retainer clean \<Topic>

```bash
emqx ctl retainer clean t/1
```

### retainer reindex status

```bash
$ emqx ctl retainer reindex status
Reindexing is not running
```

### retainer reindex start [force]

```bash
$ emqx ctl retainer reindex start true
Starting reindexing
Reindexed 0 messages
Reindexing finished
```


## cluster

This command is used to view and manage the cluster status of nodes.

Note that the `join` command in EMQX for joining a cluster is a "request" sent to the node specified in the parameters, rather than an "invitation". In other words, the command `emqx ctl cluster join <OneOfTheClusteredNodes>` is used to send a request to join the cluster of the node specified by `<OneOfTheClusteredNodes>`, rather than having that node join its own cluster.

| Command                      | Description                                | Use cases and Considerations                                 |
| ---------------------------- | ------------------------------------------ | ------------------------------------------------------------ |
| emqx ctl cluster             | Command for cluster control of EMQX.       |                                                              |
| cluster join \<Node\>        | Join a cluster.                            | - Use this command to join a node to the EMQX cluster where the specified node is located.<br />- Ensure that the specified node is active and accessible. |
| cluster leave                | Leave the cluster.                         | - Use this command to remove the node from the current EMQX cluster. |
| cluster force-leave \<Node\> | Forcefully remove a node from the cluster. | - Use this command to forcefully remove the specified node from the EMQX cluster.<br />- Note that this operation may cause cluster state inconsistency, so use it with caution. |
| cluster status [--json]      | View the cluster status.                   | - Use this command to view the status of the EMQX cluster.<br />- The optional `--json` parameter displays the cluster status in JSON format.<br />- Useful for monitoring and debugging the health of the cluster. |

### cluster join \<Node\>

```bash
$ emqx ctl cluster join emqx2@127.0.0.1
Failed to join the cluster: {node_down,'emqx2@127.0.0.1'}
```

### cluster leave

```bash
$ emqx ctl cluster leave
Failed to leave the cluster: node_not_in_cluster
```

### cluster force-leave \<Node\>

```bash
$ emqx ctl cluster force-leave emqx2@127.0.0.1
Failed to remove the node from cluster: node_not_in_cluster
```

### cluster status [--json]

```bash
$ emqx ctl cluster status
Cluster status: #{running_nodes => ['emqx@127.0.0.1'],stopped_nodes => []}
```

```bash
$ emqx ctl cluster status --json
{
  "stopped_nodes" : [

  ],
  "running_nodes" : [
    "emqx@127.0.0.1"
  ]
}
```

## clients

This command is to view and manage connected clients.

| Command                   | Description                                                  |
| ------------------------- | ------------------------------------------------------------ |
| clients list              | View all clients currently connected to EMQX. This command can be used to monitor active clients and the number of connections. |
| clients show \<ClientId\> | View detailed connection information for a specific client.  |
| clients kick \<ClientId\> | Forcefully disconnect a specified client.                    |

### emqx ctl clients list

```bash
$ emqx ctl clients list
Client(emqx_c, username=undefined, peername=127.0.0.1:59441, clean_start=true, keepalive=60, session_expiry_interval=0, subscriptions=1, inflight=0, awaiting_rel=0, delivered_msgs=4530, enqueued_msgs=0, dropped_msgs=0, connected=true, created_at=1684736435155, connected_at=1684736435155)
Client(emqx_a, username=undefined, peername=127.0.0.1:59444, clean_start=true, keepalive=60, session_expiry_interval=0, subscriptions=1, inflight=0, awaiting_rel=0, delivered_msgs=4588, enqueued_msgs=0, dropped_msgs=0, connected=true, created_at=1684736441613, connected_at=1684736441613)
```

### emqx ctl clients show \<ClientId\>

```bash
$ emqx ctl clients show emqx_c
Client(emqx_c, username=undefined, peername=127.0.0.1:59441, clean_start=true, keepalive=60, session_expiry_interval=0, subscriptions=1, inflight=0, awaiting_rel=0, delivered_msgs=4680, enqueued_msgs=0, dropped_msgs=0, connected=true, created_at=1684736435155, connected_at=1684736435155)
```

### emqx ctl clients kick \<ClientId\>

```bash
$ emqx ctl clients kick emqx_c
ok
```

:::tip
If a large number of clients are connected to the system, the `list` command may be time-consuming and resource-intensive.
:::


## topics

This command is to view all subscribed topics in current system.

| Command               | Description                                                  |
| --------------------- | ------------------------------------------------------------ |
| topics list           | List all topics. This command can be used to monitor the number and distribution of topics. |
| topics show \<Topic\> | Show detailed information about a specific topic.            |

### topics list

```bash
$ emqx ctl topics list
t/1 -> emqx@127.0.0.1
```

### topics show \<Topic\>

```bash
$ emqx ctl topics show t/1
t/1 -> emqx@127.0.0.1
```

:::tip
If there are a large number of topic subscriptions in the cluster, the `list` command may be time-consuming and resource-intensive.
:::

## subscriptions

This command is to view, add or delete clients' subscriptions.

| Command                                          | Description                               |
| ------------------------------------------------ | ----------------------------------------- |
| subscriptions list                               | List all subscriptions.                   |
| subscriptions show \<ClientId\>                  | Show subscriptions for a specific client. |
| subscriptions add \<ClientId\> \<Topic\> \<QoS\> | Mannually add a subscription.             |
| subscriptions del \<ClientId\> \<Topic>          | Manually remove a subscription.           |

### subscriptions list

```bash
$ emqx ctl subscriptions list
emqx_a -> topic:t/1 qos:0 nl:0 rh:0 rap:0
emqx_c -> topic:t/1 qos:0 nl:0 rh:0 rap:0
```

### subscriptions show \<ClientId\>

```bash
$ emqx ctl subscriptions show emqx_a
emqx_a -> topic:t/1 qos:0 nl:0 rh:0 rap:0
```

### subscriptions add \<ClientId\> \<Topic\> \<QoS\>

```bash
$ emqx ctl subscriptions add emqx_a t/1 1
ok
```

### subscriptions del \<ClientId\> \<Topic\>

```bash
$ emqx ctl subscriptions del emqx_a t/1
ok
```

:::tip
When there are a large number of subscriptions in the system, the `list` command may be time-consuming and resource-intensive.
:::

## plugins

This command is used to view and manage plugin installation.

| Command                                  | Description                                                  |
| ---------------------------------------- | ------------------------------------------------------------ |
| plugins list                             | List all installed plugins.                                  |
| plugins describe \<Name-Vsn\>            | Display detailed information about an installed plugin.      |
| plugins install \<Name-Vsn\>             | Install a plugin package that is located in the plugin installation directory. |
| plugins uninstall \<Name-Vsn\>           | Uninstall a specified plugin.                                |
| plugins start \<Name-Vsn\>               | Start a specified plugin.                                    |
| plugins stop \<Name-Vsn\>                | Stop a specified plugin.                                     |
| plugins restart \<Name-Vsn\>             | Restart a specified plugin.                                  |
| plugins disable \<Name-Vsn\>             | Disable automatic startup of a plugin.                       |
| plugins enable \<Name-Vsn\> \[Position\] | Enable automatic startup of a plugin and specify the startup position. |

### plugins list

```bash
emqx ctl plugins list
```

### plugins describe \<Name-Vsn\>

```bash
emqx ctl plugins describe emqx_auth_mnesia-3.0.1
```

### plugins install \<Name-Vsn\>

```bash
emqx ctl plugins install emqx_auth_mnesia-3.0.1
```

### plugins uninstall \<Name-Vsn\>

```bash
emqx ctl plugins uninstall emqx_auth_mnesia-3.0.1
```

### plugins start \<Name-Vsn\>

```bash
emqx ctl plugins start emqx_auth_mnesia-3.0.1
```

### plugins stop \<Name-Vsn\>

```bash
emqx ctl plugins stop emqx_auth_mnesia-3.0.1
```

### plugins restart \<Name-Vsn\>

```bash
emqx ctl plugins restart emqx_auth_mnesia-3.0.1
```

### plugins disable \<Name-Vsn\>

```bash
emqx ctl plugins disable emqx_auth_mnesia-3.0.1
```

### plugins enable \<Name-Vsn\> \[Position\]

```bash
emqx ctl plugins enable emqx_auth_mnesia-3.0.1 front
```

You can use `front`, `rear`, or `before Other-Vsn` to specify a relative position for adjusting the startup order. If no Position is provided, the configured plugins will remain in their original positions, and the new plugin will be appended at the end.

## vm

Inspect statistic data collected from the Erlang virtual machine.

```bash
$ emqx ctl vm
cpu/load1               : 13.16
cpu/load5               : 11.95
cpu/load15              : 9.75
memory/total            : 127648904
memory/processes        : 30427456
memory/processes_used   : 30426744
memory/system           : 97221448
memory/atom             : 2277809
memory/atom_used        : 2259843
memory/binary           : 668072
memory/code             : 48748792
memory/ets              : 10725432
process/limit           : 2097152
process/count           : 626
io/max_fds              : 8192
io/active_fds           : 0
ports/count             : 27
ports/limit             : 1048576
```

## mnesia

This command is used to view the running status and metrics of the built-in database (Mnesia).

```bash
$ emqx ctl mnesia
===> System info in version "4.20.4.1", debug level = none <===
opt_disc. Directory "/Users/emqx/Downloads/emqx-503/data/mnesia/emqx@127.0.0.1" is used.
use fallback at restart = false
running db nodes   = ['emqx@127.0.0.1']
stopped db nodes   = []
master node tables = []
backend types      = null_copies    - mria_mnesia_null_storage
                     rocksdb_copies - mnesia_rocksdb
remote             = []
ram_copies         = [bpapi,emqx_channel_registry,
                      emqx_ee_schema_registry_serde_tab,
                      emqx_exclusive_subscription,
                      emqx_gateway_coap_channel_registry,emqx_retainer_index,
                      emqx_retainer_index_meta,emqx_retainer_message,
                      emqx_route,emqx_routing_node,emqx_shared_subscription,
                      emqx_trie,mria_schema]
disc_copies        = [cluster_rpc_commit,cluster_rpc_mfa,emqx_acl,
                      emqx_activated_alarm,emqx_admin,emqx_admin_jwt,emqx_app,
                      emqx_authn_mnesia,emqx_banned,emqx_dashboard_monitor,
                      emqx_deactivated_alarm,emqx_delayed,
                      emqx_enhanced_authn_scram_mnesia,emqx_psk,
                      emqx_telemetry,emqx_trace,schema]
disc_only_copies   = []
[{'emqx@127.0.0.1',disc_copies}] = [schema,emqx_psk,emqx_delayed,emqx_app,
                                    emqx_admin_jwt,emqx_dashboard_monitor,
                                    emqx_admin,cluster_rpc_mfa,
                                    cluster_rpc_commit,emqx_acl,
                                    emqx_enhanced_authn_scram_mnesia,
                                    emqx_authn_mnesia,emqx_banned,
                                    emqx_activated_alarm,
                                    emqx_deactivated_alarm,emqx_telemetry,
                                    emqx_trace]
[{'emqx@127.0.0.1',ram_copies}] = [mria_schema,emqx_trie,
                                   emqx_shared_subscription,emqx_routing_node,
                                   emqx_route,emqx_exclusive_subscription,
                                   bpapi,emqx_channel_registry,
                                   emqx_retainer_index_meta,
                                   emqx_retainer_message,emqx_retainer_index,
                                   emqx_ee_schema_registry_serde_tab,
                                   emqx_gateway_coap_channel_registry]
414 transactions committed, 32 aborted, 6 restarted, 250 logged to disc
0 held locks, 0 in queue; 0 local transactions, 0 remote
0 transactions waits for other nodes: []
```

## log

This command can be used to manage log handlers states, such as setting logging level etc.

| Command                                        | Description                                                  |
| ---------------------------------------------- | ------------------------------------------------------------ |
| log set-level \<Level\>                        | Set the overall log level.                                   |
| log primary-level                              | Show the current primary log level. `primary-level` represents the primary log level of EMQX, which is used to specify the default log level for the entire system. Setting `primary-level` will affect all log outputs unless specific log handlers have their own independent log levels. |
| log primary-level \<Level\>                    | Set the primary log level.                                   |
| log handlers list                              | Show the log handlers. `handlers` refer to the collection of log handlers used for handling logs. Each log handler can set its own log level independently and define how to handle and store log messages. |
| log handlers start \<HandlerId\>               | Start a specific handler.                                    |
| log handlers stop \<HandlerId\>                | Stop a specific handler。                                    |
| log handlers set-level \<HandlerId\> \<Level\> | Set the log level for a specific handler.                    |

### log set-level \<Level\>

```bash
$ emqx ctl log set-level debug
debug
```

### log primary-level

```bash
$ emqx ctl log primary-level
debug
```

### log primary-level \<Level\>

```bash
$ emqx ctl log primary-level info
info
```

### log handlers list

```bash
$ emqx ctl log handlers list
LogHandler(id=ssl_handler, level=debug, destination=console, status=started)
LogHandler(id=console, level=debug, destination=console, status=started)
```

### log handlers start \<HandlerId\>

```bash
$ emqx ctl log handlers start console
log handler console started
```

### log handlers stop \<HandlerId\>

```bash
$ emqx ctl log handlers stop console
log handler console stopped
```

### log handlers set-level \<HandlerId\> \<Level>

```bash
$ emqx ctl log handlers set-level console debug
debug
```

## trace

This command is used to trace (and log) events of a given client or topic etc.

| Command                                              | Description                                     |
| ---------------------------------------------------- | ----------------------------------------------- |
| trace list                                           | List all traces started on the local node.      |
| trace start client \<ClientId\> \<File\> [\<Level\>] | Start tracing for a specific client.            |
| trace stop client \<ClientId\>                       | Stop tracing for a specific client.             |
| trace start topic \<Topic\> \<File\> [\<Level\>]     | Start tracing for a specific topic.             |
| trace stop topic \<Topic\>                           | Stop tracing for a specific topic.              |
| trace start ip_address \<IP\> \<File\> [\<Level\>]   | Start tracing for a specific client IP address. |
| trace stop ip_address \<IP\>                         | Stop tracing for a specific client IP address.  |

### trace list

```bash
$ emqx ctl trace list
Trace(ip_address=127.0.0.1, level=debug, destination="trace.log")
```

### trace start client \<ClientId\> \<File\> [\<Level\>]

```bash
$ emqx ctl trace start client emqx_c trace.log debug
trace emqx_c CLI-emqx_c successfully
```

### trace stop client \<ClientId\>

```bash
$ emqx ctl trace stop client emqx_c
stop tracing clientid emqx_c successfully
```

### trace start topic \<Topic> \<File> [\<Level>]

```bash
$ emqx ctl trace start topic t/1 trace.log info
trace t/1 CLI-t/1 successfully
```

### trace stop topic \<Topic>

```bash
$ emqx ctl trace stop topic t/1
stop tracing topic t/1 successfully
```

### trace start ip_address \<IP> \<File> [\<Level>]

```bash
$ emqx ctl trace start ip_address 127.0.0.1 trace.log debug
trace 127.0.0.1 CLI-127.0.0.1 successfully
```

### trace stop ip_address \<IP>

```bash
$ emqx ctl trace stop ip_address 127.0.0.1
stop tracing ip_address 127.0.0.1 successfully
```

::: tip
It's recommended to use absolute paths for trace log files when start from command line.
`emqx ctl trace start client foobar /abs/path/to/trace.log debug`
:::

::: tip
You can also manage traces from the dashboard UI. See [tracer](../observability/tracer.md).
:::

`emqx ctl traces`

This command is like the `trace` command, but applies on all nodes in the cluster.

## traces

This command is similar to the `trace` command, but it starts or stops a tracer across all nodes in the cluster. 

| Command                                                 | Description                       |
| ------------------------------------------------------- | --------------------------------- |
| traces list                                             | List all cluster traces started   |
| traces start \<Name> client \<ClientId> [\<Duration>]   | Traces for a client in cluster    |
| traces start \<Name> topic \<Topic> [\<Duration>]       | Traces for a topic in cluster     |
| traces start \<Name> ip_address \<IPAddr> [\<Duration>] | Traces for a client IP in cluster |
| traces stop \<Name>                                     | Stop trace in cluster             |
| traces delete \<Name>                                   | Delete trace in cluster           |

### traces list

```bash
$ emqx ctl traces list
Trace(mytraces_ip: ip_address=127.0.0.1, waiting, LogSize:#{'emqx@127.0.0.1' => 0})
```

### traces start \<Name> client \<ClientId> [\<Duration>]

```bash
$ emqx ctl traces start mytraces client emqx_c 1200
cluster_trace clientid emqx_c mytraces successfully
```

### traces start \<Name> topic \<Topic> [\<Duration>]

```bash
$ emqx ctl traces start mytraces_ip topic t/1 1200
cluster_trace topic t/1 mytraces_ip successfully
```

### traces start \<Name> ip_address \<IPAddr> [\<Duration>]

```bash
$ emqx ctl traces start mytraces_ip ip_address 127.0.0.1 1200
cluster_trace ip_address 127.0.0.1 mytraces_ip successfully
```

### traces stop \<Name>

```bash
$ emqx ctl traces stop mytraces_ip
Stop cluster_trace mytraces_ip successfully
```

### traces delete \<Name>

```bash
$ emqx ctl traces delete mytraces_ip
Del cluster_trace mytraces_ip successfully
```

## listeners

This command is used to manage listeners.

| Command                          | Description                                                  |
| -------------------------------- | ------------------------------------------------------------ |
| listeners                        | List information of all listeners.                           |
| listeners stop \<Identifier\>    | Stop a listener. Identifier is in the format `{type}:{name}`, e.g., `tcp:default`. |
| listeners start \<Identifier\>   | Start a listener.                                            |
| listeners restart \<Identifier\> | Restart a listener.                                          |

### listeners

```bash
$ emqx ctl listeners
ssl:default
  listen_on       : 0.0.0.0:8883
  acceptors       : 16
  proxy_protocol  : false
  running         : true
  current_conn    : 0
  max_conns       : 5000000
tcp:default
  listen_on       : 0.0.0.0:1883
  acceptors       : 16
  proxy_protocol  : false
  running         : true
  current_conn    : 12
  max_conns       : 5000000
ws:default
  listen_on       : 0.0.0.0:8083
  acceptors       : 16
  proxy_protocol  : false
  running         : true
  current_conn    : 0
  max_conns       : 5000000
wss:default
  listen_on       : 0.0.0.0:8084
  acceptors       : 16
  proxy_protocol  : false
  running         : true
  current_conn    : 0
  max_conns       : 5000000
```

### listeners stop \<Identifier\>

```bash
$ emqx ctl listeners stop tcp:default
Stop tcp:default listener successfully.
```

### listeners start \<Identifier\>

```bash
$ emqx ctl listeners start tcp:default
Started tcp:default listener successfully.
```

### listeners restart \<Identifier\>

```bash
$ emqx ctl listeners restart tcp:default
Restarted tcp:default listener successfully.
```

::: tip
Stopping or restarting a listener causes all the connected clients to disconnect.
:::


## authz cache-clean

`emqx ctl authz cache-clean`

This command is useful when you want to force evict cached authz (ACL) data.

## pem_cache

`emqx ctl pem_cache`

This command is to force EMQX reload updated pem (x509 keys and certificates) files after for example a certificate renewal.

## olp

`emqx ctl olp`

OLP stands for overload protection.
The `olp` command is to check overload status, and also the enable/disabled system overload protection.

For more details, see `overload_protection` configuration doc.

::: tip
`olp` is not enabled by default, enabling from CLI does not persist it to the configs.
:::

## gateway-registry

`emqx ctl gateway-registry`

List the registered gateways in the system.

Currently there are by default 5 registered gateways:

* coap
* exproto
* lwm2m
* mqttsn
* stomp

EMQX is designed to be plugable, so that more gateways can be installed as pluginsand register to EMQX at runtime.
Once registered, a gateway can be managed with management APIs and CLIs (see `gateway` command below).

## gatewa

This command can be used to inspect or manage gateway loading/running status.

| Command                            | Descriptio                                          |
| ---------------------------------- | --------------------------------------------------- |
| gateway list                       | List information of all gateways.                   |
| gateway lookup \<Name\>            | Look up detailed information of a specific gateway. |
| gateway load \<Name\> \<JsonConf\> | Load a gateway and configure its parameters.        |
| gateway unload \<Name\>            | Unload a gateway.                                   |
| gateway stop \<Name\>              | Stop a gateway.                                     |
| gateway start \<Name\>             | Start a gateway.                                    |

### gateway list

```bash
$ emqx ctl gateway list
Gateway(name=coap, status=running, clients=0, started_at=2023-05-22T14:23:50.353+08:00)
Gateway(name=exproto, status=unloaded)
Gateway(name=lwm2m, status=unloaded)
Gateway(name=mqttsn, status=unloaded)
Gateway(name=stomp, status=unloaded)
```

### gateway lookup \<Name\>

```bash
$ emqx ctl gateway lookup coap
name: coap
status: running
created_at: 2023-05-22T14:23:50.352+08:00
started_at: 2023-05-22T14:23:50.353+08:00
config: #{connection_required => false,enable => true,enable_stats => true,
          heartbeat => 30000,idle_timeout => 30000,
          listeners =>
              #{udp =>
                    #{default =>
                          #{access_rules => [],bind => 5683,enable => true,
                            enable_authn => true,max_conn_rate => 1000,
                            max_connections => 1024000,
                            udp_options =>
                                #{active_n => 100,reuseaddr => true}}}},
          mountpoint => <<>>,notify_type => qos,publish_qos => coap,
          subscribe_qos => coap}
```

### gateway load \<Name\> \<JsonConf\>

```bash
emqx ctl gateway load coap '{"type":"coap", ...}'
```

### gateway unload \<Name\>

```bash
$ emqx ctl gateway unload coap
ok
```

### gateway stop \<Name\>

```bash
$ emqx ctl gateway stop coap
ok
```

### gateway start \<Name\>

```bash
$ emqx ctl gateway start coap
ok
```


## gateway-metrics

Inspect gateway metrics.

## rules

This command is used to list rules crated in the Rule Engine.

| Command             | Description                                                  |
| ------------------- | ------------------------------------------------------------ |
| rules list          | List all rules, including information such as rule ID, name and etc. |
| rules show \<RuleID> | Display the detailed information of a specific rule.         |

Note that below is an example of the execution of each rule:

### rules list

```bash
$ emqx ctl rules list
Rule{id=my-rule, name=, enabled=true, descr=this is my rule}
```

### rules show \<RuleID>

```bash
$ emqx ctl rules show my-rule
Id:
  my-rule
Name:

Description:
  this is my rule
Enabled:
  true
SQL:
  SELECT
    *
  FROM
    "f/#"
Created at:
  2023-05-22T14:14:27.567+08:00
Updated at:
  2023-05-22T14:14:27.567+08:00
Actions:
  - Name:  republish
    Type:  function
    Args:  #{payload => <<>>,qos => 0,retain => false,topic => <<"t/1">>,
             user_properties => <<"${user_properties}">>}
```

CLI is only for inspection, Rule and action managements are managed from dashboard.

## license

::: tip

This section applies to the EMQX Enterprise edition only.

:::

| Command                | Description                  |
| ---------------------- | ---------------------------- |
| license info           | Display License information. |
| license update License | Updat License information.   |

### license info

```bash
$ emqx ctl license info
customer        : Evaluation
email           : contact@emqx.io
deployment      : default
max_connections : 100
start_at        : 2023-01-09
expiry_at       : 2028-01-08
type            : trial
customer_type   : 10
expiry          : false
```

### license update License

```bash
emqx ctl license update <YOUR_LICENSE_STRING>
```

You need to replace `YOUR_LICENSE_STRING` with the actual License string.
