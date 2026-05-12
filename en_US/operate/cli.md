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
Node 'emqx@127.0.0.1' 5.8.7 is started
```

## broker

This command is to inspect the local broker running status, statistics and metrics.

```bash
$ emqx ctl broker
sysdescr  : EMQX Enterprise
version   : 5.8.7
datetime  : 2025-08-06T10:01:23.857678490+02:00
uptime    : 52 seconds
```

### broker stats

This command is used to view the statistics of the local broker, including the number of connections, sessions, subscriptions, topics, and other metrics.

```bash
$ emqx ctl broker stats
channels.count                : 0
channels.max                  : 0
cluster_sessions.count        : 0
cluster_sessions.max          : 0
connections.count             : 0
connections.max               : 0
delayed.count                 : 0
delayed.max                   : 0
durable_subscriptions.count   : 0
durable_subscriptions.max     : 0
live_connections.count        : 0
live_connections.max          : 0
retained.count                : 3
retained.max                  : 3
sessions.count                : 0
sessions.max                  : 0
suboptions.count              : 0
suboptions.max                : 0
subscribers.count             : 0
subscribers.max               : 0
subscriptions.count           : 0
subscriptions.max             : 0
subscriptions.shared.count    : 0
subscriptions.shared.max      : 0
topics.count                  : 0
topics.max                    : 0
```

### broker metrics

This command is used to view the metrics of the local broker, including authentication, authorization, message delivery, packet processing, and overload protection metrics.

```bash
$ emqx ctl broker metrics
authentication.failure        : 0
authentication.success        : 0
authentication.success.anonymo: 0
authorization.allow           : 0
authorization.cache_hit       : 0
authorization.cache_miss      : 0
authorization.deny            : 0
authorization.matched.allow   : 0
authorization.matched.deny    : 0
authorization.nomatch         : 0
authorization.superuser       : 0
bytes.received                : 0
bytes.sent                    : 0
client.auth.anonymous         : 0
client.authenticate           : 0
client.authorize              : 0
client.connack                : 0
client.connect                : 0
client.connected              : 0
client.disconnected           : 0
client.subscribe              : 0
client.unsubscribe            : 0
delivery.dropped              : 0
delivery.dropped.expired      : 0
delivery.dropped.no_local     : 0
delivery.dropped.qos0_msg     : 0
delivery.dropped.queue_full   : 0
delivery.dropped.too_large    : 0
messages.acked                : 0
messages.delayed              : 0
messages.delivered            : 0
messages.dropped              : 0
messages.dropped.await_pubrel_: 0
messages.dropped.no_subscriber: 0
messages.forward              : 0
messages.persisted            : 0
messages.publish              : 0
messages.qos0.received        : 0
messages.qos0.sent            : 0
messages.qos1.received        : 0
messages.qos1.sent            : 0
messages.qos2.received        : 0
messages.qos2.sent            : 0
messages.received             : 0
messages.sent                 : 0
messages.transformation_failed: 0
messages.transformation_succee: 0
messages.validation_failed    : 0
messages.validation_succeeded : 0
overload_protection.delay.ok  : 0
overload_protection.delay.time: 0
overload_protection.gc        : 0
overload_protection.hibernatio: 0
overload_protection.new_conn  : 0
packets.auth.received         : 0
packets.auth.sent             : 0
packets.connack.auth_error    : 0
packets.connack.error         : 0
packets.connack.sent          : 0
packets.connect.received      : 0
packets.disconnect.received   : 0
packets.disconnect.sent       : 0
packets.pingreq.received      : 0
packets.pingresp.sent         : 0
packets.puback.inuse          : 0
packets.puback.missed         : 0
packets.puback.received       : 0
packets.puback.sent           : 0
packets.pubcomp.inuse         : 0
packets.pubcomp.missed        : 0
packets.pubcomp.received      : 0
packets.pubcomp.sent          : 0
packets.publish.auth_error    : 0
packets.publish.dropped       : 0
packets.publish.error         : 0
packets.publish.inuse         : 0
packets.publish.received      : 0
packets.publish.sent          : 0
packets.pubrec.inuse          : 0
packets.pubrec.missed         : 0
packets.pubrec.received       : 0
packets.pubrec.sent           : 0
packets.pubrel.missed         : 0
packets.pubrel.received       : 0
packets.pubrel.sent           : 0
packets.received              : 0
packets.sent                  : 0
packets.suback.sent           : 0
packets.subscribe.auth_error  : 0
packets.subscribe.error       : 0
packets.subscribe.received    : 0
packets.unsuback.sent         : 0
packets.unsubscribe.error     : 0
packets.unsubscribe.received  : 0
session.created               : 0
session.discarded             : 0
session.resumed               : 0
session.takenover             : 0
session.terminated            : 0

```

## cluster

This command is used to view and manage the cluster status of nodes.

Note that the `join` command in EMQX for joining a cluster is a "request" sent to the node specified in the parameters, rather than an "invitation". In other words, the command `emqx ctl cluster join <OneOfTheClusteredNodes>` is used to send a request to join the cluster of the node specified by `<OneOfTheClusteredNodes>`, rather than having that node join its own cluster.

### cluster join \<Node\>

Use this command to join a node to the EMQX cluster where the specified node is located.

Ensure that the specified node is active and accessible.

```bash
$ emqx ctl cluster join emqx2@127.0.0.1
Failed to join the cluster: {node_down,'emqx2@127.0.0.1'}
```

### cluster leave

Use this command to remove the node from the current EMQX cluster.

```bash
$ emqx ctl cluster leave
Failed to leave the cluster: node_not_in_cluster
```

### cluster force-leave \<Node\>

Forcefully remove a node from the cluster.

Use this command to remove the specified node from the EMQX cluster forcefully.

::: tip Note

This operation may cause cluster state inconsistency, so use it with caution.

:::

```bash
$ emqx ctl cluster force-leave emqx2@127.0.0.1
Failed to remove the node from cluster: node_not_in_cluster
```

### cluster status [--json]

Use this command to view the status of the EMQX cluster.

The optional `--json` parameter displays the cluster status in JSON format.

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

### cluster discovery enable

Enable and run automatic cluster discovery (if configured).

```bash
$ emqx ctl cluster discovery enable
Automatic cluster discovery enabled.
```

## clients

This command is to view and manage connected clients.

### clients list

View all clients currently connected to EMQX. This command can be used to monitor active clients and the number of connections.

:::tip
If a large number of clients are connected to the system, the `list` command may be time-consuming and resource-intensive.
:::

```bash
$ emqx ctl clients list
Client(emqx_c, username=undefined, peername=127.0.0.1:59441, clean_start=true, keepalive=60, session_expiry_interval=0, subscriptions=1, inflight=0, awaiting_rel=0, delivered_msgs=4530, enqueued_msgs=0, dropped_msgs=0, connected=true, created_at=1684736435155, connected_at=1684736435155)
Client(emqx_a, username=undefined, peername=127.0.0.1:59444, clean_start=true, keepalive=60, session_expiry_interval=0, subscriptions=1, inflight=0, awaiting_rel=0, delivered_msgs=4588, enqueued_msgs=0, dropped_msgs=0, connected=true, created_at=1684736441613, connected_at=1684736441613)
```

### clients show \<ClientId\>

View detailed connection information for a specific client.

```bash
$ emqx ctl clients show emqx_c
Client(emqx_c, username=undefined, peername=127.0.0.1:59441, clean_start=true, keepalive=60, session_expiry_interval=0, subscriptions=1, inflight=0, awaiting_rel=0, delivered_msgs=4680, enqueued_msgs=0, dropped_msgs=0, connected=true, created_at=1684736435155, connected_at=1684736435155)
```

### clients kick \<ClientId\>

Forcefully disconnect a specified client.

```bash
$ emqx ctl clients kick emqx_c
ok
```

## topics

This command is to view all subscribed topics in current system.

### topics list

List all topics. This command can be used to monitor the number and distribution of topics.

:::tip Note

If there are a large number of topic subscriptions in the cluster, the `list` command may be time-consuming and resource-intensive.
:::

```bash
$ emqx ctl topics list
t/1 -> emqx@127.0.0.1
```

### topics show \<Topic\>

Show detailed information about a specific topic.

```bash
$ emqx ctl topics show t/1
t/1 -> emqx@127.0.0.1
```

## subscriptions

This command is to view, add or delete clients' subscriptions.

### subscriptions list

List all subscriptions.

```bash
$ emqx ctl subscriptions list
emqx_a -> topic:t/1 qos:0 nl:0 rh:0 rap:0
emqx_c -> topic:t/1 qos:0 nl:0 rh:0 rap:0
```

### subscriptions show \<ClientId\>

Show subscriptions for a specific client.

```bash
$ emqx ctl subscriptions show emqx_a
emqx_a -> topic:t/1 qos:0 nl:0 rh:0 rap:0
```

### subscriptions add \<ClientId\> \<Topic\> \<QoS\>

Manually add a subscription.

```bash
$ emqx ctl subscriptions add emqx_a t/1 1
ok
```

### subscriptions del \<ClientId\> \<Topic\>

Manually remove a subscription.

```bash
$ emqx ctl subscriptions del emqx_a t/1
ok
```

:::tip
When there are a large number of subscriptions in the system, the `list` command may be time-consuming and resource-intensive.
:::

## plugins

This command is used to view and manage plugin installation.

### plugins list

List all installed plugins.

```bash
emqx ctl plugins list
[]
```

### plugins describe \<Name-Vsn\>

Display detailed information about an installed plugin.

```bash
emqx ctl plugins describe emqx_auth_mnesia-3.0.1
```

### plugins allow \<Name-Vsn\>

Grant the permission to install a specified plugin via the Dashboard.

```bash
emqx ctl plugins allow emqx_auth_mnesia-3.0.1
{
  "result" : "ok",
  "name_vsn" : "emqx_auth_mnesia-3.0.1",
  "action" : "do_allow_installation"
}
```

### plugins disallow \<Name-Vsn\>

Revoke the permission to install a specified plugin via the Dashboard.

```bash
emqx ctl plugins disallow emqx_auth_mnesia-3.0.1
{
  "result" : "ok",
  "name_vsn" : "emqx_auth_mnesia-3.0.1",
  "action" : "do_disallow_installation"
}
```

### plugins install \<Name-Vsn\>

Install a plugin package that is located in the plugin installation directory.

```bash
emqx ctl plugins install emqx_auth_mnesia-3.0.1
```

### plugins uninstall \<Name-Vsn\>

Uninstall a specified plugin.

```bash
emqx ctl plugins uninstall emqx_auth_mnesia-3.0.1
```

### plugins start \<Name-Vsn\>

Start a specified plugin.

```bash
emqx ctl plugins start emqx_auth_mnesia-3.0.1
```

### plugins stop \<Name-Vsn\>

Stop a specified plugin.

```bash
emqx ctl plugins stop emqx_auth_mnesia-3.0.1
```

### plugins restart \<Name-Vsn\>

Restart a specified plugin.

```bash
emqx ctl plugins restart emqx_auth_mnesia-3.0.1
```

### plugins disable \<Name-Vsn\>

Disable automatic startup of a plugin.

```bash
emqx ctl plugins disable emqx_auth_mnesia-3.0.1
```

### plugins enable \<Name-Vsn\> [Position]

Enable automatic startup of a plugin and specify the startup position.

```bash
emqx ctl plugins enable emqx_auth_mnesia-3.0.1 front
```

You can use `front`, `rear`, or `before Other-Vsn` to specify a relative position for adjusting the startup order. If no Position is provided, the configured plugins will remain in their original positions, and the new plugin will be appended at the end.

## vm

Inspect statistic data collected from the Erlang virtual machine.

### vm all

This command is used to view all information about Erlang VM, including CPU Load, memory usage, etc.

```bash
$ emqx ctl vm all
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

### vm load

This command is used to view the CPU load averages of Erlang VM over 1, 5, and 15 minutes.

```bash
$ emqx ctl vm load
cpu/load1               : 0.96
cpu/load5               : 1.03
cpu/load15              : 1.05
```

### vm memory

This command is used to view the memory usage of Erlang VM, including total memory, process memory, atom memory, binary memory, and ETS memory.

```bash
$ emqx ctl vm memory
memory/total            : 218672189
memory/processes        : 70762184
memory/processes_used   : 70760616
memory/system           : 147910005
memory/atom             : 3080769
memory/atom_used        : 3061022
memory/binary           : 1652808
memory/code             : 67620307
memory/ets              : 17414480
```

### vm process

This command is used to view the process information of Erlang VM, including the number of processes, and process limit.

```bash
$ emqx ctl vm process
process/limit           : 2097152
process/count           : 870
```

### vm io

This command is used to view the I/O information of Erlang VM, including the maximum number of file descriptors and the number of active file descriptors.

```bash
$ emqx ctl vm io
io/max_fds              : 1048576
io/active_fds           : 0
```

### vm ports

This command is used to view the port information of Erlang VM, including the number of ports and the port limit.

```bash
$ emqx ctl vm ports
ports/count             : 12
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

### log set-level \<Level\>

Set the overall log level.

```bash
$ emqx ctl log set-level debug
debug
```

### log primary-level

Show the current primary log level. `primary-level` represents the primary log level of EMQX, which is used to specify the default log level for the entire system. Setting `primary-level` will affect all log outputs unless specific log handlers have their own independent log levels.

```bash
$ emqx ctl log primary-level
debug
```

### log primary-level \<Level\>

Set the primary log level.

```bash
$ emqx ctl log primary-level info
info
```

### log handlers list

Show the log handlers. `handlers` refer to the collection of log handlers used for handling logs. Each log handler can set its own log level independently and define how to handle and store log messages.

```bash
$ emqx ctl log handlers list
LogHandler(id=ssl_handler, level=debug, destination=console, status=started)
LogHandler(id=console, level=debug, destination=console, status=started)
```

### log handlers start \<HandlerId\>

Start a specific handler.

```bash
$ emqx ctl log handlers start console
log handler console started
```

### log handlers stop \<HandlerId\>

Stop a specific handler。

```bash
$ emqx ctl log handlers stop console
log handler console stopped
```

### log handlers set-level \<HandlerId\> \<Level\>

Set the log level for a specific handler.

```bash
$ emqx ctl log handlers set-level console debug
debug
```

## trace

This command is used to trace (and log) events of a given client or topic etc.

### trace list

List all traces started on the local node.

```bash
$ emqx ctl trace list
Trace(ip_address=127.0.0.1, level=debug, destination="trace.log")
```

### trace start client \<ClientId\> \<File\> [\<Level\>]

Start tracing for a specific client.

```bash
$ emqx ctl trace start client emqx_c trace.log debug
trace emqx_c CLI-emqx_c successfully
```

### trace stop client \<ClientId\>

Stop tracing for a specific client.

```bash
$ emqx ctl trace stop client emqx_c
stop tracing clientid emqx_c successfully
```

### trace start topic \<Topic\> \<File\> [\<Level\>]

Start tracing for a specific topic.

```bash
$ emqx ctl trace start topic t/1 trace.log info
trace t/1 CLI-t/1 successfully
```

### trace stop topic \<Topic\>

Stop tracing for a specific topic.

```bash
$ emqx ctl trace stop topic t/1
stop tracing topic t/1 successfully
```

### trace start ip_address \<IP\> \<File\> [\<Level\>]

Start tracing for a specific client IP address.

```bash
$ emqx ctl trace start ip_address 127.0.0.1 trace.log debug
trace 127.0.0.1 CLI-127.0.0.1 successfully
```

### trace stop ip_address \<IP\>

Stop tracing for a specific client IP address.

```bash
$ emqx ctl trace stop ip_address 127.0.0.1
stop tracing ip_address 127.0.0.1 successfully
```

::: tip
It's recommended to use absolute paths for trace log files when start from command line.
`emqx ctl trace start client foobar /abs/path/to/trace.log debug`
:::

::: tip
You can also manage traces from the dashboard UI. See [Log Trace](./observability/tracer.md).
:::

## traces

This command is similar to the `trace` command, but it starts or stops a tracer across all nodes in the cluster.

### traces list

List all cluster traces started.

```bash
$ emqx ctl traces list
Trace(mytraces_ip: ip_address=127.0.0.1, waiting, LogSize:#{'emqx@127.0.0.1' => 0})
```

### traces start \<Name\> client \<ClientId\> [\<Duration\>]

Traces for a client in cluster.

```bash
$ emqx ctl traces start mytraces client emqx_c 1200
cluster_trace clientid emqx_c mytraces successfully
```

### traces start \<Name\> topic \<Topic\> [\<Duration\>]

Traces for a topic in cluster.

```bash
$ emqx ctl traces start mytraces_ip topic t/1 1200
cluster_trace topic t/1 mytraces_ip successfully
```

### traces start \<Name\> ip_address \<IPAddr\> [\<Duration\>]

Traces for a client IP in cluster.

```bash
$ emqx ctl traces start mytraces_ip ip_address 127.0.0.1 1200
cluster_trace ip_address 127.0.0.1 mytraces_ip successfully
```

### traces stop \<Name\>

Stop trace in cluster.

```bash
$ emqx ctl traces stop mytraces_ip
Stop cluster_trace mytraces_ip successfully
```

### traces delete \<Name\>

Delete trace in cluster.

```bash
$ emqx ctl traces delete mytraces_ip
Del cluster_trace mytraces_ip successfully
```

## listeners

This command is used to manage listeners.

### listeners

List information of all listeners.

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
  shutdown_count  : [{takenover,2},{discarded,1}]
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

#### Common Shutdown Reasons

For TCP listeners, EMQX reports a `shutdown_count` field, which records how many client disconnections occurred, grouped by reason. This can help identify why clients are getting disconnected from the TCP listener.

```bash
shutdown_count  : [{takenover,2},{discarded,1}]
```

In the example above:

- 2 clients were disconnected because a new session took over the same `clientid`.
- 1 client was discarded because a new clean session replaced it.

Below is a list of common shutdown reasons that may appear in this field.

| Reason                        | Description                                                  |
| ----------------------------- | ------------------------------------------------------------ |
| `banned`                      | The client is blacklisted due to ACL violations, rate limiting, or IP restrictions. |
| `closed`                      | The connection was closed either by the server or the client. |
| `discarded`                   | A new client with the same `clientid` and `clean_start = true` connected while the previous session was still active. |
| `takenover`                   | A new client with the same `clientid` and `clean_start = false` connected while the previous session was still active. |
| `einval`                      | An invalid argument or socket error occurred, often caused by attempting to write to a socket that has already been closed (a race condition between socket state change notification and data write). |
| `frame_too_large`             | The MQTT packet exceeds the maximum allowed frame size.      |
| `idle_timeout`                | No `CONNECT` packet was received within the allowed time after the TCP/SSL connection was established. |
| `invalid_proto_name`          | The protocol name in the `CONNECT` packet is invalid or not `"MQTT"`. |
| `invalid_topic`               | The client used an invalid topic (e.g., containing illegal characters or forbidden by the broker). |
| `keepalive_timeout`           | The client failed to send any packets within the keepalive interval. |
| `malformed_packet`            | The MQTT packet is corrupted or does not conform to the MQTT specification. |
| `not_authorized`              | The client attempted an unauthorized action, rejected by ACL. |
| `ssl_closed`                  | The SSL/TLS connection was closed by the peer.               |
| `ssl_error`                   | An error occurred during the SSL/TLS handshake or data transmission. |
| `ssl_upgrade_timeout`         | The SSL/TLS handshake did not complete within the allowed time. |
| `unexpected_packet`           | The client sent a packet that was unexpected in the current connection state. |
| `zero_remaining_len`          | The packet has a zero remaining length field, which is invalid in most contexts. |
| `bad_username_or_password`    | Authentication failed due to incorrect username or password. |
| `client_identifier_not_valid` | The provided `clientid` is invalid or locked by another client during login. |
| `protocol_error`              | A generic MQTT protocol violation occurred.                  |
| `tcp_closed`                  | The TCP connection was closed by the client or due to a network issue. |
| `timeout`                     | A general timeout occurred (e.g., during authentication, etc.). |

### listeners stop \<Identifier\>

Stop a listener. Identifier is in the format `{type}:{name}`, e.g., `tcp:default`. (Temporarily effective; the original state will be restored after EMQX restarts.)

```bash
$ emqx ctl listeners stop tcp:default
Stop tcp:default listener successfully.
```

::: tip
Stopping a listener causes all the connected clients to disconnect.
:::

### listeners start \<Identifier\>

Start a listener. (Temporarily effective; the original state will be restored after EMQX restarts.)

```bash
$ emqx ctl listeners start tcp:default
Started tcp:default listener successfully.
```

### listeners restart \<Identifier\>

Restart a listener.

```bash
$ emqx ctl listeners restart tcp:default
Restarted tcp:default listener successfully.
```

::: tip
Restarting a listener causes all the connected clients to disconnect.
:::

### listeners enable \<Identifier\> \<true/false\>

Enable or disable a listener. (Persisted to the configuration; permanently effective.)

```bash
$ emqx ctl listeners enable tcp:default true
Enabled tcp:default listener successfully.
```

```bash
$ emqx ctl listeners enable tcp:default false
Disabled tcp:default listener successfully.
```


## authz cache-clean

This command is useful when you want to force evict cached authz (ACL) data.

### authz cache-clean all

Clears authorization cache on all nodes.

```bash
$ emqx ctl authz cache-clean all
Authorization cache drain started on all nodes OK
```

### authz cache-clean node \<Node\>

Clears authorization cache on the given node.

```bash
$ emqx ctl authz cache-clean node emqx@127.0.0.1
Authorization cache drain started on node emqx@127.0.0.1 OK
```

### authz cache-clean \<ClientId\>

Clears authorization cache for the given client.

```bash
$ emqx ctl authz cache-clean mqttx_9502dc8a
Drain mqttx_9502dc8a authz cache OK
```

## pem_cache

This command is to force EMQX reload updated pem (x509 keys and certificates) files.

### pem_cache clean all

Clears x509 certificate cache on all nodes.

```bash
$ emqx ctl pem_cache clean all
PEM cache clean OK
```

### pem_cache clean node \<Node\>

```bash
$ emqx ctl pem_cache clean emqx@127.0.0.1
emqx@127.0.0.1 PEM cache clean OK
```

## olp

OLP stands for overload protection.
The `olp` command is to check overload status, and also the enable/disabled system overload protection.

For more details, see `overload_protection` configuration doc.

::: tip
`olp` is not enabled by default, enabling from CLI does not persist it to the configs.
:::

### olp status

Return status of overload protection if the system is overloaded.
Reports 'not overloaded' otherwise.

```bash
$ emqx ctl olp status
'emqx@172.17.0.3' is not overloaded
```

### olp enable

Enable overload protection.

```bash
$ emqx ctl olp enable
Enable overload protection 'emqx@127.0.0.1' : {ok,<0.5703.0>}
```

### olp disable

Disable overload protection.

```bash
$ emqx ctl olp disable
Disable overload protetion 'emqx@127.0.0.1' : ok
```

## data

This command is used for exporting/importing node data to/from a tar archive file.

### data export

```bash
data export \
  [--root-keys key1,key2,key3] \
  [--table-sets set1,set2,set3] \
  [--dir out_dir]
```

Export data from the EMQX node to a tar archive file. This command is useful for backing up data or transferring data between nodes.

Included data:
- Cluster configuration
- Additional files from EMQX data directory, like SSL certificates
- Built-in database

Options `--root-keys` and `--table-sets` can be used to specify which data to export. If not specified, all data will be exported.

```bash
emqx ctl data export --root-keys listeners,connectors,actions,rule_engine --dir /tmp
Exporting data to "/tmp/emqx-export-2025-08-06-12-00-19.334.tar.gz"...
Exporting cluster configuration...
Exporting additional files from EMQX data_dir: "data"...
Exporting built-in database...
Exporting emqx_banned_rules database table...
Exporting emqx_banned database table...
Exporting emqx_psk database table...
Exporting emqx_authn_mnesia database table...
Exporting emqx_authn_scram_mnesia database table...
Exporting emqx_acl database table...
Exporting emqx_app database table...
Exporting emqx_mt_config database table...
Exporting emqx_admin database table...
Exporting emqx_retainer_message database table...
Data has been successfully exported to /tmp/emqx-export-2025-08-06-12-00-19.334.tar.gz.
```

### data import \<File\>

Import data from the specified tar archive file. This command is used for restoring data from a backup or transferring data to a new node.

```
emqx ctl data import /tmp/emqx-export-2025-08-06-12-00-19.334.tar.gz
Importing data from "/tmp/emqx-export-2025-08-06-12-00-19.334.tar.gz"...
Importing cluster configuration for namespace global...
Importing built-in database...
Importing emqx_retainer_message database table...
Starting reindexing retained messages
Reindexed 3 messages
Reindexing retained messages finished
Importing emqx_admin database table...
Importing emqx_mt_config database table...
Importing emqx_app database table...
Importing emqx_acl database table...
Importing emqx_authn_scram_mnesia database table...
Importing emqx_authn_mnesia database table...
Importing emqx_psk database table...
Importing emqx_banned database table...
Importing emqx_banned_rules database table...
Data has been imported successfully.
```

## ds

This command is used to operate Durable Storage.

### ds info

Show overview of the embedded durable storage state.

```bash
emqx ctl ds info
THIS SITE:
EFC84E67230295E2

SITES:
.------------------.----------------.--------.
: Site             : Node           : Status :
:------------------:----------------:--------:
: EFC84E67230295E2 : emqx@127.0.0.1 :     up :
 ------------------ ---------------- --------

SHARDS:
.----------.----------.-------------.
: DB/Shard : Replicas : Transitions :
:----------:----------:-------------:
 ---------- ---------- -------------
```

### ds set-replicas \<storage\> \<site1\> \<site2\> ...

Change the replica set of the durable storage.

### ds join \<storage\> \<site\>

Add site to the replica set of the storage.

### ds leave \<storage\> \<site\>

Remove site from the replica set of the storage.

### ds forget \<site\>

Remove a site from the list of known sites.

## exclusive

This command is to view all exclusive topics in the current system or delete an exclusive topic.

### exclusive list

List all exclusive topics.

```bash
$ emqx ctl exclusive list
t/1 -> client1
```

### exclusive delete \<Topic\>

Delete an exclusive topic.

```bash
$ emqx ctl exclusive delete t/1
ok
```

## retainer

The `retainer` command can be used to inspect or manage retained messages. It also comes with a `emqx ctl retainer reindex` command which can be used to create or update indices for retained messages.

### retainer info

Display the number of retained messages.

```bash
$ emqx ctl retainer info
Number of retained messages: 3
```

### retainer topics

Display all topics with retained messages.

```bash
$ emqx ctl retainer topics
$SYS/brokers
$SYS/brokers/emqx@127.0.0.1/sysdescr
$SYS/brokers/emqx@127.0.0.1/version
```

### retainer clean

Clear all retained messages.

```bash
emqx ctl retainer clean
```

### retainer clean \<Topic\>

Clear retained messages according to the specific topic filter.

```bash
emqx ctl retainer clean t/1
```

### retainer reindex status

Display the status of reindexing process.

```bash
$ emqx ctl retainer reindex status
Reindexing is not running
```

### retainer reindex start [force]

Generate a new index for retained message topics based on the configuration settings. Pass `true` as the `<force>` parameter to ignore any previously started reindexing process.

```bash
$ emqx ctl retainer reindex start true
Starting reindexing
Reindexed 0 messages
Reindexing finished
```

## observer

This command provides Erlang virtual machine insights including a realtime view like linux's `top` command. Subcommands are as follows:

### observer status

Launch the observer in the current console, used to monitor and debug the status and activities of the EMQX node.

```bash
$ emqx ctl observer status
```

### observer bin_leak

Force all processes to perform garbage collection and prints the top 100 processes that release the maximum amount of binary data, potentially revealing potential memory leak issues.

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

Ensure that the specified module is loaded on all nodes in the EMQX cluster. This command can be used to load modules when it is necessary to ensure that they are available throughout the entire cluster.

```bash
$ emqx ctl observer load Mod
Loaded 'Mod' module on []: ok
```

## conf

This command is used for inspecting and altering EMQX Cluster configuration.

### conf reload --replace|--merge

Reload etc/emqx.conf on local node. The new configuration values will be overlaid on the existing values by default. Use the --replace flag to replace existing values with the new ones instead.

### conf show_keys

Print all the currently used configuration keys.

### conf show [\<key\>]

Print in-use configs (including default values) under the given key. Print ALL keys if key is not provided.

### conf load --replace|--merge \<path\>

Load a HOCON format config file. The new configuration values will be overlaid on the existing values by default. Use the `--replace` flag to replace existing values with the new ones instead. The current node will initiate a cluster wide config change transaction to sync the changes to other nodes in the cluster.

NOTE: Do not make runtime config changes during rolling upgrade.

## conf cluster_sync

This command is used for troubleshooting when there is something wrong with cluster-calls used to sync configuration changes between the nodes in the cluster. 

::: tip 

In EMQX 5.0.x, this command was named `cluster_call`. This old command is still available in EMQX 5.1 but it is not displayed in the help information.

:::

EMQX HTTP API can be used to modify many configurations. When an API is called, for example, through operations in the Dashboard, the node receiving the request first writes the modified content locally to `data/configs/cluster.hocon`. Then, the same operation is recorded in the database and asynchronously forwarded to other nodes in the cluster.

If for some reason, this replication can not apply in a peer node, this command can be used to inspect and even fix the replication so it can move forward.

EMQX generates an ID (`tnx_id`) for each configuration modification within the cluster scope. This ID strictly increases within the cluster scope, and every modification, such as changing a configuration from the Dashboard, is recorded in the database.

::: tip
The `skip` and `fast_forward` commands may result in diverged configs between the nodes in the cluster.
:::

### conf cluster_sync status

Show cluster config sync status summary for all nodes.

```bash
$ emqx ctl conf cluster_sync status
-----------------------------------------------
All configuration synchronized(tnx_id=0) successfully
-----------------------------------------------
```

### conf cluster_sync inspect \<tnx\_id\>

Inspect detailed information of the config change transaction at the given `tnx_id`.

The following example shows viewing the content of the second modification (`tnx_id=2`), which is an operation to enable a TLS listener.

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

### conf cluster_sync skip [node]

Increment the (currently failing) commit on the given node.

::: warning Note

This may lead to inconsistent configs among the clustered nodes.

:::

### conf cluster\_sync fast\_forward [node] \<tnx\_id\>

Fast-forward config change to the given `tnx_id` on the given node.

::: warning Note

This may lead to inconsistent configs among the clustered nodes.

:::

### conf cluster\_sync fix

Sync the node with the most comprehensive configuration to other node, typically the config leader (with the highest `tnx_id`).

## eviction status

This command is used to get current node eviction status.

```bash
$ emqx ctl eviction
Eviction status: disabled
```

## rebalance

The `rebalance` command is used to rebalance load on the cluster by migrating connections and sessions from high-load nodes to low-load nodes to achieve load balancing between nodes.

### rebalance start --evacuation

Start current node evacuation with optional server redirect to the specified servers.

```
rebalance start --evacuation \
  [--wait-health-check Secs] \
  [--redirect-to "Host1:Port1 Host2:Port2 .."] \
  [--conn-evict-rate CountPerSec] \
  [--migrate-to "node1@host1 node2@host2 .."] \
  [--wait-takeover Secs] \
  [--sess-evict-rate CountPerSec]
```

### rebalance start

Start rebalance on the specified nodes using the current node as the coordinator.

```
rebalance start \
  [--nodes "node1@host1 node2@host2 .."] \
  [--wait-health-check Secs] \
  [--conn-evict-rate ConnPerSec] \
  [--abs-conn-threshold Count] \
  [--rel-conn-threshold Fraction] \
  [--wait-takeover Secs] \
  [--sess-evict-rate CountPerSec] \
  [--abs-sess-threshold Count] \
  [--rel-sess-threshold Fraction]
```

### rebalance node-status

Get current node rebalance status.

### rebalance node-status "node1@host1"

Get remote node rebalance status.

### rebalance status

Get status of all current rebalance/evacuation processes across the cluster.

### rebalance stop

Stop current node evacuation.

## gateway

This command can be used to inspect or manage gateway loading/running status.

### gateway list

List information of all gateways.

```bash
$ emqx ctl gateway list
Gateway(name=coap, status=running, clients=0, started_at=2023-05-22T14:23:50.353+08:00)
Gateway(name=exproto, status=unloaded)
Gateway(name=lwm2m, status=unloaded)
Gateway(name=mqttsn, status=unloaded)
Gateway(name=stomp, status=unloaded)
```

### gateway lookup \<Name\>

Look up detailed information of a specific gateway.

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

Load a gateway and configure its parameters.

```bash
emqx ctl gateway load coap '{"type":"coap", ...}'
```

### gateway unload \<Name\>

Unload a gateway.

```bash
$ emqx ctl gateway unload coap
ok
```

### gateway stop \<Name\>

Stop a gateway.

```bash
$ emqx ctl gateway stop coap
ok
```

### gateway start \<Name\>

Start a gateway.

```bash
$ emqx ctl gateway start coap
ok
```

## gateway-registry

`emqx ctl gateway-registry`

List the registered gateways in the system.

Currently there are by default 5 registered gateways:

* coap
* exproto
* lwm2m
* mqttsn
* stomp

EMQX is designed to be pluggable, so that more gateways can be installed as pluginsand register to EMQX at runtime.
Once registered, a gateway can be managed with management APIs and CLIs (see `gateway` command below).

## gateway-clients

This command is used to inspect gateway clients.

### gateway-clients list \<Name\>

List all clients for a gateway.

### gateway-clients lookup \<Name\> \<ClientId\>

Lookup the Client Info for the specified client.

### gateway-clients kick \<Name\> \<ClientId\>

Kick out a specific client from the gateway.

## gateway-metrics \<Name\>

List all metrics for a gateway.

## license

::: tip

This section applies to the EMQX Enterprise edition only.

:::

### license info

Display License information.

```bash
$ emqx ctl license info
customer        : Developer
email           : contact@emqx.io
deployment      : Development
max_sessions    : 10000000
start_at        : 2025-03-02
expiry_at       : 2029-03-01
type            : community
customer_type   : 11
expiry          : false
```

### license update License

Update License information.

```bash
emqx ctl license update <YOUR_LICENSE_STRING>
```

You need to replace `YOUR_LICENSE_STRING` with the actual License string.

### license update default

Revert to default Community License.

```bash
emqx ctl license update default
```

## admins

The `admins` command can be used to manage administrative users.

### admins add \<Username\> \<Password\> \<Description\>

Add a Dashboard user.

```bash
$ emqx ctl admins add emqx_u EMQemq@1172
ok
```

### admins passwd \<Username\> \<Password\>

Reset the password for a specific Dashboard user.

```bash
$ emqx ctl admins passwd emqx_u EMQemq@11721
ok
```

### admins del \<Username\>

Delete a specific Dashboard user.

```bash
$ emqx ctl admins del emqx_u
ok
```

## rules

This command is used to list rules created in the Rule Engine.

### rules list

List all rules, including information such as rule ID, name and etc.

```bash
$ emqx ctl rules list
Rule{id=my-rule, name=, enabled=true, descr=this is my rule}
```

### rules show \<RuleID\>

Display the detailed information of a specific rule.

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
