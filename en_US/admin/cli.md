# Command Line Interface

This page introduces all kinds of startup and administrative commands supported by EMQX and gives detailed introductions to ctl administrative commands.

## Startup Commands

EMQX supports some basic startup and administrative commands, which can be executed using the `emqx <command>` command.

Here are some commonly used startup and administrative commands:

| Command    | Description                                                  |
| ---------- | ------------------------------------------------------------ |
| start      | Starts EMQX in daemon mode, without requiring an interactive shell during runtime. |
| console    | Starts EMQX in Erlang or Elixir interactive shell. Used for debugging EMQX in a development environment, requiring interaction with EMQX. |
| foreground | Starts EMQX in foreground mode, without using an interactive shell. Used to start EMQX in a development environment without running it in the background. |
| stop       | Stops the running EMQX node.                                 |
| ctl        | Manages and monitors EMQX. Executing `emqx ctl help` can get more detailed information. |

The following are advanced commands for development and debugging, and ordinary users usually don't need to care about them:

| Command        | Description                                                  |
| -------------- | ------------------------------------------------------------ |
| remote_console | Connects to the interactive shell of a remote EMQX node.     |
| attach         | Attaches to a running EMQX node to perform interactive operations. |
| ertspath       | Retrieves the path of the EMQX Erlang library.               |
| root_dir       | Retrieves the path of the EMQX root directory.               |
| pid            | Retrieves the process ID of the running EMQX node.           |
| ping           | Checks if the EMQX node is running.                          |
| check_config   | Validates if the EMQX configuration file is correct.         |
| console_clean  | Clears the output of the interactive shell console.          |
| escript        | Executes an Escript script on the EMQX node.                 |

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

This command provides Erlang virtual machine insights including a realtime view like linux's 'top' command. Subcommands are as follows:

| Command           | Description                                                  |
| ----------------- | ------------------------------------------------------------ |
| observer status   | Launches the observer in the current console, used to monitor and debug the status and activities of the EMQX node. |
| observer bin_leak | Forces all processes to perform garbage collection and prints the top 100 processes that release the maximum amount of binary data, potentially revealing potential memory leak issues. |
| observer load Mod | Ensures that the specified module is loaded on all nodes in the EMQX cluster. This command can be used to load modules when it is necessary to ensure that they are available throughout the entire cluster. |

## conf cluster_sync

In version 5.0.x, this command was named `cluster_call`, this old command is still available in 5.1 but it is not displayed in usage info.

`emqx ctl conf cluster_sync`

This command is mostly for troubleshooting when there is something wrong with cluster-calls
used to sync config changes between the nodes in the cluster.

EMQX management API can be served by any of the 'core' nodes in the cluster, when a management
API is called (e.g., from the dashboard) to change configuration, this change is first applied
on the serving node, then replicated (asynchronously) to other nodes in the cluster.
If for some reason, this replication can not apply in a peer node, this command can be used
to inspect and even fix the replication so it can move forward.

For example, to inspect the 2nd config change (disabled SSL listener from dashboard).

```
$ emqx ctl conf cluster_sync tnxid 2
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

`emqx ctl admins`

The `admins` command can be used to create/update/delete administrative users.

## retainer

`emqx ctl retainer`

The `retainer` command can be used to inspect or manage retained messages.
It also comes with a `emqx ctl retainer reindex` command which can be used to
create or update indices for retained messages.


## cluster

`emqx ctl cluster`

This command is to join a node to a cluster.
Please note that the `join` instruction is 'requesting' rather not 'inviting'.
That is, the `emqx ctl cluster join <OneOfTheClusteredNodes>` is the be executed
on the joining node, but not from any of the nodes in the cluster.

## clients

`emqx ctl clients`

This command is to list/show/kick connected clients.

:::tip
It may take a long time to `list` all when there is a large number of clients.
:::


## topics

`emqx ctl topics`

This command is previously (in 4.x releases) known as the `routes` command.
It's to list/describe subscribed topics (or topic filters) in the cluster.

:::tip
It may take a long time to `list` all when there is large number of topics.
:::

## subscriptions

`emqx ctl subscriptions`

This command is to list/show/add/delete client's subscriptions.

:::tip
It may take a long time to `list` all when there is large number of subscriptions.
:::

## plugins

`emqx ctl plugins`

This command is used to manage plugin installation.

## vm

`emqx ctl vm`

Inspect statistic data collected from the Erlang virtual machine.

## mnesia

`emqx ctl mnesia`

Prints mnesia runtime status and metrics.

## log

`emqx ctl log`

This command can be used to manage log handlers states, such as setting logging level etc.

## trace and traces

`emqx ctl trace`

This command is used to trace (and log) events of a given client or topic etc.

::: tip
It's recommended to use absolute paths for trace log files when start from command line.
`emqx ctl trace start client foobar /abs/path/to/trace.log debug`
:::

::: tip
You can also manage traces from the dashboard UI. See [tracer](../observability/tracer.md)
:::

`emqx ctl traces`

This command is like the `trace` command, but applies on all nodes in the cluster.

## listeners

`emqx ctl listeners`

List or start/stop listeners

::: tip
Stopping or restarting a listener causes all the connected clients to disconnect.
:::


## authz cache-clean

`emqx ctl authz cache-clean`

This command is useful when you want to force evict cached authz (ACL) data.

## pem_cache

`emqx ctl pem_cache`

This command is to force EMQX reload updated pem (x509 keys and certificates) files
after for example a certificate renewal.

## olp

`emqx ctl olp`

OLP stands for overload protection.
The `olp` command is to check overload status, and also the enable/disabled system
overload protection.

For more details see `overload_protection` configuration doc.

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

EMQX is designed to be plugable, so that more gateways can be installed as plugins
and register to EMQX at runtime.
Once registered, a gateway can be managed with management APIs and CLIs (see `gateway` command below)

## gateway

`emqx ctl gateway`

This command can be used to inspect or manage gateway loading/running status.


## gateway-metrics

`emqx ctl gateway-metrics`

Inspect gateway metrics.

## rules

`emqx ctl rules`

List rules crated in the Rule Engine.
CLI is only for inspection, Rule and action managements are managed from dashboard.
