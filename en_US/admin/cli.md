# CLI

## Main script

The main boot script of EMQX is a bash script which can also be used to
execute some of the administrative commands.

```bash
$ emqx help
Usage: emqx COMMAND [help]

Commonly used COMMANDs:
  start:      Start EMQX in daemon mode
  console:    Start EMQX in an interactive Erlang or Elixir shell
  foreground: Start EMQX in foreground mode without an interactive shell
  stop:       Stop the running EMQX node
  ctl:        Administration commands, execute 'emqx ctl help' for more details

More:
  Shell attach:  remote_console | attach
  Up/Down-grade: upgrade | downgrade | install | uninstall
  Install info:  ertspath | root_dir | versions | root_dir
  Runtime info:  pid | ping | versions
  Advanced:      console_clean | escript | rpc | rpcterms | eval | eval-erl

Execute 'emqx COMMAND help' for more information
```

## The ctl Commands

The `emqx ctl COMMAND ARGS ...` (or equivalently `emqx_ctl COMMAND ARGS ...`) commands
require EMQX node to be up and running.

It starts up a (hidden) Erlang node, connects to the local EMQX Erlang node
and issues Erlang RPC calls to get the commands executed.

Below is a list of all supported commands without copy-pasting a lot of the
descriptive information from the usage outputs.

### status

`emqx ctl status`

This command is a quick inspection to see if the broker is up and running.

### broker

`emqx ctl broker`

This command is to inspect the local broker running status, statistics and metrics.

### observer

`emqx ctl observer`

This command provides Erlang virtual machine insights including a realtime view like linux's 'top' top command.

### cluster_call

`emqx ctl cluster_call`

This command is mostly for troubleshooting when there is something wrong with cluster-calls
used to sync config changes between the nodes in the cluster.

EMQX management API can be served by any of the 'core' nodes in the cluster, when a management
API is called (e.g., from the dashboard) to change configuration, this change is first applied
on the serving node, then replicated (asynchronously) to other nodes in the cluster.
If for some reason, this replication can not apply in a peer node, this command can be used
to inspect and even fix the replication so it can move forward.

For example, to inspect the 2nd config change (disabled SSL listener from dashboard).

```
$ emqx ctl cluster_call tnxid 2
{atomic,#{created_at => {{2022,6,21},{21,57,50}},
          initiator => 'emqx@127.0.0.1',
          mfa =>
              {emqx,update_config,
                    [[listeners,ssl,default],
                     {action,stop,#{<<"enabled">> => false}},
                     #{override_to => cluster,rawconf_with_defaults => true}]},
          tnx_id => 2}}
```

::: warning Warning
The `skip` and `fast_forward` commands may result in diverged configs between the nodes in the cluster.
:::

### admins

`emqx ctl admins`

The `admins` command can be used to create/update/delete administrative users.

### retainer

`emqx ctl retainer`

The `retainer` command can be used to inspect or manage retained messages.
It also comes with a `emqx ctl retainer reindex` command which can be used to
create or update indices for retained messages.


### cluster

`emqx ctl cluster`

This command is to join a node to a cluster.
Please note that the `join` instruction is 'requesting' rather not 'inviting'.
That is, the `emqx ctl cluster join <OneOfTheClusteredNodes>` is the be executed
on the joining node, but not from any of the nodes in the cluster.

### clients

`emqx ctl clients`

This command is to list/show/kick connected clients.

:::warning
It may take a long time to `list` all when there is a large number of clients.
:::


### topics

`emqx ctl topics`

This command is previously (in 4.x releases) known as the `routes` command.
It's to list/describe subscribed topics (or topic filters) in the cluster.

:::warning
It may take a long time to `list` all when there is large number of topics.
:::

### subscriptions

`emqx ctl subscriptions`

This command is to list/show/add/delete client's subscriptions.

:::warning
It may take a long time to `list` all when there is large number of subscriptions.
:::

### plugins

`emqx ctl plugins`

This command is used to manage plugin installation.

### vm

`emqx ctl vm`

Inspect statistic data collected from the Erlang virtual machine.

### mnesia

`emqx ctl mnesia`

Prints mnesia runtime status and metrics.

### log

`emqx ctl log`

This command can be used to manage log handlers states, such as setting logging level etc.

### trace and traces

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

### listeners

`emqx ctl listeners`

List or start/stop listeners

::: warning
Stopping or restarting a listener causes all the connected clients to disconnect.
:::


### authz cache-clean

`emqx ctl authz cache-clean`

This command is useful when you want to force evict cached authz (ACL) data.

### pem_cache

`emqx ctl pem_cache`

This command is to force EMQX reload updated pem (x509 keys and certificates) files
after for example a certificate renewal.

### olp

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

### gateway

`emqx ctl gateway`

This command can be used to inspect or manage gateway loading/running status.


### gateway-metrics

`emqx ctl gateway-metrics`

Inspect gateway metrics.

### rules

`emqx ctl rules`

List rules crated in the Rule Engine.
CLI is only for inspection, Rule and action managements are managed from dashboard.
