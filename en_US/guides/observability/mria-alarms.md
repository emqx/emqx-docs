# Mria Logs and Alarms

This document describes log messages and alarms reported by the Mria database management system. Currently, it covers network partition events. Additional error types may be added in future revisions.

## Network Partition

### Partition Detected

When a network partition is detected, the following log messages appear on all nodes (Core or Replicant):

```text
[error] ** Node 'emqx@remote.host' not responding **, ** Removing (timedout) connection **
...
[notice] msg: Remote RLOG agent died, reason: noconnection, repl_state: ...
```

### Partition Healed

When the partition heals, the following logs appear on all Core nodes as EMQX detects that the previously lost peers reconnect:

```text
[error] Mnesia('emqx@local.host'): ** ERROR ** mnesia_event got {inconsistent_database, running_partitioned_network, 'emqx@remote.host'}

[critical] msg: Core cluster partition, context: running_partitioned_network, from: 'emqx@remote.host'
```

Also, a `partition` alarm is raised:

```text
[warning] msg: alarm_is_activated, message: <<"Partition occurs at node emqx@remote.host">>, name: partition
```

### Core Node Recovery

On Core nodes in the minority partition, the following logs will appear:

```text
[notice] msg: Mria is restarting to join the cluster, seed: 'emqx@remote.node'
[warning] msg: Stopping mria, reason: heal
[notice] msg: stopping_emqx_apps, ...
```

When the minority reboot is complete, the rebooted Core nodes will print a standard EMQX hello message:

```text
...
Listener tcp:default on 0.0.0.0:1883 started.
Listener ssl:default on 0.0.0.0:8883 started.
Listener ws:default on 0.0.0.0:8083 started.
Listener wss:default on 0.0.0.0:8084 started.
```

### Replicant Recovery

On Replicant nodes, the following log confirms that replication has fully resumed:

```text
[notice] msg: Shard fully up, node: 'emqx@remote.host', shard: ...
```

### Broker Heal Alarm

Another indication of partition recovery is the `broker_heal` alarm, raised on all nodes:

```text
[warning] msg: broker_heal_initiated, pid: <0.8705.0>, results: ...
```

This alarm clears automatically:

```text
[warning] msg: alarm_is_deactivated, pid: <0.4506.0>, name: broker_heal
```
