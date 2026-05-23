# Mria Logs and Alarms

This document describes various errors reported by Mria database management system.

## Network Partition

When a network partition is detected,
the following log messages appear in the logs on all nodes (Cores or Replicants):

```
[error] ** Node 'emqx@remote.host' not responding **, ** Removing (timedout) connection **
...
[notice] msg: Remote RLOG agent died, reason: noconnection, repl_state: ...
```

When the partition heals the following logs will appear on all core nodes,
as EMQX detects that the previously lost peers reconnect:

```
[error] Mnesia('emqx@local.host'): ** ERROR ** mnesia_event got {inconsistent_database, running_partitioned_network, 'emqx@remote.host'}

[critical] msg: Core cluster partition, context: running_partitioned_network, from: 'emqx@remote.host'
```

Also, a `partition` alarm is raised:

```
[warning] msg: alarm_is_activated, message: <<"Partition occurs at node emqx@remote.host">>, name: partition
```

In addition,
on Core nodes in the minority partition the following logs will appear:

```
[notice] msg: Mria is restarting to join the cluster, seed: 'emqx@remote.node'
[warning] msg: Stopping mria, reason: heal
[notice] msg: stopping_emqx_apps, ...
```

When the minority reboot is complete,
the rebooted core nodes will print a standard EMQX hello message:
```
...
Listener tcp:default on 0.0.0.0:1883 started.
Listener ssl:default on 0.0.0.0:8883 started.
Listener ws:default on 0.0.0.0:8083 started.
Listener wss:default on 0.0.0.0:8084 started.
```

Replicants:

```
[notice] msg: Shard fully up, node: 'emqx@remote.host', shard: ...
```

Another indication of the partition recovery is the `broker_heal` alarm that is raised on all nodes:

```
[warning] msg: broker_heal_initiated, pid: <0.8705.0>, results: ...
```

This alarm clears automatically:

```
[warning] msg: alarm_is_deactivated, pid: <0.4506.0>, name: broker_heal
```
