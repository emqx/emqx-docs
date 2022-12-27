# 集群负载重平衡与节点疏散

Cluster load rebalancing is the enforced process of client and session migration from a set of nodes to some others.
Node evacuation is a particular case of rebalancing when we want to migrate all connections and sessions from
a particular node.

## Motivation

Users might want to shut down arbitrary nodes, e.g., to maintain the underlying
software or hardware. Or move connections from higher-loaded nodes to lower.

We need to minimize data loss when a node is switched off. The aim of node evacuation
is to minimize session data loss.

Node evacuation should transfer the maximum possible amount of connections and sessions to the other nodes.
After it's completed in one node, the node can be shut down by sysadmin (but not automatically).

Node rebalancing is generally the opposite process. It transfers a calculated (balanced among the nodes) number of connections
and sessions from higher loaded nodes to other nodes. A typical example is introducing or restarting a node.

## Evacuation

Evacuation is started locally on a node via CLI or with API.

* The evacuated node stops receiving connections.
* It gradually disconnects the connected clients.
* After all clients are disconnected, the node waits for a configured amount of time. After that, the disconnected clients
are expected to reconnect and takeover their sessions.
* Then, it gradually migrates the remaining (disconnected) sessions to other nodes.

The evacuation state is persistent. The node remains in the evacuated state even after restart. The node should be explicitly
returned to the normal state.

The CLI command is the following:

```
emqx_ctl rebalance start --evacuation \
    [--redirect-to "Host1:Port1 Host2:Port2 ..."] \
    [--conn-evict-rate CountPerSec] \
    [--migrate-to "node1@host1 node2@host2 ..."] \
    [--wait-takeover Secs] \
    [--sess-evict-rate CountPerSec]
```

| Param               | Type             | Description |
|---------------------|------------------|-------------|
| `--redirect-to`     | string           | Server reference for [Server redirect](https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901255). MQTT 5 clients will receive this property when trying to connect |
| `--conn-evict-rate` | positive integer | Client disconnection rate, count/second |
| `--migrate-to`      | string           | Space or comma-separated list of nodes to which sessions will be evacuated
| `--wait-takeover`   | positive integer | Amount of time in seconds to wait before starting session evacuation
| `--sess-evict-rate` | positive integer | Client evacuation rate, count/second |

Example:

```
./bin/emqx_ctl rebalance start --evacuation --wait-takeover 200 --conn-evict-rate 30 --sess-evict-rate 30 --migrate-to "emqx2@127.0.0.1 emqx3@127.0.0.1"
Rebalance(evacuation) started
```

The CLI command for getting evacuation status is:

```
emqx_ctl rebalance node-status
```

Example:

```
./bin/emqx_ctl rebalance node-status
Rebalance type: rebalance
Rebalance state: evicting_conns
Coordinator node: 'emqx2@127.0.0.1'
Connection eviction rate: 5 connections/second
Session eviction rate: 5 sessions/second
Connection goal: 504.0
Recipient nodes: ['emqx2@127.0.0.1']
Channel statistics:
  current_connected: 960
  current_disconnected_sessions: 35
  current_sessions: 995
  initial_connected: 1000
  initial_sessions: 1000
```

The CLI command to stop evacuation is:

```
emqx_ctl rebalance stop
```

Example:

```
./bin/emqx_ctl rebalance stop
Rebalance(evacuation) stopped
```

## Rebalancing

Rebalancing is a more complicated process since it involves several nodes. When enabled on a node:

* The node becomes the _coordinator_.
* It divides passed nodes into two groups: donors and recipients. The donors are the nodes with the excess number
of connections, and the recipients are the nodes that lack connections.
* It makes the donor nodes stop receiving connections.
* It waits for a configured amount of time to allow an LB to remove the donors from the active backends.
* It gradually disconnects the connected clients from the donor nodes until their average connection count reaches that of the recipients.
* Then, it waits for a configured amount of time. The disconnected clients are expected to reconnect and takeover their sessions.
* Then, it gradually migrates the remaining (disconnected) sessions from the donor nodes until their average disconnection session count reaches that of the recipients.
* Finally, the rebalancing stops, and the donor nodes are returned to their normal state.

Rebalancing is ephemeral. If one of the participating nodes crashes, the whole process is aborted on all nodes.

The CLI command is the following:

```
rebalance start \
    [--nodes "node1@host1 node2@host2"] \
    [--wait-health-check Secs] \
    [--conn-evict-rate ConnPerSec] \
    [--abs-conn-threshold Count] \
    [--rel-conn-threshold Fraction] \
    [--conn-evict-rate ConnPerSec] \
    [--wait-takeover Secs] \
    [--sess-evict-rate CountPerSec] \
    [--abs-sess-threshold Count] \
    [--rel-sess-threshold Fraction]
```

| Param                  | Type             | Description |
|------------------------|------------------|-------------|
| `--nodes`              | string           | Space or comma-separated list of nodes participating in the rebalance. It may or may not include the coordinator (node on which the command is run) |
| `--wait-health-check`  | positive integer | Amount of time in seconds to wait before starting rebalance. During this period, an LB is supposed to remove donor nodes from the active backends |
| `--conn-evict-rate`    | positive integer | Client disconnection rate on donor nodes, count/second |
| `--abs-conn-threshold` | positive integer | Absolute threshold for checking connection balance |
| `--rel-conn-threshold` | number > 1.0     | Relative threshold for checking connection balance |
| `--wait-takeover`      | positive integer | Amount of time in seconds to wait before starting session evacuation |
| `--sess-evict-rate`    | positive integer | Session evacuation rate on donor nodes, count/second |
| `--abs-sess-threshold` | positive integer | Absolute threshold for checking session balance |
| `--rel-sess-threshold` | number > 1.0     | Relative threshold for checking session balance |

Connections are considered to be balanced when the following condition holds:

```
avg(DonorConns) < avg(RecipientConns) + abs_conn_threshold OR avg(DonorConns) < avg(RecipientConns) * rel_conn_threshold
```

A similar rule is applied to disconnected sessions.

Example:

```
./bin/emqx_ctl rebalance start --wait-health-check 10 --wait-takeover 60  --conn-evict-rate 5 --sess-evict-rate 5 --abs-conn-threshold 30 --abs-sess-threshold 30 --nodes "emqx1@127.0.0.1 emqx2@127.0.0.1 emqx3@127.0.0.1"
Rebalance started
```

The CLI command for getting rebalance status is:

```
emqx_ctl rebalance node-status
```

Example:

```
./bin/emqx_ctl rebalance node-status
Node 'emqx1@127.0.0.1': rebalance coordinator
Rebalance state: evicting_conns
Coordinator node: 'emqx1@127.0.0.1'
Donor nodes: ['emqx2@127.0.0.1','emqx3@127.0.0.1']
Recipient nodes: ['emqx1@127.0.0.1']
Connection eviction rate: 5 connections/second
Session eviction rate: 5 sessions/second
Connection goal: 0.0
Current average donor node connection count: 300.0
```

The CLI command to stop rebalance is:

```
emqx_ctl rebalance stop
```

Example:

```
./bin/emqx_ctl rebalance stop
Rebalance stopped
```

## Load Balancer Integration

During evacuation/rebalance, it is up to the user to provide the necessary configuration for the load balancer (if any).
This configuration should help disconnected clients to be directed to the recipient nodes when they reconnect.
Without such a configuration, there may be an excess number of disconnections.

To help create that configuration, EMQX provides health check endpoints:

```
GET /api/v4/load_rebalance/availability_check
```

They respond with 503 HTTP code for the donor or evacuated nodes and 200 HTTP code for nodes operating normally and receiving connections.

For example, the described configuration for Haproxy and a 3-node cluster could look like this:

```
defaults
  timeout connect 5s
  timeout client 60m
  timeout server 60m

listen mqtt
  bind *:1883
  mode tcp
  maxconn 50000
  timeout client 6000s
  default_backend emqx_cluster

backend emqx_cluster
  mode tcp
  balance leastconn
  option httpchk
  http-check send meth GET uri /api/v4/load_rebalance/availability_check hdr Authorization "Basic YWRtaW46cHVibGlj"
  server emqx1 127.0.0.1:3001 check port 5001 inter 1000 fall 2 rise 5 weight 1 maxconn 1000
  server emqx2 127.0.0.1:3002 check port 5002 inter 1000 fall 2 rise 5 weight 1 maxconn 1000
  server emqx3 127.0.0.1:3003 check port 5003 inter 1000 fall 2 rise 5 weight 1 maxconn 1000
```

Here we have 3 nodes with MQTT listeners on ports 3001, 3002, and 3003 and HTTP listeners on ports 5001, 5002, and 5003, respectively.

## Global Status

CLI command to fetch information about all evacuation/rebalance processes across the cluster:

```
emqx_ctl rebalance status
```

Example:

```
./bin/emqx_ctl rebalance status
--------------------------------------------------------------------
Node 'emqx1@127.0.0.1': evacuation
Rebalance state: waiting_takeover
Connection eviction rate: 30 connections/second
Session eviction rate: 30 sessions/second
Connection goal: 0
Session goal: 0
Session recipient  nodes: ['emqx2@127.0.0.1']
Channel statistics:
  current_connected: 0
  current_sessions: 247
  initial_connected: 233
  initial_sessions: 247
--------------------------------------------------------------------
Node 'emqx2@127.0.0.1': rebalance coordinator
Rebalance state: wait_health_check
Coordinator node: 'emqx2@127.0.0.1'
Donor nodes: ['emqx3@127.0.0.1']
Recipient nodes: ['emqx2@127.0.0.1']
Connection eviction rate: 5 connections/second
Session eviction rate: 5 sessions/second
```

## API

All the operations available from the CLI are also available from the API. Start/stop commands require a node
as a parameter.

### Enable Evacuation

```
curl -v -u admin:public -H "Content-Type: application/json" -X POST 'http://127.0.0.1:8081/api/v4/load_rebalance/emqx1@127.0.0.1/evacuation/start' -d '{"conn_evict_rate": 5, "sess_evict_rate": 5, "migrate_to": ["emqx3@127.0.0.1", "emqx2@127.0.0.1"]}'

{"data":[],"code":0}
```

Possible body fields are `nodes`, `redirect_to`, `conn_evict_rate`, `migrate_to`, `wait_takeover`, and `sess_evict_rate`
with the same meaning as in the corresponding [CLI command](#evacuation).

### Disable Evacuation

```
curl -v -u admin:public -H "Content-Type: application/json" -X POST 'http://127.0.0.1:8081/api/v4/load_rebalance/emqx1@127.0.0.1/evacuation/stop'

{"data":[],"code":0}
```

### Enable Rebalance

```
curl -v -u admin:public -H "Content-Type: application/json" -X POST 'http://127.0.0.1:8081/api/v4/load_rebalance/emqx1@127.0.0.1/start' -d '{"conn_evict_rate": 5, "sess_evict_rate": 5, "nodes": ["emqx1@127.0.0.1", "emqx2@127.0.0.1"]}'

{"data":[],"code":0}
```

Possible body fields are `nodes`, `conn_evict_rate`, `sess_evict_rate`, `wait_takeover`, `wait_health_check`, `abs_conn_threshold`, `rel_conn_threshold`, `abs_sess_threshold`, and `rel_sess_threshold`
with the same meaning as in the corresponding [CLI command](#rebalance).

### Disable Rebalance

```
curl -v -u admin:public -H "Content-Type: application/json" -X POST 'http://127.0.0.1:8081/api/v4/load_rebalance/emqx1@127.0.0.1/stop'

{"data":[],"code":0}
```

### Get Node-local Status

```
curl -s -u admin:public -H "Content-Type: application/json" -X GET 'http://127.0.0.1:8081/api/v4/load_rebalance/status'

{
  "status": "enabled",
  "stats": {
    "initial_sessions": 0,
    "initial_connected": 0,
    "current_sessions": 0,
    "current_connected": 0
  },
  "state": "waiting_takeover",
  "session_recipients": [
    "emqx3@127.0.0.1",
    "emqx2@127.0.0.1"
  ],
  "session_goal": 0,
  "session_eviction_rate": 5,
  "process": "evacuation",
  "connection_goal": 0,
  "connection_eviction_rate": 5
}
```

### Get Cluster-wide Status

```
curl -s -u admin:public -H "Content-Type: application/json" -X GET 'http://127.0.0.1:8081/api/v4/load_rebalance/global_status'
{
  "rebalances": [],
  "evacuations": [
    {
      "node": "emqx1@127.0.0.1",
      "stats": {
        "initial_sessions": 0,
        "initial_connected": 0,
        "current_sessions": 0,
        "current_connected": 0
      },
      "state": "waiting_takeover",
      "session_recipients": [
        "emqx3@127.0.0.1",
        "emqx2@127.0.0.1"
      ],
      "session_goal": 0,
      "session_eviction_rate": 5,
      "connection_goal": 0,
      "connection_eviction_rate": 5
    }
  ]
}
```


