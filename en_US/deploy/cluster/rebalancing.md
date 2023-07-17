# Node Evacuation and Cluster Load Rebalancing

MQTT is a stateful long-lived connection access protocol, which means connections will not be easily disconnected once the connection is established. Therefore, upgrading, maintaining, and scaling cluster nodes will become more challenging. To facilitate users' cluster operation and maintenance, EMQX provides node evacuation and cluster load rebalancing functions.

## Node Evacuation

When you need to maintain or upgrade a node in the cluster, directly shutting down the node can result in lost connections and sessions, causing data loss. In addition, this type of operation can cause a large number of devices to go offline and reconnect for a period of time, increasing server load and potentially affecting overall business.

Therefore, EMQX provides node evacuation functionality to help migrate all connection and session data from the node to other nodes in the cluster before shutting down, reducing the impact on the overall business.

### How It Works

The node evacuation works in the following sequence:

1. The node to be evacuated stops receiving connections.
2. The node to be evacuated gradually disconnects its current clients at a preset rate (specified by `conn-evict-rate`). The disconnected clients use the reconnection mechanism to connect to other nodes (the target nodes) in the cluster. The reconnection mechanisms differ depending on the protocol versions:
   - MQTT v3.1/v3.1.1 clients: specified by load balancing strategy and the client needs to enable the reconnection mechanism;
   - MQTT v5.0 clients: specified by the `redirect-to` parameter.
3. Wait for the target node to complete reconnection with the clients and take over the sessions (specified by `wait-takeover`).
4. After the reconnection waiting time has elapsed, the remaining unclaimed sessions on the nodes to be evacuated will be migrated to the target node:

     - The node to which the session will be migrated is specified by `migrate-to`;

     - The speed of session migration is specified by `sess-evict-rate`.


You can stop the evacuation at any time. If the node to be evacuated closes during the evacuation, the evacuation process will be resumed after the node is restarted.

### Start and Stop Node Evacuation via CLI

You can use CLI command to start the node evacuation, get the evacuation status, and stop the node evacuation.

#### Start Node Evacuation

You can use the following CLI command to start the node evacuation. The `--evacuation` parameter means this is an evacuation operation:

```bash
./bin/emqx_ctl rebalance start --evacuation \
    [--wait-health-check Secs] \
    [--redirect-to "Host1:Port1 Host2:Port2 ..."] \
    [--conn-evict-rate CountPerSec] \
    [--migrate-to "node1@host1 node2@host2 ..."] \
    [--wait-takeover Secs] \
    [--sess-evict-rate CountPerSec]
```

| Parameter             | Type             | Description                                                  |
| --------------------- | ---------------- | ------------------------------------------------------------ |
| `--wait-health-check` | Positive integer | The duration (in seconds, 60 seconds by default) during which the node waits for the Load Balancer (LB) to remove it from the active backend node list. When this specified waiting time elapses, the evacuation process begins, and the source node starts rejecting any new incoming connections. |
| `--redirect-to`       | String           | The redirected server address during reconnection, for MQTT 5.0 clients; Refer to [MQTT 5.0 Specification - Server redirection](https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901255) for more details. |
| `--conn-evict-rate`   | Positive integer | Client disconnection rate, count/second; 500 connections per second by default |
| `--migrate-to`        | String           | Space or comma-separated list of nodes to which sessions will be evacuated |
| `--wait-takeover`     | Positive integer | Amount of time in seconds to wait before starting session evacuation; Unit: second, 60 seconds by default |
| `--sess-evict-rate`   | Positive integer | Client evacuation rate, count/second; 500 sessions per second by default |


**Code Example**

If you want to migrate the clients on the node `emqx@127.0.0.1` to the nodes `emqx2@127.0.0.1` and `emqx3@127.0.0.1`, you can execute the following command on the node `emqx@127.0.0.1`:

```bash
./bin/emqx_ctl rebalance start --evacuation \
	--wait-health-check 60 \
	--wait-takeover 200 \
	--conn-evict-rate 30 \
	--sess-evict-rate 30 \
	--migrate-to "emqx2@127.0.0.1 emqx3@127.0.0.1"
Rebalance(evacuation) started
```

This command will disconnect existing clients at a rate of `30` connections per second. After all connections are disconnected, it will wait for `200` seconds during which client sessions will be migrated to the reconnected nodes. Afterward, the remaining sessions will be migrated at a rate of `30` sessions per second to the `emqx2@127.0.0.1` and `emqx3@127.0.0.1` nodes.

#### Get Evacuation Status

You can use the following CLI command to get the evacuation status:

```bash
./bin/emqx_ctl rebalance node-status
```

Below is an example of the returned results:

```bash
Rebalance type: evacuation
Rebalance state: evicting_conns
Connection eviction rate: 30 connections/second
Session eviction rate: 30 sessions/second
Connection goal: 0
Session goal: 0
Session recipient nodes: []
Channel statistics:
  current_connected: 10
  current_sessions: 0
  initial_connected: 100
  initial_sessions: 0
```

#### Stop Node Evacuation

You can use the following CLI command to stop evacuation:

```bash
./bin/emqx_ctl rebalance stop
```

Below is an example of the returned results:

```bash
./bin/emqx_ctl rebalance stop
Rebalance(evacuation) stopped
```

### Start/Stop Node Evacuation via HTTP API

You can also use HTTP API to start/stop the node evacuation and you need to specify the node to be evacuated in the parameters. For details, see [API Docs](https://docs.emqx.com/en/enterprise/v5.0/admin/api-docs.html).

## Rebalancing

Due to the same reason as MQTT being a stateful long-lived connection protocol, connections will not be easily disconnected once the connection is established. Even after scaling up the nodes, existing connections do not automatically shift to the newly added nodes. Consequently, if there are not a significant number of new client connections, the additional nodes may remain underutilized for a long period. In such cases, you need to manually migrate connections from high-load nodes to low-load nodes to achieve cluster load balancing.

<img src="./assets/rebalancing.png" alt="rebalancing" style="zoom:50%;" />

### How It Works

Rebalancing is a more complicated process since it involves several nodes.

You can initiate a cluster load rebalancing task on any node. EMQX will automatically calculate the necessary connection migration plan based on the current connection load of each node. It will then migrate the corresponding number of connections and sessions from high-load nodes to low-load nodes to achieve load balancing between nodes. The workflow is as follows:

1. Calculate the migration plan and divide the nodes involved in rebalancing (specified by `--nodes`) into source nodes and target nodes:
   - Source nodes: High-load nodes
   - Target nodes: Low-load nodes
2. Stop accepting new connections on the source nodes.
3. Wait for a period of time (specified by `wait-health-check`) until the load balancer (LB) removes the source nodes from the active backend node list.
4. Gradually disconnect connected clients on the source nodes until the average number of connections matches that of the target nodes.
5. Wait for the target nodes to reconnect with clients and take over the sessions (specified by `wait-takeover`).
6. After the reconnection waiting time exceeds, the source nodes will migrate the remaining unclaimed sessions to the target nodes at the rate specified by `sess-evict-rate`.

At this point, the load rebalancing task is completed, and the source nodes return to their normal state.

::: tip

Rebalancing is ephemeral. If one of the participating nodes crashes, the whole process is aborted on all nodes.

:::

### Start and Stop Rebalancing via CLI

You can use CLI command to start the rebalancing, get the rebalancing status, and stop the rebalancing.

#### Start Rebalancing

The command for starting the rebalancing includes the following fields:

```bash
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

| Fields                 | Type             | Description                                                  |
| ---------------------- | ---------------- | ------------------------------------------------------------ |
| `--nodes`              | String           | Space or comma-separated list of nodes participating in the rebalance. It may or may not include the coordinator (node on which the command is run) |
| `--wait-health-check`  | Positive integer | The duration (in seconds, 60 seconds by default) during which the node waits for the Load Balancer (LB) to remove it from the active backend node list. When this specified waiting time elapses, the load rebalancing process begins. |
| `--conn-evict-rate`    | Positive integer | Client disconnection rate on source nodes; 500 connections per second by default |
| `--abs-conn-threshold` | Positive integer | Absolute threshold for checking connection balance; 1000 by default |
| `--rel-conn-threshold` | Number<br /> > 1.0 | Relative threshold for checking connection balance; 1.1 by default |
| `--wait-takeover`      | Positive integer | Specified waiting time (in seconds, default 60 seconds) for clients to reconnect and take over the sessions after all connections are disconnected. |
| `--sess-evict-rate`    | Positive integer | Session evacuation rate on source nodes; 500 sessions per second by default |
| `--abs-sess-threshold` | Positive integer | Absolute threshold for checking session balance; 1000 by default |
| `--rel-sess-threshold` | Number<br /> > 1.0 | Relative threshold for checking session balance; 1.1 by default |

**Check Session Balance**

Connections are considered to be balanced when the following condition holds:

```bash
avg(DonorConns) < avg(RecipientConns) + abs_conn_threshold
OR
avg(DonorConns) < avg(RecipientConns) * rel_conn_threshold
```

A similar rule is applied to disconnected sessions.

**Example**

To achieve load rebalancing among the three nodes `emqx@127.0.0.1`, `emqx2@127.0.0.1`, and `emqx3@127.0.0.1`, you can use the following command:

```bash
./bin/emqx_ctl rebalance start \
	--wait-health-check 10 \
	--wait-takeover 60  \
	--conn-evict-rate 5 \
	--sess-evict-rate 5 \
	--abs-conn-threshold 30 \
	--abs-sess-threshold 30 \
	--nodes "emqx1@127.0.0.1 emqx2@127.0.0.1 emqx3@127.0.0.1"
Rebalance started
```

#### Get Rebalance Status

The CLI command for getting rebalance status is:

```bash
./bin/emqx_ctl rebalance node-status
```

**Example**

```bash
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

#### Stop Rebalancing

The CLI command to stop rebalancing is:

```bash
emqx_ctl rebalance stop
```

**Example**

```bash
./bin/emqx_ctl rebalance stop
Rebalance stopped
```

### Start/Stop Rebalancing via HTTP API

All the operations available from the CLI are also available from the API. Start/stop commands require a node as a parameter. For details, see [API Docs](https://docs.emqx.com/en/enterprise/v5.1/admin/api-docs.html).

## Integrate Load Balancer

Users can integrate a load balancer to perform evacuation/rebalancing. When a disconnected client attempts to reconnect, the load balancer will redirect it to the recipient nodes based on the current status of backend nodes. Users need to configure health check parameters for integrating the load balancer; otherwise, there can be an excess number of disconnections. To assist in creating this configuration, EMQX provides a health check REST API:

`GET /api/v5/load_rebalance/availability_check`

The health check responds with HTTP code 503 for the donor or evacuated nodes and HTTP code 200 for nodes operating normally and receiving connections.

For example, you can use the following configuration for HAProxy and an EMQX cluster with 3 nodes, with the MQTT listeners on ports 3001, 3002, and 3003, and REST API ports on 5001, 5002, and 5003:

```bash
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
  http-check send meth GET uri /api/v5/load_rebalance/availability_check hdr Authorization "Basic xxxxxx"
  server emqx1 127.0.0.1:3001 check port 5001 inter 1000 fall 2 rise 5 weight 1 maxconn 1000
  server emqx2 127.0.0.1:3002 check port 5002 inter 1000 fall 2 rise 5 weight 1 maxconn 1000
  server emqx3 127.0.0.1:3003 check port 5003 inter 1000 fall 2 rise 5 weight 1 maxconn 1000
```
