# 集群负载重平衡与节点疏散

集群负载重平衡是将客户端与会话从一组节点强行迁移到其他节点的行为，而节点疏散则是要将所有连接与会话从某个节点迁离的行为。

## 应用场景

节点疏散或负载重平衡可以用于以下场景，比如需要关闭任意节点以便维护底层软件或硬件；或是需要将连接从高负载节点迁到低负载节点。

关闭节点时，我们需要将数据损失降到最低，而节点疏散正是为了使会话数据的损失最小化。

节点疏散时，应该将所有连接与会话数据迁移到其他节点。节点疏散完成后，节点不会自动关闭，需要系统管理员手动关闭

节点重平衡的流程则相反。它将自动计算得到到达成节点平衡所需迁移的连接数量，然后将对应数量的连接和会话数从高负载节点迁移到低负载节点，从而在节点之间实现负载均衡。通常在新加入或重启一个节点后需要此操作来达成平衡。

## 节点疏散

可以通过命令行工具（CLI）或 API 在待疏散节点进行节点疏散操作。

* 待疏散节点将停止接收新的连接请求。
* 待疏散节点将逐渐断开已连接的客户端。
* 在所有客户端的连接断开后，待疏散节点将按设定进入一段等待期。等待期后，已断开连接的客户端将尝试重连并恢复会话。
* 超过等待时间后，逐渐将剩余未被接管的会话迁移至其他节点，完成疏散。

节点疏散任务是一项长期任务，因此即使在重启后，疏散节点仍将处于疏散状态。需要重新将节点的状态设为正常。

您可以通过如下命令执行节点的疏散任务：

```
emqx_ctl rebalance start --evacuation \
    [--redirect-to "Host1:Port1 Host2:Port2 ..."] \
    [--conn-evict-rate CountPerSec] \
    [--migrate-to "node1@host1 node2@host2 ..."] \
    [--wait-takeover Secs] \
    [--sess-evict-rate CountPerSec]
```

| 配置项          | 类型           | 描述 |
|---------------------|------------------|-------------|
| `--redirect-to`     | string           | 具体可参考[《MQTT 5.0 协议》中的《服务器重定向》](https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901255)章节。对于采用 MQTT 5.0 协议的客户端，在尝试连接时会获得该配置项。 |
| `--conn-evict-rate` | positive integer | 客户端每秒的连接断开率。 |
| `--migrate-to`      | string           | 待疏散的会话列表，以空格或逗号区隔。 |
| `--wait-takeover`   | positive integer | 等待秒数，读秒后，将开启会话疏散任务。 |
| `--sess-evict-rate` | positive integer | 客户端每秒的疏散率。 |

示例：

```
./bin/emqx_ctl rebalance start --evacuation --wait-takeover 200 --conn-evict-rate 30 --sess-evict-rate 30 --migrate-to "emqx2@127.0.0.1 emqx3@127.0.0.1"
Rebalance(evacuation) started
```

获取疏散状态的命令如下：

```
emqx_ctl rebalance node-status
```

示例：

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

停止疏散任务的命令如下：

```
emqx_ctl rebalance stop
```

示例：

```
./bin/emqx_ctl rebalance stop
Rebalance(evacuation) stopped
```

## 重平衡

重平衡涉及多个节点，因此比疏散任务要复杂。当基于某个节点执行重平衡任务时：

* 该节点变为*调度节点*。
* 调度节点将接入的节点分为两类，源节点和目标节点。源节点就是那些连接数超量的节点，目标节点是连接数不足的节点。
* 调度节点将告知源节点停止接收新的连接。
* 在经过设定的等待时间后，调度节点会将源节点从活跃的后端节点列表中移除。
* 调度节点会逐步断开已连接至源节点的客户端，直到源节点的平均连接数与目标节点相同。
* 在经过设定的等待时间后，断开的客户端会尝试重连并恢复会话。
* 调度节点会逐步将把剩下的（已断开）的会话从源节点迁离，直到源节点的平均连接数与目标节点相同。
* 最后，重平衡任务结束，源节点切回正常状态。

重平衡是一项临时性任务。如果任一节点在进行重平衡时崩溃，整个任务将结束。

执行重平衡任务的命令如下：

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

| 配置项                 | 类型             | 描述                                                         |
| ---------------------- | ---------------- | ------------------------------------------------------------ |
| `--nodes`              | string           | 参与负载重平衡的节点列表，以空格或逗号区隔，调度节点，即在其上运行负载重平衡命令的节点，可以不在列表中。 |
| `--wait-health-check`  | positive integer | 等待秒数，读秒结束后，重平衡任务将启动。在此期间，负载均衡器会将源节点从活跃的后端节点列表中移除。 |
| `--conn-evict-rate`    | positive integer | 源节点每秒的连接断开率。                                     |
| `--abs-conn-threshold` | positive integer | 用于检查连接平衡的绝对阈值。                                 |
| `--rel-conn-threshold` | number > 1.0     | 用于检查连接平衡的相对阈值。                                 |
| `--wait-takeover`      | positive integer | 等待秒数，读秒后，将开始会话疏散任务。                       |
| `--sess-evict-rate`    | positive integer | 源节点每秒的会话疏散率。                                     |
| `--abs-sess-threshold` | positive integer | 用于检查会话连接平衡的绝对阈值。                             |
| `--rel-sess-threshold` | number > 1.0     | 用于检查会话连接平衡的相对阈值。                             |

当满足以下条件时，我们认为连接是平衡的：

```
avg(DonorConns) < avg(RecipientConns) + abs_conn_threshold OR avg(DonorConns) < avg(RecipientConns) * rel_conn_threshold
```

类似的规则也适用于会话的连接断开。

示例：

```
./bin/emqx_ctl rebalance start --wait-health-check 10 --wait-takeover 60  --conn-evict-rate 5 --sess-evict-rate 5 --abs-conn-threshold 30 --abs-sess-threshold 30 --nodes "emqx1@127.0.0.1 emqx2@127.0.0.1 emqx3@127.0.0.1"
Rebalance started
```

获取重平衡状态的命令如下：

```
emqx_ctl rebalance node-status
```

示例：

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

停止重平衡任务的命令如下：

```
emqx_ctl rebalance stop
```

示例：

```
./bin/emqx_ctl rebalance stop
Rebalance stopped
```

## 集成负载均衡器

在执行疏散/重平衡任务时，如使用负载均衡器，用户需自行提供相关配置信息。断开的客户端尝试重连时，负载均衡器会基于这些配置信息将其重新定向到目标节点。如果没有此类配置信息，可能出现断开连接数过多的问题。

为方便用户配置，EMQX 提供了健康检查功能：

```
GET /api/v4/load_rebalance/availability_check
```

执行健康检查后，EMQX 会针对源节点/待疏散节点返回 HTTP 状态码 503，对于正常运行及接收连接请求的节点，返回 HTTP 状态码 200。

比如，对于 HAProxy 和一个三节点集群，配置文件应如下所示：

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

我们有三个节点，对应的 MQTT 监听器分别位于  3001、3002 和 3003 端口，HTTP 监听器则分别位于 5001、5002 和 5003 端口。

## 全局状态

我们可通过如下命令获取整体集群的疏散/重平衡任务状态：

```
emqx_ctl rebalance status
```

示例：

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

所有的命令行操作也可通过 API 完成。在执行开始/停止命令时，需要以参数的形式传入节点信息。

### 开启疏散任务

```
curl -v -u admin:public -H "Content-Type: application/json" -X POST 'http://127.0.0.1:8081/api/v4/load_rebalance/emqx1@127.0.0.1/evacuation/start' -d '{"conn_evict_rate": 5, "sess_evict_rate": 5, "migrate_to": ["emqx3@127.0.0.1", "emqx2@127.0.0.1"]}'

{"data":[],"code":0}
```

请求体中应该以下字段：

-  `nodes`
-  `redirect_to`
- `conn_evict_rate`
- `migrate_to`
- `wait_takeover`
- `sess_evict_rate`

字段含义同对应的[命令行命令](#evacuation)相同。

### 停止疏散任务

```
curl -v -u admin:public -H "Content-Type: application/json" -X POST 'http://127.0.0.1:8081/api/v4/load_rebalance/emqx1@127.0.0.1/evacuation/stop'

{"data":[],"code":0}
```

### 开启重平衡任务

```
curl -v -u admin:public -H "Content-Type: application/json" -X POST 'http://127.0.0.1:8081/api/v4/load_rebalance/emqx1@127.0.0.1/start' -d '{"conn_evict_rate": 5, "sess_evict_rate": 5, "nodes": ["emqx1@127.0.0.1", "emqx2@127.0.0.1"]}'

{"data":[],"code":0}
```

请求体中应该以下字段： 

- `nodes`
- `conn_evict_rate`
- `sess_evict_rate`
- `wait_takeover`
- `wait_health_check`
- `abs_conn_threshold`
- `rel_conn_threshold`
- `abs_sess_threshold` 
- `rel_sess_threshold`

字段含义同对应的[命令行命令](#evacuation)相同。

### 停止重平衡任务

```
curl -v -u admin:public -H "Content-Type: application/json" -X POST 'http://127.0.0.1:8081/api/v4/load_rebalance/emqx1@127.0.0.1/stop'

{"data":[],"code":0}
```

### 获取本地节点的状态

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

### 获取整体集群的状态

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

