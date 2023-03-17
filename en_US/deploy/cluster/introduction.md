# Cluster

Besides working with a single EMQX node, EMQX also provides the cluster feature for high availability, better scalability, data security, and centralized management. 

## Reasons for clustering

There are situations where you could benefit from EMQX clusters.

1. **Scalability**: EMQX can be easily scaled horizontally by adding more nodes to the cluster, allowing it to handle an increasing number of MQTT messages and clients.
2. **High Availability**: Running in a cluster provides high availability, as the cluster can continue to function even if one or more nodes fail. EMQX uses a distributed architecture that ensures no single point of failure.
3. **Load Balancing**: EMQX nodes in the cluster can be configured to distribute the load of handling MQTT messages, which helps to avoid overload of a single node and allows for better use of available resources.
4. **Centralized Management**: EMQX can be managed centrally, as all nodes in the cluster can be monitored and controlled from a single management console. This makes it easy to manage a large number of devices and messages.

Overall, EMQX cluster can help improve the scalability, availability, reliability and management of IoT messaging systems, which is why clustering is recommended for larger or mission-critical applications.

The basic function of a distributed EMQX cluster is to forward and publish messages to different subscribers, as shown below.

EMQX 5.0 adopts a new [Mria cluster architecture](./mria-introduction.md). With this Mria architecture, one EMQX node can support up to 5 million MQTT connections, and the EMQX cluster can support up to 100 million concurrent MQTT connections.



![EMQX_cluster](./assets/EMQX_cluster.png)



## Key features

EMQX added an abstraction layer with the [Ekka](https://github.com/emqx/ekka) library on top of distributed Erlang, enabling features like auto discovery of EMQX nodes, auto cluster, network partition, autoheal, and autoclean.

### Node discovery and auto Clustering

EMQX supports several node discovery strategies:

| Strategy | Description                             |
| -------- | --------------------------------------- |
| `manual` | Manually create a cluster with commands |
| `static` | Autocluster through static node list    |
| `mcast`* | Create a cluster through UDP multicast  |
| `DNS`    | Autocluster through DNS A record        |
| `etcd`   | Autocluster through etcd                |
| `k8s`    | Autocluster provided by Kubernetes      |

[^*]: The multicast discovery strategy has been deprecated and will be removed in future releases.

### Network partition autoheal

Network partition autoheal is a feature of EMQX that allows the broker to recover automatically from network partitions without requiring any manual intervention, suitable for mission-critical applications where downtime is not acceptable.

To enable EMQX network partition autoheal, you can work with `cluster.autoheal` configuration item in `emqx.conf`:

```bash
cluster.autoheal = on
```

Once enabled, EMQX will continuously monitor the connectivity between nodes in the cluster. If a network partition is detected, EMQX will isolate the affected nodes and continue to operate with the remaining nodes. Once the network partition is resolved, the broker will automatically re-integrate the isolated nodes into the cluster.

<!--The work process is as follows:-->

1. <!--The node receives `inconsistent_database` from Mnesia and waits 3 seconds before starting the network partition confirmation;-->

2. <!--After the node confirms the network partition, it reports to the Leader node (the cluster node that starts first);-->

3. <!--After the Leader node delays for a while, it creates a-->
   <!--SplitView when all nodes are online;-->

4. <!--The Leader node selects the self-healing coordinator node in the majority partition;-->

5. <!--The coordinator node restarts the nodes in the minority partition to restore the cluster.-->

   <!--I think we can replace the above paragraph with a diagram-->

### Cluster node autoclean

Cluster node autoclean feature will automatically remove the disconnected nodes from the cluster after the configured time interval. This feature helps to ensure that the cluster is running efficiently and prevent performance degradation over time.

This feature is enabled by default, you can customize the waiting period before removing the disconnected nodes. Default: `5m`

```bash
cluster.autoclean = 5m
```

### Session across nodes

The session across nodes feature ensures the client sessions will not be lost even during client's disconnection. To use this feature, you should first set `clean session` to `false` on the client side, then EMQX will keep the previous session data associated with the Client ID when the client disconnects. If this client reconnects, EMQX will resume the previous sessions, deliver any messages that were queued during the client's disconnection, and maintain the client's subscriptions.

### Pseudo-distributed cluster 

EMQX also provides a pseudo-distributed cluster feature for testing and development purposes. It refers to a cluster setup where multiple instances of EMQX are running on a single machine, with each instance configured as a node in the cluster. 

After starting the first node, use the following command to start the second node and join the cluster manually. To avoid port conflicts, we need to adjust some listening ports:

```bash
EMQX_NODE__NAME='emqx2@127.0.0.1' \
    EMQX_STATSD__SERVER='127.0.0.1:8124' \
    EMQX_LISTENERS__TCP__DEFAULT__BIND='0.0.0.0:1882' \
    EMQX_LISTENERS__SSL__DEFAULT__BIND='0.0.0.0:8882' \
    EMQX_LISTENERS__WS__DEFAULT__BIND='0.0.0.0:8082' \
    EMQX_LISTENERS__WSS__DEFAULT__BIND='0.0.0.0:8085' \
    EMQX_DASHBOARD__LISTENERS__HTTP__BIND='0.0.0.0:18082' \
    EMQX_NODE__DATA_DIR="./data2" \
./bin/emqx start

./bin/emqx_ctl cluster join emqx1@127.0.0.1
```

The above code example is to create a cluster manually, you can also refer to the [auto clustering](#auto-clustering) section on how to create a cluster automatically. 

## Network and hardware specifications

Below is the network requirements and hardware specifications recommend to run EMQX clusters.

**Network**

Network latency: < 10 ms. The cluster will not be available if the latency is higher than 100 ms. 

The core nodes should be under the same private network. In Mria+RLOG mode, it is also recommended to deploy the replicant nodes in the same private network. 

**CPU and memory**

You can use the [Server Estimate](https://www.emqx.com/en/server-estimate) to calculate the CPU and memory resources needed under various connections and Pub&Sub TPS. It is recommended to configure a higher memory of the Core nodes. 

<!--EMQX is powered by [Erlang/OTP](https://www.erlang.org/), the programming language platform Ericsson developed for telecommunication equipment systems. Before we talk about EMQX clustering, let's first talk about what is Erlang/OTP.-->

## <!--Erlang/OTP and Erlang node-->

<!--Telecom equipment, such as routers and access gateways, are usually distributed systems with the main control board and multiple business boards connected via the backplane.-->

<!--The distributed programs of the Erlang/OTP platform are multiple distributed yet interconnected Erlang runtime systems.-->
<!--Each Erlang runtime system is called a node. Nodes are interconnected with TCP to form a network (or a cluster).-->

<!--Erlang nodes are identified by a unique node name, which consists of two parts separated by `@`:-->

```bash
<name>@<ip-address-or-FQDN>
```

<!--Communication between nodes is addressed by node name. Next, we will illustrate how to create nodes and clusters in Erlang REPL.-->

1. <!--Start four shell terminals locally and then use the `-name` parameter to start four Erlang nodes with the same `cookie`:-->

```bash
erl -name node1@127.0.0.1 -setcookie my_nodes
erl -name node2@127.0.0.1 -setcookie my_nodes
erl -name node3@127.0.0.1 -setcookie my_nodes
erl -name node4@127.0.0.1 -setcookie my_nodes
```

2. <!--Visit the console (`node1@127.0.0.1`) and check the name of the current node and connected nodes, among which, `node().` is to check the node name and `nodes().` is to check the connected nodes.-->

```bash
(node1@127.0.0.1) 4> node().
'node1@127.0.0.1'

(node1@127.0.0.1) 4> nodes().
[]
```

3. <!--Let `node1` initiate a connection to other nodes:-->

```bash
(node1@127.0.0.1) 1> net_kernel:connect_node('node2@127.0.0.1').
true
(node1@127.0.0.1) 2> net_kernel:connect_node('node3@127.0.0.1').
true
(node1@127.0.0.1) 3> net_kernel:connect_node('node4@127.0.0.1').
true
```

4. <!--Rerun the command in step 2 and recheck the connected nodes.-->

```bash
(node1@127.0.0.1) 4> nodes().
['node2@127.0.0.1','node3@127.0.0.1','node4@127.0.0.1']
```

<!--We can see that `node2`, `node3`, and `node4` have established a distributed connection with `node1`, and these four nodes form a cluster.-->

<!--Whenever a new node joins the cluster, it will establish a TCP-->
<!--connection with all the nodes in the cluster. Connection among these 4 nodes is shown below:-->

<!--<img src="./assets/cluster_1.png" alt="image" style="zoom:33%;" />-->

## <!--Distributed EMQX cluster-->

<!--The basic function of a distributed EMQX cluster is to forward and publish messages to different subscribers, as shown below.-->

<!--<img src="../../assets/design_9.png" alt="image" style="zoom:33%;" />-->

<!--To achieve this, EMQX maintains several data structures in [embedded database](./mria-introduction.md):-->

- <!--Subscription table-->
- <!--Routing table-->
- <!--Topic tree-->

### <!--Subscription table: topics-subscribers-->

<!--EMQX maintains a subscription table to store the topic-\> subscriber mapping and ensure the incoming messages are routed to the correct clients. This data is only stored on the EMQX node where the subscribers are located. The table scheme is as follows:-->

<!--<!-- TODO 数据分区跟这个有关吗 -->-->

```bash
node1:

    topic1 -> client1, client2
    topic2 -> client3

node2:

    topic1 -> client4
```

### <!--Route table: topic-node-->

<!--The route table stores the mapping between the topic and the node, that is, the topic list of each client on all nodes, and ensures the incoming messages are routed to the correct clients. This data will be duplicated among all nodes within the cluster. The table scheme is as follows:-->

```bash
topic1 -> node1, node2
topic2 -> node3
topic3 -> node2, node4
```

### <!--Topic tree: topic matching with wildcards-->

<!--Topic tree has a hierarchical data structure. It stores information on topic hierarchies for matching messages to subscribed clients.-->

<!--This data will be duplicated among all nodes within the cluster. Below is a topic tree example:-->

| <!--Client-->  | <!--Node-->  | <!--Subscribed topic--> |
| -------------- | ------------ | ----------------------- |
| <!--client1--> | <!--node1--> | <!--t/+/x, t/+/y-->     |
| <!--client2--> | <!--node2--> | <!--t/#-->              |
| <!--client3--> | <!--node3--> | <!--t/+/x, t/a-->       |

<!--When all subscriptions are completed, EMQX will maintain the following topic tree table and route table:-->

<!--<img src="./assets/cluster_2.png" alt="image" style="zoom:33%;" />-->

### <!--Message distribution process-->

<!--When an MQTT client publishes a message, the node where it is located retrieves the route table and forwards the message to the target node according to the message topic.-->
<!--The target node then retrieves the local subscription table and sends the message to the target subscribers.-->

<!--For example, when `client1` publishes a message to topic `t/a`, the routing and distribution of the message between nodes are as follows:-->

1. <!--`client1` publishes a message with topic `t/a` to `node1`.-->

2. <!--`node1` queries the topic tree and locate `t/#` and `t/a` that match topic `t/a`.-->

3. <!--`node1` queries the route table and finds:-->

   - <!--Topic `t/#` is subscribed by some clients on `node2`;-->

   - <!--Topic `t/a` is subscribed by some clients on `node3 `;-->


   <!--So `node1` will forward the message to `node2` and `node3`.-->

4. <!--`node2` receives the forwarded `t/a` message, queries the local subscription table, and then distributes the message to clients subscribed to the topic.-->

5. <!--`node3` receives the forwarded `t/a` message, queries the local subscription table, and then distributes the message to clients subscribed to the topic.-->

6. <!--Message forwarding and distribution are finished.-->

### <!--Data partition and sharing-->

<!--<!-- TODO 何为分区存放，用处是什么 -->-->

<!--EMQX's subscription table is partitioned in the cluster, while the topic tree and routing table are replicated within the cluster.-->

## <!---->
