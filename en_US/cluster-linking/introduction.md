# EMQX Cluster Linking

Cluster Linking is a feature that connects multiple, separate EMQX clusters, facilitating communication between clients on different, often geographically dispersed clusters. Compared to traditional MQTT bridging, Cluster Linking is more efficient, reliable, and scalable. It minimizes bandwidth requirements and tolerates network interruptions.

This section introduces the Cluster Linking and how to use and configure the feature.

## Why Cluster Linking

This section explains how Cluster Linking becomes the solution for efficient client communication between different clusters and why it is more beneficial in bandwidth usage, network tolerance, and scalability compared with the MQTT bridge.

### Challengers with a Single Cluster

A single EMQX cluster can serve thousands of geographically distributed MQTT clients effectively. However, when clients are spread globally, issues with high latency and poor network connectivity arise. Deploying multiple EMQX clusters in different regions can mitigate these problems by serving clients locally. 

### Cluster Linking vs. MQTT Bridge

Deploying multiple EMQX clusters in different regions introduces a new challenge: enabling seamless communication between clients connected to different clusters.

The traditional solution involves adding an MQTT bridge to each cluster, which forwards all messages between clusters. This approach leads to excessive bandwidth usage and can increase message latency, as many forwarded messages might not be relevant to clients on the other side of the bridge.

Cluster Linking addresses these issues by forwarding only relevant messages between clusters. This optimization reduces bandwidth usage and ensures efficient communication, even during network interruptions.

#### Function Comparison with MQTT Bridge

Cluster Linking shares some functionalities with MQTT Bridge. Below is a comparison between the two. This comparison highlights the key differences and similarities between MQTT Bridge and Cluster Linking, helping you to understand the advantages and use cases of each feature.

| Feature                              | MQTT Bridge                                                  | Cluster Linking                                              |
| ------------------------------------ | ------------------------------------------------------------ | ------------------------------------------------------------ |
| **Protocol**                         | MQTT                                                         | MQTT                                                         |
| **Authentication**                   | Follows remote broker                                        | Follows remote broker                                        |
| **Authorization**                    | Follows remote broker                                        | Only needs authorization for control topics.<br />**Control topics**: Data exchanging topics needed for the linking function<br />**Message topics**: Topics that need message replication |
| **Applicable Products**              | Any standard MQTT Broker                                     | Only EMQX                                                    |
| **Message Loopback**                 | Requires support for Bridge Mode                             | Automatically avoided                                        |
| **Namespace**                        | Often requires adding topic prefixes to avoid message loopback | Unified namespace                                            |
| **Custom Topic Prefix**              | Supported                                                    | Not supported                                                |
| **Bridging Method**                  | Supports configuring bridging rules (Pub/Sub) on remote or local<br />The same connection can act as both publisher and subscriber. | Only supports push mode, and needs local configuration to push to remote. Peer configuration is required for mutual communication.<br />The same connection can only act as a publisher. Push mode is more efficient and stable than bidirectional mode. |
| **Message Replication Method**       | Full subscription to specified topics, less efficient        | Replication on demand based on local subscription list       |
| **Rule Engine and Data Integration** | Always triggers message publish events                       | Does not trigger local rule engine message publish events    |

## Workflow and Usage Scenarios

This section describes the Cluster Linking workflow in two different scenarios.

### Replicate Messages from Remote cluster to Local Cluster

In this scenario, Cluster A acts as the local cluster and Cluster B as the remote cluster. Cluster A needs to replicate topics `t/#` and `c/1` from Cluster B.

1. **Client Authentication and Authorization:**
   - Add credentials to the client authentication configurations in both Cluster A and Cluster B.
   - Add authorizations to the client ACLs in both Cluster A and Cluster B.
2. **Configure Cluster Linking in Cluster A:**
   - Enter the MQTT address of Cluster B.
   - Provide the necessary authentication information.
   - Specify the topics to be replicated: `t/#` and `c/1`.
3. **Configure Peer Cluster Linking in Cluster B:**
   - Enter the MQTT address of Cluster A.
   - Provide the necessary authentication information.
   - No need to specify topics (Cluster B does not require messages from Cluster A).
4. **Establish Mutual Connections:**
   - Ensure that both clusters are connected to each other.
5. **Bootstrap Route Information Push (A → B):**
   - Initiate a route information push from Cluster A to Cluster B.
6. **Subscription and Message Flow:**
   - Clients in Cluster A subscribe to topics.
   - Route information is pushed from Cluster A to Cluster B.
   - Clients publish messages to Cluster B.
   - Messages are pushed from Cluster B to Cluster A.
7. **Unsubscription:**
   - Clients in Cluster A unsubscribe from topics.
   - Delete route information push from Cluster A to Cluster B.
8. **Completion:** The configuration process is complete.

### Migrate Old Cluster to New Cluster

In this scenario, Cluster A is the old cluster, and Cluster B is the new cluster. The goal is to replicate all messages fully from Cluster A to Cluster B.

1. **Establish Peer Connection:**
   - Set up a peer connection between Cluster A and Cluster B.
2. **Configure Topics:**
   - Configure the topics to `#` in both clusters.
3. **Establish Mutual Connections:**
   - Ensure that both clusters are connected to each other.
4. **DNS Change/Load Balancer Traffic Switch:**
   - Implement DNS changes or load balancer traffic switching.
5. **Client Migration:**
   - Migrate clients to the new cluster. Messages will not be interrupted, but sessions cannot be migrated, requiring clients to re-subscribe.
6. **Disconnect Cluster A:**
   - Disconnect Cluster A from the network.
7. **New Connection in Cluster B:**
   - Establish new connections in Cluster B, and clients need to initiate active subscriptions again.
8. **Completion:** The migration process is complete.

## Next Steps

Next, you can learn how to use the Cluster Linking feature and configure its functionalities through the following guides:

- [Quick Start with Cluster Linking](./quick-start.md)
- [Configure Cluster Linking](./configuration.md)
