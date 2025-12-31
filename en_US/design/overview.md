# Design and Implementation

This section introduces the design principles of some key EMQX features, including:

- [Clustering](./clustering.md)

  EMQX has demonstrated impressive performance on a single node, as evidenced by our benchmark tests, which achieved millions of connections.
  However, to ensure reliability and availability, EMQX must scale out by forming a cluster.
  This chapter discusses the complexities involved in MQTT broker clustering and how EMQX is specifically designed to overcome these challenges.

- [Inflight Window and Message Queue](./inflight-window-and-message-queue.md)

  To improve message throughput and reduce the impact of network fluctuations, EMQX allows multiple unacknowledged QoS 1 and QoS 2 packets to exist on the network link at the same time. These sent but unconfirmed packets will be stored in the Inflight Window until an acknowledgment is complete. When the length limit of the Inflight Window is reached, these packets will be stored in the Message Queue. This section will introduce the design principles and the relevant configuration items. 

- [Message Retransmission](./retransmission.md)

  Message Retransmission is part of the MQTT protocol specification. This section will introduce the basic configuration, protocol specification, and design.

- [Durable Storage](./durable-storage.md)

  Durable Storage provides a high-reliability persistence foundation for MQTT data in EMQX. This page explains the design of the Durable Storage subsystem, including its storage architecture, data partitioning model, replication strategy, quota enforcement, and read/write paths, which together enable reliable message storage, replay, and consistency guarantees across the cluster.

More topics will be updated soon. Stay tuned.
