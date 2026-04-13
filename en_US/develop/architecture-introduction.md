# Architecture and Design

This section introduces the design principles of some key EMQX features, including:

- [EMQX Clustering](./cluster/introduction.md)

  EMQX has demonstrated impressive performance on a single node, as evidenced by our benchmark tests, which achieved millions of connections. However, to ensure reliability and availability, EMQX must scale out by forming a cluster. This chapter discusses the complexities of MQTT broker clustering and how EMQX is designed to address these challenges.

- [MQTT Durable Sessions](./durability_introduction.md) (EMQX Enterprise feature) introduces the architecture behind persistent MQTT sessions in EMQX, explaining how session state and messages are durably stored to survive node restarts and network disruptions.

- [Inflight Window and Message Queue](./design/inflight-window-and-message-queue.md)

  To improve message throughput and reduce the impact of network fluctuations, EMQX allows multiple unacknowledged QoS 1 and QoS 2 packets to exist on the network link at the same time. These sent but unconfirmed packets will be stored in the Inflight Window until an acknowledgment is complete. When the length limit of the Inflight Window is reached, these packets will be stored in the Message Queue. This section will introduce the design principles and the relevant configuration items.

- [Message Retransmission](./design/retransmission.md)

  Message Retransmission is part of the MQTT protocol specification. This section will introduce the basic configuration, protocol specification, and design.