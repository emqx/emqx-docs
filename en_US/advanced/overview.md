# Design and implementation

This chapter will introduce the design principles of some key EMQX features, including:

- [Inflight Window and Message Queue](./inflight-window-and-message-queue)

  To improve message throughput and reduce the impact of network fluctuations, EMQX allows multiple unacknowledged QoS 1 and QoS 2 packets to existing on the network link at the same time. These sent but unconfirmed packets will be stored in the Inflight Window until an acknowledgment is complete. When the length limit of the Inflight Window is reached, these packets will be stored in the Message Queue. This section will introduce the design principles and the relevant configuration items. 

- [Message retransmission](./retransmission.md)

  Message Retransmission is part of the MQTT protocol specification. This section will introduce the basic configuration, protocol specification, and design.

More topics will be updated soon, stay tuned.
