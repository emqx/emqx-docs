# EMQX Cluster Linking

Cluster Linking is a feature that allows bridging multiple separate EMQX clusters together, enabling communication between clients talking to different, potentially geographically distributed clusters. In contrast to the traditional MQTT bridging approaches, Cluster Linking is more efficient, reliable, and scalable: it minimizes the bandwidth requirements and tolerates network interruptions.

A single EMQX cluster is perfectly fine serving thousands of geographically distributed MQTT clients, but when they are spread across the globe, a challenge arises: some clients will inevitably struggle with high latency and poorer network connectivity. In such cases, it is usually better to deploy multiple EMQX clusters in different regions to serve clients locally. However, this approach introduces a new challenge: how to enable communication between clients connected to different clusters.

A classic solution to this problem is adding an MQTT bridge on each cluster, which forwards messages between the clusters. However, this comes with a price: if you want fully bridged communication, you need to forward basically all messages between clusters, which can lead to excessive bandwidth usage. Even worse, significant fraction of this bandwidth might be wasted on messages that are not relevant to the clients on the other side of the bridge, while increasing the latency of the messages that are actually relevant.

This is exactly what Cluster Linking is designed to solve. Instead of forwarding all messages between clusters, Cluster Linking optimizes the communication by forwarding only the relevant messages, and by doing so in a way that tolerates network interruptions.
