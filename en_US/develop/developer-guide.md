---
prev:
  text: 'LLM Resources'
  link: '../get-started/llms-txt'
---

# Developer Guide

The Developer Guide is designed to help developers get started with EMQX and build IoT applications on top of it. This chapter covers client connectivity, API usage, data processing, integration with external systems, and advanced protocol features. The main contents in this chapter include:

- [Client SDK](./connect-emqx/introduction.md) provides step-by-step instructions and code samples for connecting to EMQX using popular MQTT client libraries in C, Java, Go, Python, and JavaScript.

- [Tutorials](./tutorial/tutorial.md) offers practical guides on a wide range of topics including client connection, data collection, MQTT communication optimization, integrations, security, and deployment.

- [Rule Engine](./data-integration/rules.md) introduces the built-in data processing engine that lets you extract, filter, enrich, and transform IoT data in real time, working in conjunction with Data Integration.

- [Data Integration](./data-integration/data-bridges.md) explains how to connect EMQX with external data systems, including databases, message queues, and cloud services, using Sink and Source components.

- [Flow Designer](./flow-designer/introduction.md) (EMQX Enterprise feature) is a visual, no-code tool for building data processing pipelines by connecting rules, actions, and integrations in a graphical interface.

- [Advanced Features](./advanced-feature.md) introduces additional protocol capabilities in EMQX, including MQTT over WebSocket, MQTT over QUIC, Cluster Linking, File Transfer over MQTT, Multi-Protocol Gateway, and client attributes.

- [Architecture](./architecture-introduction.md) covers the design principles behind key EMQX internals, including clustering, MQTT Durable Sessions, the inflight window and message queue, and message retransmission.

- [MQTT Reference](./mqtt-reference.md) is a comprehensive reference for the MQTT protocol, covering versions, terminology, features, and reason codes.
