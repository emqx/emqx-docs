# Introduction

**EMQX** is an Open-source MQTT broker with a high-performance real-time message processing engine, powering event streaming for IoT devices at massive scale.

As the most scalable MQTT broker, EMQX can help you connect any device, at any scale. Move and process your IoT data anywhere.

## Benefits

- **Massive Scale**: Scale to 100 million concurrent MQTT connections with a single EMQX 5.0 cluster.
- **High Performance**: Move and process millions of MQTT messages per second in a single broker.
- **Low Latency**: Guarantee sub-millisecond latency in message delivery with the soft real-time runtime.
- **Fully MQTT 5.0**: 100% compliant with MQTT 5.0 and 3.x standard for better scalability, security, and reliability.
- **High Availability**: Achieve high availability and horizontal scalability through a masterless distributed architecture.
- **Cloud-Native & K8s**: Easy to deploy on-premises or in public clouds with Kubernetes Operator and Terraform.

## Features List

- Full MQTT V3.1/V3.1.1 and V5.0 protocol specification support
  - QoS0, QoS1, QoS2 message support
  - Persistent conversation and offline message support
  - Retained message support
  - Last Will message support
- TCP/SSL connection support
- MQTT/WebSocket/SSL support
- HTTP message publishing interface support
- $SYS/# system theme support
- Client online status query and subscription support
- Client ID or IP address authentication support
- User name and password authentication support
- LDAP authentication
- Redis, MySQL, PostgreSQL, MongoDB, HTTP authentication integration
- Browser cookie authentication
- Access control (ACL) based on client ID, IP address, user name
- Multi-server node cluster (Cluster)
- Support manual, mcast, dns, etcd, k8s and other cluster discovery methods
- Automatic network partition healing
- Message rate limit
- Connection rate limit
- Configure nodes by partition
- Multi-server node bridge (Bridge)
- MQTT Broker bridge support
- Stomp protocol support
- MQTT-SN protocol support
- CoAP protocol support
- Stomp/SockJS support
- Delay Publish ($delay/topic)
- Flapping detection
- Blacklist support
- Shared subscription($share/<group\>/topic)
- TLS/PSK support
- Rule engine
  - No action (debug)
  - Message republish
  - Bridge data to MQTT Broker
  - Check (debug)
  - Send data to web service
