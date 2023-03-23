# Cluster security

Besides [authentication](../../access-control/authn/authn.md) and [authorization](../../access-control/authz/authz.md) mechanisms on the node level, EMQX also leverages the features of port mapping and TLS encryption to ensure the security of client data transmission, message communication between cluster nodes, and enterprise system integrations.

This section introduces port mapping in EMQX and how to configure TLS/SSL encryption for inter-cluster traffic.

::: tip Tip
It's a good practice to keep the clustering ports internal by configuring firewall rules e.g., [AWS security groups](https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html) or [iptables](https://en.wikipedia.org/wiki/Iptables). 

If there is a firewall between the cluster nodes, the conventional listening ports should be allowed for other nodes in the cluster to reach. <!--I think more content should be added about the firewall setting-->
:::

## Set Node Cookies

For security concerns, you should change the default cookie settings to a Secret cookie in `emqx.conf` on all nodes to join the cluster. 

Note: All nodes to join the cluster should use the same Secret cookie. For details about the magic cookie used, see [Distributed Erlang - Security](https://www.erlang.org/doc/reference_manual/distributed.html#security). 

```
node {
  cookie = "<a Secret cookie>"
}
```

## Port Mapping

EMQX uses a port mapping rule for clustering to ensure that the communication between nodes is reliable and efficient. EMQX nodes communicate with each other through two different channels, Erlang Distribution ports and Cluster RPC ports. <!--should we also add the port range-->

| Channel                       | Description                                                  | Default Port                                       |
| ----------------------------- | ------------------------------------------------------------ | -------------------------------------------------- |
| **Erlang Distribution Ports** | For node communications                                      | `4370`                                             |
| **Cluster RPC Ports**         | For node administrative tasks, such as node joining or leaving | `5370` or<br>`5369` if EMQX is deployed via Docker |

EMQX applies the same port mapping rule for Erlang Distribution Ports and Cluster RPC Ports, which is: 

```
ListeningPort = BasePort + Offset
```

The offset is calculated based on the numeric suffix of the node's name. If the node's name does not have a numeric suffix, then the offset is set to 0. For example:

- For node `emqx@192.168.0.12`, it does not have a numeric suffix, the port will be `4370` for Erlang Distribution Ports (or `5370` for Cluster RPC Ports). 
- For node `emqx1@192.168.0.12`, the numeric suffix is 1, the port will be `4371`  (or `5371` for Cluster RPC Ports). 



## Configure TLS to Secure Cluster Connections

EMQX also supports using TLS to secure the communication channel between EMQX nodes to protect the confidentiality, integrity, and authenticity of the data exchanged between them. TLS comes at the cost of increased CPU load and RAM usage, please configure as per your business needs. 

This section introduces how to configure TLS for EMQX clusters. On how to obtain an SSL/TLS certificate, see [Enable SSL/TLS Connection](../../network/emqx-mqtt-tls.md). 

### Use TLS for Cluster RPC Connections

To configure TLS for cluster RPC below configs should be set in `emqx.conf`.

```
rpc {
  driver = ssl
  certfile = /path/to/cert/domain.pem
  cacertfile = /path/to/cert/ca.pem
  keyfile = /path/to/cert/domain.key
}
```

### Use TLS for Erlang distribution

::: tip
TLS comes at the cost of increased CPU load and RAM usage
:::

EMQX core nodes use Erlang distribution to synchronize database updates and manage nodes in the cluster, such as starting/stopping a component or collecting runtime metrics etc.

* Make sure to verify `etc/ssl_dist.conf` file has the right paths to keys and certificates.
* Ensure config `cluster.proto_dist` is set to `inet_tls`.

