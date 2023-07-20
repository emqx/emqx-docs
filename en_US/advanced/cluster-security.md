# Cluster Security

EMQX provides several security mechanisms to ensure the confidentiality, integrity, and availability of data, including [authentication](../advanced/auth.md) and [authorization](../advanced/acl.md) mechanisms on the node level, [a secret cookie](#set-node-cookie) to ensure secure communication between nodes in a cluster, and [firewall settings](#firewall-settings) to regulate cluster node communication. And we will also touch on the [EMQX cluster protocol setting](#emqx-cluster-protocol-setting).

## Set Node Cookie

Cookies are used for interconnection authentication between Erlang nodes. A cookie is a string, and only two nodes with the same cookie can establish a connection. For details, see [Distributed Erlang](http://erlang.org/doc/reference_manual/distributed.html).

For security concerns, you should change the default cookie settings to a Secret cookie in `emqx.conf` on all nodes to join the cluster.

```hcl
node {
  cookie = "<a Secret cookie>"
}
```

## Firewall Settings

### Node Discovery Ports

If the environment variable `WITH_EPMD=1` is set in advance, the epmd (listening port 4369) will be enabled for node discovery when EMQX is started, which is called `epmd mode`.

If the environment variable `WITH_EPMD` is not set, epmd is not enabled when EMQX is started, and EMQX ekka is used for node discovery, which is also the default method of node discovery since version 4.0. This is called `ekka mode`.

**epmd Mode**

If there is a firewall between cluster nodes, the firewall needs to open TCP port 4369 for each node, to allow peers to query each other's listening port. The firewall should also allow nodes to connect to the port in the configurable range from `node.dist_listen_min` to `node.dist_listen_max` (inclusive, default is `6369` for both)

**ekka Mode (Default Mode Since Version 4.0：**

In `ekka` mode, the port mapping is conventional, but not dynamic as in `epmd` mode.
The configurations of `node.dist_listen_min` and `node.dist_listen_max` takes no effect in this case.

If there is a firewall between the cluster nodes, the conventional listening port should be allowed
for nodes to connect to each other. See below for the port mapping rule in `ekka` mode.

Erlang distribution port mapping rule in `ekka` mode: `ListeningPort = BasePort + Offset`,
where `BasePort` is 4370 (which is not made configurable), and `Offset` is the numeric suffix of the node's name. If the node name does not have a numeric suffix, `Offsset` is 0.

For example, having `node.name = emqx@192.168.0.12` in `emqx.conf` should make the node listen on port `4370`, and port  `4371` for `emqx1` (or `emqx-1`), and so on.

### Cluster RPC Port

Each EMQX node also listens on a (conventional) port for the RPC channels, which should also be allowed by the firewall. The port mapping rule is similar to the node discovery ports in `ekka mode`, but with the `BasePort = 5370`. That is, having
`node.name = emqx@192.168.0.12` in `emqx.conf` should make the node listen on port `5370` and port `5371` for `emqx1` (or `emqx-1`), and so on.

## EMQX Cluster Protocol Setting

Each node in the Erlang cluster can be connected through TCPv4, TCPv6, or TLS, and the connection method can be configured in`etc/emqx.conf`:

| Configuration name    | Type      | Default value       | Description                                                  |
| --------------------- | --------- | ------------------- | ------------------------------------------------------------ |
| cluster.proto_dist    | enum      | `inet_tcp`          | Distributed protocol with optional values are as follows:<br />  - inet_tcp: use TCP IPv4<br/>  - inet6_tcp: use TCP IPv6<br/>  - inet_tls: use TLS |
| node.ssl_dist_optfile | file path | `etc/ssl_dist.conf` | When `cluster.proto_dist` is selected as inet_tls, you need to configure the ` etc/ssl_dist.conf` file, and specify the TLS certificate. |



