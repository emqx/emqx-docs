# Cluster

In EMQX, cluster is a group of EMQX nodes that work together to provide a highly scalable and fault-tolerant MQTT messaging system. Clustering allows you to distribute the load across multiple nodes and ensure that the system can continue to operate even if one or more nodes fail.

## Node Names

Before starting the cluster creation step, let's first get familiar with the concept of node names in EMQX. EMQX nodes are identified by their names. A node name consists of two parts, node name and host, separated with `@`, for example, `emqx@s1.emqx.io`. The host part must either be the IP address or a fully qualified domain name (FQDN), such as `myhost.example.tld`, for instance:

- For EMQX node deployed on server `s1.emqx.io`, the node name should be `emqx@s1.emqx.io`;
- If this server has a static IP (`192.168.0.10`), the node name should be `emqx@192.168.0.10`.

When configuring nodes with `emqx.conf`, you can work with the code below:

```bash
node {
  name = "emqx@s1.emqx.io"
  role = core
}
```

Where, 

- `name` refers to the desired node name, for example, `emqx@localhost`.
- `role` refers to the function that EMQX node performs within an EMQX cluster. There are 2 types of roles: core nodes and replicant codes. For a detailed explanation of core nodes and replicant nodes, see [EMQX Clustering - Core and Replicant Nodes](../design/clustering.md). 
  - default value: `core` 
  - optional value: `core` or `replicant`

## Configure Cluster

In EMQX, cluster is a group of EMQX nodes that work together to provide a highly scalable and fault-tolerant MQTT messaging system. Clustering allows you to distribute the load across multiple nodes and ensure that the system can continue to operate even if one or more nodes fail.

This section introduces how to configure an EMQX cluster. You can add the cluster configuration items either on a core or replicant node, if you are working on a replicant node, there are severl but some configuration items, for example, `core_nodes`, only take effect under certain preconditions:

- `node.db_backend` of the node is set to `rlog`, indicating the node uses `rlog` as the database backend. 
- `node.role` is set to `replicant`, indicating this code functions as a replicant node. 
- `node.discovery_strategy` is set to `manual` or `static`, there is no need to set this configuration item if automatic cluster discovery mechanism is used. For a detailed explanation of the node discovery strategy and the corresponding configuration items, see [Create Cluster](../deploy/cluster/create-cluster.md). 

```bash
cluster {
  name = emqxcl
  discovery_strategy = manual
  core_nodes = []
  driver = tcp
  ssl_options {
    certfile = ""
    keyfile = ""
    cacertfile = ""
  }
}
```

Where,

| Configuration Item       | Description                                                  | Default Value | Optional Values                                   |
| ------------------------ | ------------------------------------------------------------ | ------------- | ------------------------------------------------- |
| `name`                   | This sets the name of the cluster                            | `emqxcl`      |                                                   |
| `discovery_strategy`     | This sets the node discovery strategy for the cluster.       | `manual`      | `manual`, `static`, `mcast`, `DNS`, `etcd`, `k8s` |
| `core_nodes`             | This sets the core nodes that this replicant code will connect to.<br>Multiple nodes can be added here, separated with a `,` | --            | --                                                |
| `driver`                 | This sets the transport protocol for inter-EMQX node communication. | `tcp`         | `tcp`, `SSL`                                      |
| `ssl_options`            | This sets the SSL/TLS configuration options for the listener, it has three properties | --            | --                                                |
| `ssl_options.cacertfile` | This sets the path to the file containing the trusted CA (certificate authority) certificates that the listener uses to verify the authenticity of the client certificates. | --            | --                                                |
| `ssl_options.certfile`   | This sets the path to the file containing the SSL/TLS certificate for the listener. | --            | --                                                |
| `ssl_options.keyfile`    | This sets the path to the file containing the private key corresponding to the SSL/TLS certificate. | --            | --                                                |

:::tip

EMQX has offered more configuration items to better serve customized needs, you can continue to read [Configuration Manual](./configuration-manual.md).

:::