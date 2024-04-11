# Cluster Configuration

In EMQX, a cluster is a group of EMQX nodes that work together to provide a highly scalable and fault-tolerant MQTT messaging system. Clustering allows you to distribute the load across multiple nodes and ensure that the system can continue to operate even if one or more nodes fail.

## Configure Node Names

Before starting the cluster creation step, let's first get familiar with the concept of node names in EMQX. EMQX nodes are identified by their names. A node name consists of two parts, node name and host, separated with `@`, for example, `emqx@s1.emqx.io`. The host part must either be the IP address or a fully qualified domain name (FQDN), such as `myhost.example.tld`, for instance:

- For the EMQX node deployed on server `s1.emqx.io`, the node name should be `emqx@s1.emqx.io`;
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
- `role` refers to the function that an EMQX node performs within an EMQX cluster. There are 2 types of roles: core nodes and replicant codes. For a detailed explanation of core nodes and replicant nodes, see [EMQX Clustering - Core and Replicant Nodes](../design/clustering.md). 
  - Default value: `core` 
  - Optional value: `core` or `replicant`

## Configure Cluster

This section introduces how to configure an EMQX cluster. You can add the cluster configuration items either on a core or replicant node. If you are working on a replicant node, there are some configuration items, for example, `core_nodes`, that only take effect under certain preconditions:

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
| `discovery_strategy`     | This sets the node discovery strategy for the cluster.       | `manual`      | `manual`, `static`, `DNS`, `etcd`, `k8s`          |
| `core_nodes`             | This sets the core nodes that this replicant code will connect to.<br />Multiple nodes can be added here, separated with a `,` | --            | --                                                |
| `driver`                 | This sets the transport protocol for inter-EMQX node communication. | `tcp`         | `tcp`, `SSL`                                      |
| `ssl_options`            | This sets the SSL/TLS configuration options for the listener, it has three properties | --            | --                                                |
| `ssl_options.cacertfile` | PEM file containing the trusted CA (certificate authority) certificates that the listener uses to verify the authenticity of the client certificates. | --            | --                                                |
| `ssl_options.certfile`   | PEM file containing the SSL/TLS certificate chain for the listener. If the certificate is not directly issued by a root CA, the intermediate CA certificates should be appended after the listener certificate to form a chain. | --            | --                                                |
| `ssl_options.keyfile`    | PEM file containing the private key corresponding to the SSL/TLS certificate. | --            | --                                                |
| `ssl_options.fail_if_no_peer_cert` | If set to `true`, the server fails if the client does not have a certificate to send, that is, sends an empty certificate. If set to `false`, it fails only if the client sends an invalid certificate (an empty certificate is considered valid). | --            | --                                                |

{% emqxce %}

EMQX has offered more configuration items to serve customized needs better. For details, see [Configuration Manual](https://www.emqx.io/docs/zh/v@CE_VERSION@/hocon/).

{% endemqxce %}

{% emqxee %}

EMQX has offered more configuration items to serve customized needs better. For details, see [Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/).

{% endemqxee %}
