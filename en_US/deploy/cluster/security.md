# Cluster Security

When it comes to the security of the EMQX cluster, there two primary
aspects to consider.

* Secure the ports each node listens on for clustering.
* Keep the Erlang cookie secret. See` node.cookie` config.

::: tip Tip
It's a good practice to keep the clustering ports internal by configuring
firewall rules e.g., AWS security groups or iptables.
:::

## Intra-cluster Communication Ports

To form a cluster, EMQX nodes need to connect to each other through some conventional port numbers.

If there is a firewall between the cluster nodes, the conventional listening ports should be allowed for other nodes in the cluster to reach.

There are two different channels for EMQX nodes to communicate with each other.

### The Erlang Distribution Ports

::: tip Tip
EMQX uses a conventional port mapping mechanism, but does **NOT** use [Erlang Port Mapper Daemon, EPMD](https://www.erlang.org/doc/man/epmd.html)
:::

Erlang distribution port: `ListeningPort = BasePort + Offset`, where `BasePort` is 4370 (which is not made configurable), and `Offset` is the numeric suffix of the node's name. If the node name does not have a numeric suffix, `Offsset` is 0.

For example, having `node.name = emqx@192.168.0.12` in `emqx.conf` should make the node listen on port `4370`, and port  `4371` for `emqx1` (or `emqx-1`), and so on.

### The Cluster RPC Port

By default, each emqx node also listens on a (conventional) port for the RPC channels, which should be allowed by the firewall.

The port mapping rule is similar to the port mapping rules for Erlang distribution, only `BasePort` is `5370`.

That is, having `node.name = emqx@192.168.0.12` in `emqx.conf` should make the node listen on port `5370`, and port `5371` for `emqx1` (or `emqx-1`), and so on.

::: tip
EMQX in a docker container uses static port `5369` for cluster RPC.
:::

### Using TLS for Cluster RPC Connections

::: warning
TLS comes at the cost of increased CPU load and RAM usage
:::

To configure TLS for cluster RPC below configs should be set in `emqx.conf`.

Ensure the following configs in `emqx.conf`.

```
rpc {
  driver = ssl
  certfile = /path/to/cert/domain.pem
  cacertfile = /path/to/cert/ca.pem
  keyfile = /path/to/cert/domain.key
}
```

Below are the steps to generate certificates and a self-signed CA.

1. Create a root CA using `openssl` tool:

   ```
   # Create self-signed root CA:
   openssl req -nodes -x509 -sha256 -days 1825 -newkey rsa:2048 -keyout ca.key -out ca.pem -subj "/O=LocalOrg/CN=LocalOrg-Root-CA"
   ```

2. Generate CA-signed certificates for the nodes using the `ca.pem` created at step 1:

   ```
   # Create a private key:
   openssl genrsa -out domain.key 2048
   # Create openssl extfile:
   cat <<EOF > domain.ext
   authorityKeyIdentifier=keyid,issuer
   basicConstraints=CA:FALSE
   subjectAltName = @alt_names
   [alt_names]
   DNS.1 = backplane
   EOF
   # Create a CSR:
   openssl req -key domain.key -new -out domain.csr -subj "/O=LocalOrg"
   # Sign the CSR with the Root CA:
   openssl x509 -req -CA ca.pem -CAkey ca.key -in domain.csr -out domain.pem -days 365 -CAcreateserial -extfile domain.ext
   ```
   All the nodes in the cluster must use certificates signed by the same CA.

3. Put the generated `domain.pem`, `domain.key`, and `ca.pem` files on each cluster node.
   Ensure the emqx user can read these files, and permissions are set to `600`.

### Using TLS for Erlang distribution

::: warning
TLS comes at the cost of increased CPU load and RAM usage
:::

Erlang distribution is used by EMQX nodes to sync database updates and ad-hoc cluster-wide such as collecting runtime metrics etc.

* Make sure to verify the ssl_dist.conf file has the right paths to keys and certificates.
* Ensure config `cluster.proto_dist` is set to `inet_tls`.
