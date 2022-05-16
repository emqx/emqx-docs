# Cluster Security

When to comes to security of the EMQX cluster there two primary
aspects to consider.

* Secure the ports each node listens on for clustering.
* Keep the Erlang cookie secret, see `node.cookie` config.

::: tip Tip
It's a good practice keep the clustering ports internal by configuring
firewall rules e.g. AWS security groups, or iptables.
:::

## Intra-cluster communication ports

To form a cluster, EMQX nodes need to connect to each other through some conventional
port numbers.

If there is a firewall between the cluster nodes, the conventional listening ports
should be allowed for other nodes in the cluster to reach.

There are two different channels for EMQX nodes to communicate with each other.

### The Erlang Distribution Ports

::: tip Tip
EMQX uses a conventional port mapping mechanism,
but does **NOT** use [Erlang Port Mapper Daemon, EPMD](https://www.erlang.org/doc/man/epmd.html)
:::

Erlang distribution port: `ListeningPort = BasePort + Offset`,
where `BasePort` is 4370 (which is not made configurable), and `Offset` is the numeric
suffix of the node's name. If the node name does not have a numeric suffix, `Offsset` is 0.

For example, having `node.name = emqx@192.168.0.12` in `emqx.conf` should make the
node listen on port `4370`, and port  `4371` for `emqx1` (or `emqx-1`), and so on.

### The Cluster RPC Port

By default, each emqx node also listens on a (conventional) port for the RPC channels,
which should be allowed by the firewall.

The port mapping rule is similar to the port mapping rules for Erlang distribution,
only `BasePort` is `5370`.

That is, having `node.name = emqx@192.168.0.12` in `emqx.conf` should make the node
listen on port `5370`, and port `5371` for `emqx1` (or `emqx-1`), and so on.

::: tip
EMQX in a docker container uses static port `5369` for cluster RPC.
:::

### Using TLS for Cluster PRC connections

It is possible to enable TLS encryption for the backplane connections. It comes at the cost of increased CPU load, though.

1. Create a root CA using `openssl` tool:

   ```
   # Create self-signed root CA:
   openssl req -nodes -x509 -sha256 -days 1825 -newkey rsa:2048 -keyout rootCA.key -out rootCA.pem -subj "/O=LocalOrg/CN=LocalOrg-Root-CA"
   ```

2. Generate CA-signed certificates for the nodes using the rootCA.pem created at step 1:

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
   openssl x509 -req -CA rootCA.pem -CAkey rootCA.key -in domain.csr -out domain.pem -days 365 -CAcreateserial -extfile domain.ext
   ```
   All the nodes in the cluster must use certificates signed by the same CA.

3. Put the generated `domain.pem`, `domain.key` and `rootCA.pem` files to `/var/lib/emqx/ssl` on each node of the cluster.
   Make sure the emqx user can read these files, and permissions are set to `600`.

4. Ensure the following configs in `emqx.conf`.

   ```
   rpc.driver=ssl
   rpc.default_client_driver=ssl
   rpc.certfile=/var/lib/emqx/ssl/domain.pem
   rpc.cacertfile=/var/lib/emqx/ssl/rootCA.pem
   rpc.keyfile=/var/lib/emqx/ssl/domain.key
   rpc.enable_ssl=5369 # TODO this is not added to 5.0 yet
   ```
