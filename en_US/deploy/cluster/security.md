# Cluster security

Security is the cornerstone of all IoT applications and platforms. EMQX adopts multiple protection mechanisms to ensure data and privacy security, for example, EMQX supports multiple [authentication](../../access-control/authn/authn.md) and [authorization](../../access-control/authz/authz.md) mechanisms on the node level, it also leverages the features of Port mapping and TLS encryption to ensure the data transmission security of client data transmission, message communication between cluster nodes, and enterprise system integrations.

This section introduces port mapping in EMQX and how to configure TLS/SSL encryption. 

::: tip Tip
It's a good practice to keep the clustering ports internal by configuring firewall rules e.g., [AWS security groups](https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html) or [iptables](https://en.wikipedia.org/wiki/Iptables). 

If there is a firewall between the cluster nodes, the conventional listening ports should be allowed for other nodes in the cluster to reach. <!--I think more content should be added about the firewall setting-->
:::

## Set Node Cookies

For security concerns, you should change the default cookie settings to `emqxsecretcookie` in `emqx.conf` on all nodes to join the cluster. 

Note: All nodes to join the cluster should use the same security cookie. For details about the magic cookie used, see [Distributed Erlang - Security](https://www.erlang.org/doc/reference_manual/distributed.html#security). 

```
node {
  cookie = "emqxsecretcookie"
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

This section introduces how to configure TLS for EMQX clusters. On how to obtain a SSL/TLS certificate, see [Enable SSL/TLS Connection](../../network/emqx-mqtt-tls.md). 

### Using TLS for Cluster RPC Connections

To configure TLS for cluster RPC below configs should be set in `emqx.conf`.

```
rpc {
  driver = ssl
  certfile = /path/to/cert/domain.pem
  cacertfile = /path/to/cert/ca.pem
  keyfile = /path/to/cert/domain.key
}
```

### Using TLS for Erlang distribution

::: tip
TLS comes at the cost of increased CPU load and RAM usage
:::

Erlang distribution is used by EMQX core nodes to sync database updates
and control/management RPCs such as start/stop a componentk, or collecting runtime metrics etc.

* Make sure to verify `etc/ssl_dist.conf` file has the right paths to keys and certificates.
* Ensure config `cluster.proto_dist` is set to `inet_tls`.

<!--Below are the steps to generate certificates and a self-signed CA.-->

<!--Create a root CA using `openssl` tool:-->

```
# Create self-signed root CA:
openssl req -nodes -x509 -sha256 -days 1825 -newkey rsa:2048 -keyout ca.key -out ca.pem -subj "/O=LocalOrg/CN=LocalOrg-Root-CA"
```

<!--Generate CA-signed certificates for the nodes using the `ca.pem` created at step 1:-->

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
<!--All the nodes in the cluster must use certificates signed by the same CA.-->

<!--Put the generated `domain.pem`, `domain.key`, and `ca.pem` files on each cluster node.-->
<!--Ensure the emqx user can read these files, and permissions are set to `600`.-->

