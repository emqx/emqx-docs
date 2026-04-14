# Cluster Security

EMQX provides several security mechanisms to ensure the confidentiality, integrity, and availability of data, including [authentication](../../access-control/authn/authn.md) and [authorization](../../access-control/authz/authz.md) mechanisms on the node level, [a secret cookie](#set-node-cookie) to ensure secure communication between nodes in a cluster, and [TLS/SSL encryption](#configure-tls-ssl-to-secure-cluster-connections) to provide end-to-end encryption for inter-node traffic.

To help provision firewall rules, we will also touch on EMQX's inter-broker [communication port mapping rules](#port-mapping). 

## Set Node Cookie

For security concerns, you should change the default cookie settings to a Secret cookie in `emqx.conf` on all nodes to join the cluster. 

Note: All nodes to join the cluster should use the same Secret cookie. For details about the magic cookie used, see [Distributed Erlang - Security](https://www.erlang.org/doc/reference_manual/distributed.html#security). 

```
node {
  cookie = "<a Secret cookie>"
}
```

:::tip Tip 

All nodes to join the cluster should use the same security cookie. For details about the magic cookie used, see [Distributed Erlang - Security](https://www.erlang.org/doc/reference_manual/distributed.html#security). 

:::

## Configure TLS/SSL to Secure Cluster Connections

EMQX also supports using TLS to secure the communication channel between EMQX nodes to protect the confidentiality, integrity, and authenticity of the data exchanged between them. TLS comes at the cost of increased CPU load and RAM usage, please configure as per your business needs. 

This section introduces how to configure TLS for EMQX clusters. On how to obtain an SSL/TLS certificate, see [Enable SSL/TLS Connection](../../network/emqx-mqtt-tls.md). 

## Use TLS/SSL for Cluster RPC Connections

To configure TLS/SSL for cluster RPC below configuration items should be set in `emqx.conf`.

```
rpc {
  driver = ssl
  # PEM format file containing the trusted CA (certificate authority) certificates that the listener uses to verify the authenticity of the cluster peers.
  cacertfile = "/path/to/cert/ca.pem"
  # PEM format file containing the SSL/TLS certificate chain for the listener. If the certificate is not directly issued by a root CA, the intermediate CA certificates should be appended after the listener certificate to form a chain.
  certfile = "/path/to/cert/domain.pem"
  # PEM format file containing the private key corresponding to the SSL/TLS certificate
  keyfile = "/path/to/cert/domain.key"
  # Set to 'verify_peer' to verify the authenticity of the clients' certificates, otherwise 'verify_none'.
  verify = verify_peer
  # If set to true, the handshake fails if the peer does not have a certificate to send, that is, sends an empty certificate. If set to false, it fails only if the peer sends an invalid certificate (an empty certificate is considered valid).
  fail_if_no_peer_cert = true
}
```

### Use TLS/SSL for Erlang Distribution

EMQX core nodes use Erlang distribution to synchronize database updates and manage nodes in the cluster, such as starting/stopping a component or collecting runtime metrics, etc.

* Make sure to verify `etc/ssl_dist.conf` file has the right paths to keys and certificates.
* Ensure config `cluster.proto_dist` is set to `inet_tls`.

## Port Mapping

It's a good practice to keep the clustering ports internal by configuring firewall rules e.g., [AWS security groups](https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html) or [iptables](https://en.wikipedia.org/wiki/Iptables). If there is a firewall between the cluster nodes, the conventional listening ports should be allowed for other nodes in the cluster to reach. This section introduces the port mapping rules, which ensure that the firewall rules are configured correctly, allowing EMQX nodes to connect to each other while preventing unauthorized access from external sources.

EMQX uses a port mapping rule for clustering to ensure that the communication between nodes is reliable and efficient. EMQX nodes communicate with each other through two different channels, Erlang Distribution ports and Cluster RPC ports. 

| Channel                       | Description                                                  | Default Port                                       |
| ----------------------------- | ------------------------------------------------------------ | -------------------------------------------------- |
| **Erlang Distribution Ports** | For node communications                                      | `4370`                                             |
| **Cluster RPC Ports**         | For node administrative tasks, such as node joining or leaving | `5370` or<br />`5369` if EMQX is deployed via Docker |

EMQX applies the same port mapping rule for Erlang Distribution Ports and Cluster RPC Ports, which is: 

```
ListeningPort = BasePort + Offset
```

The offset is calculated based on the numeric suffix of the node's name. If the node's name does not have a numeric suffix, then the offset is set to 0. For example:

- For node `emqx@192.168.0.12`, it does not have a numeric suffix, the port will be `4370` for Erlang Distribution Ports (or `5370` for Cluster RPC Ports). 
- For node `emqx1@192.168.0.12`, the numeric suffix is 1, the port will be `4371`  (or `5371` for Cluster RPC Ports). 

## Mitigate SSRF with Firewall Rules

Administrative features and integrations may require EMQX to open outbound network connections. If delegated administrators can configure namespace-scoped resources, use host-level egress filtering so EMQX nodes can reach only approved destinations.

This does not replace EMQX role-based access control, but it adds an important defense-in-depth layer against SSRF by preventing the broker host from contacting arbitrary endpoints.

When planning egress restrictions:

- Allow only the destinations required by your deployment, such as identity providers, webhooks, and connector backends.
- Deny access to sensitive addresses that are commonly abused in SSRF attacks, such as loopback, link-local, and instance metadata endpoints, unless your deployment explicitly requires them. In particular, consider blocking the following metadata endpoints:
  - `100.100.100.200` for Alibaba Cloud metadata service
  - `169.254.169.253` for the AWS external metadata service
  - `169.254.169.254` for AWS and Azure metadata service
  - `fd00:ec2::254` for the AWS IPv6 metadata service
- If you use AWS IAM authentication for Amazon MSK on EC2, do not block `169.254.169.254`, because EMQX must reach the instance metadata service to obtain credentials or generate authentication tokens. The same exception should be reflected in your `iptables` or `nftables` rules.
- Validate the rules carefully in staging before applying them to production systems.
- If EMQX runs in containers or Kubernetes, apply equivalent egress controls with the container host firewall, cloud security groups, or Kubernetes network policies.

The following `iptables` example shows the general approach. Adapt the interface names, ports, and destination addresses to match your environment. If `iptables` is not available on your host, apply equivalent rules with `nftables`:

```bash
# Allow established outbound connections
iptables -A OUTPUT -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT

# Allow DNS and NTP if required by the host
iptables -A OUTPUT -p udp --dport 53 -j ACCEPT
iptables -A OUTPUT -p udp --dport 123 -j ACCEPT

# Allow access only to approved external services
iptables -A OUTPUT -p tcp -d 198.51.100.10 --dport 443 -j ACCEPT
iptables -A OUTPUT -p tcp -d 203.0.113.20 --dport 443 -j ACCEPT

# Block common metadata and local-only destinations
iptables -A OUTPUT -d 127.0.0.0/8 -j REJECT
# If you use AWS IAM authentication for Amazon MSK on EC2, do not apply
# this blanket deny to 169.254.169.254. Add a more specific allow rule instead.
iptables -A OUTPUT -d 169.254.0.0/16 -j REJECT
iptables -A OUTPUT -d 100.100.100.200 -j REJECT
ip6tables -A OUTPUT -d fd00:ec2::254 -j REJECT

# Reject all other new outbound connections by default
iptables -A OUTPUT -m conntrack --ctstate NEW -j REJECT
```

If your host uses `nftables` instead of `iptables`, implement the same policy there, including explicit denies for known metadata endpoints such as `100.100.100.200`, `169.254.169.253`, `169.254.169.254`, and `fd00:ec2::254`. If you use AWS IAM authentication for Amazon MSK on EC2, make sure the rules leave `169.254.169.254` reachable.
