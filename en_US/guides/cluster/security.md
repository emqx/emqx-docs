# Cluster Security

EMQX provides several security mechanisms to ensure the confidentiality, integrity, and availability of data, including [authentication](../access-control/authn/authn.md) and [authorization](../access-control/authz/authz.md) mechanisms on the node level, [a secret cookie](#set-node-cookie) to ensure secure communication between nodes in a cluster, and [TLS/SSL encryption](#configure-tls-ssl-to-secure-cluster-connections) to provide end-to-end encryption for inter-node traffic.

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

This section introduces how to configure TLS for EMQX clusters. On how to obtain an SSL/TLS certificate, see [Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md).

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

## Mitigate SSRF with Rule Engine Policy and Firewall Rules

EMQX connectors, bridges, and actions open outbound network connections to external services. Without controls, a misconfigured or malicious target could cause EMQX to make unintended requests to internal or sensitive destinations, a class of vulnerability known as Server-Side Request Forgery (SSRF). EMQX provides two complementary defenses: a built-in rule engine SSRF policy with limited connector coverage, and host-level egress filtering that enforces network boundaries across connector types at runtime.

### Use `rule_engine.ssrf` for HTTP and MQTT Connectors

Starting from EMQX `6.0.3`, EMQX provides a cluster-level SSRF policy for outbound rule engine targets. Starting from EMQX `6.0.4`, the policy applies only to the `url` field of HTTP connectors and the `server` field of MQTT connectors:

```hocon
rule_engine {
  ssrf {
    enable = true
    allow_cidrs = []
    deny_cidrs = [
      "127.0.0.0/8",
      "::1/128",
      "169.254.0.0/16",
      "fe80::/10",
      "10.0.0.0/8",
      "172.16.0.0/12",
      "192.168.0.0/16",
      "fc00::/7",
      "0.0.0.0/32",
      "224.0.0.0/4",
      "ff00::/8",
      "100.100.100.200/32",
      "169.254.169.253/32"
    ]
    deny_hosts = [
      "metadata.tencentyun.com",
      "metadata.google.internal",
      "metadata.azure.internal"
    ]
  }
}
```

To configure the policy in the Dashboard, see [Rule Engine Security](../dashboard/cluster_settings.md#rule-engine-security).

When enabled, EMQX validates these HTTP and MQTT connector targets when a connector configuration is tested, created, or updated. Exact matches in `deny_hosts` are rejected immediately. Resolved IPs are checked against `allow_cidrs` first, and then against `deny_cidrs` if no allowlist match is found.

The policy does not validate other connector types, connector enable or disable operations, connector deletion, or outbound connections at runtime. If a stored HTTP or MQTT connector target becomes blocked after the connector was created, the connector can still be enabled. Deleting a connector is also not blocked by the policy.

This policy is disabled by default for compatibility. Enable it when you want to prevent HTTP and MQTT connector configurations with blocked targets from passing connectivity tests or being created or updated. If these connectors must reach internal services, review and adjust `allow_cidrs` and `deny_cidrs` before enabling the policy.

### When `rule_engine.ssrf` Alone Is Usually Enough

Using `rule_engine.ssrf` alone is usually sufficient when all of the following are true:

- All rule engine targets that require SSRF protection use HTTP or MQTT connectors.
- The policy is enabled before the connectors are created, and only trusted administrators can update their configurations.
- Outbound targets are stable and expected, such as fixed SaaS endpoints or explicitly approved public services.
- You mainly want to prevent accidental misconfiguration or obvious SSRF targets when HTTP or MQTT connector configurations are tested, created, or updated.
- You do not rely on DNS names that could later be rebound to different addresses.

### When You Should Also Add Firewall Rules

Add host-level egress filtering with `iptables`, `nftables`, cloud security groups, or Kubernetes network policies when any of the following apply:

- You use connector types other than HTTP or MQTT.
- Policy changes must apply to stored connector configurations, connector enable operations, or runtime connections.
- Delegated administrators can configure namespace-scoped resources.
- EMQX must not be able to reach internal services, metadata endpoints, or management networks, even if a target passes config-time validation.
- DNS rebinding or post-validation address changes are part of your threat model.
- Your deployment is multi-tenant, higher-risk, or must enforce a strict outbound network boundary at runtime.

When planning egress restrictions:

- Allow only the destinations required by your deployment, such as identity providers, webhooks, and connector backends.
- Deny access to sensitive addresses that are commonly abused in SSRF attacks, such as loopback, link-local, and instance metadata endpoints, unless your deployment explicitly requires them. In particular, consider blocking the following metadata endpoints:
  - `100.100.100.200` for Alibaba Cloud metadata service
  - `169.254.169.253` for the AWS external metadata service
  - `169.254.169.254` for AWS and Azure metadata service
  - `fd00:ec2::254` for the AWS IPv6 metadata service
- If you use AWS-based connectors or actions on EC2 and let EMQX obtain credentials from the instance metadata service by omitting the access key ID and secret access key, do not block `169.254.169.254`. This applies not only to Amazon MSK IAM, but also to integrations such as S3, S3 Tables, DynamoDB, and Kinesis. The same exception should be reflected in your `iptables` or `nftables` rules.
- Validate the rules carefully in staging before applying them to production systems.
- If EMQX runs in containers or Kubernetes, apply equivalent egress controls with the container host firewall, cloud security groups, or Kubernetes network policies.

`rule_engine.ssrf` does not replace these network-layer controls. The SSRF policy validates only HTTP and MQTT connector targets when a connector configuration is tested, created, or updated. Runtime network controls are required for other connector types, stored configurations that are enabled after a policy change, DNS rebinding, and any other case where the resolved address may change after validation.

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
# If AWS-based connectors or actions on EC2 must retrieve credentials from
# the instance metadata service, do not apply this blanket deny to
# 169.254.169.254. Add a more specific allow rule instead.
iptables -A OUTPUT -d 169.254.0.0/16 -j REJECT
iptables -A OUTPUT -d 100.100.100.200 -j REJECT
ip6tables -A OUTPUT -d fd00:ec2::254 -j REJECT

# Reject all other new outbound connections by default
iptables -A OUTPUT -m conntrack --ctstate NEW -j REJECT
```

If your host uses `nftables` instead of `iptables`, implement the same policy there, including explicit denies for known metadata endpoints such as `100.100.100.200`, `169.254.169.253`, `169.254.169.254`, and `fd00:ec2::254`. If AWS-based connectors or actions on EC2 must retrieve credentials from the instance metadata service, make sure the rules leave `169.254.169.254` reachable.
