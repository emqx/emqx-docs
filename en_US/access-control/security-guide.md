# Security Guide

Security for an EMQX production cluster is a prerequisite for high availability, not a compliance afterthought. Because MQTT is a stateful protocol maintaining long-lived connections, any security vulnerability directly impacts system reliability.

This guide follows a "Defense in Depth" strategy, organizing security controls into five critical layers to ensure your cluster degrades gracefully instead of failing catastrophically under attack.

## Security Hardening Checklist

Before proceeding to granular configurations, use this checklist to assess your current security posture.

### Phase 1: Infrastructure and OS Hardening

The host operating system is the first line of defense against resource exhaustion and volumetric attacks.

- [ ] **Kernel Tuning**: Set `fs.file-max` and `fs.nr_open` > 2 million to ensure the host can allocate enough sockets for massive concurrent connections. For more information, see [Performance Tuning](../performance/tune.md)
- [ ] **Service Limits**: Explicitly set `LimitNOFILE=1048576` (minimum) in `emqx.service` to override restrictive systemd process defaults.
- [ ] **TCP Hardening**: Enable SYN Cookies for flood protection and scale the `nf_conntrack` table to prevent packet drops during reconnection storms.
- [ ] **Client-to-Server Firewall**: Restrict public ingress to 8883 (SSL) and 8084 (WSS). Block or VPN-isolate port **1883** to prevent cleartext data exposure.
- [ ] **Node-to-Node Firewall**: Isolate ports 4370 and 5370 (or 5369 in Docker) to internal Cluster IPs to block unauthorized remote code execution.
- [ ] **Interface Binding**: Configure `inet_dist_use_interface` to bind the Erlang distribution protocol exclusively to a private network interface.

### Phase 2: Erlang and Cluster Security

Hardening the Erlang runtime is critical to defending against lateral movement and unauthorized cluster access.

- [ ] **Cookie**: Replace the default cookie with a high-entropy, 32+ character random string to prevent rogue nodes from joining the cluster.
- [ ] **File Permissions**: Set configuration files containing secrets (e.g., `emqx.conf`) to `400` so they are read-only by the owner.
- [ ] **Inter-node Encryption**: Enable `ssl_dist` to ensure all inter-node database replication and RPC traffic is encrypted via TLS.

### Phase 3: Transport Layer Security (TLS)

The transport layer is the primary boundary for securing public-facing IoT traffic. Implement the following configurations to harden the transport pipe:

- [ ] **Protocol Versions**: Explicitly disable TLS 1.0/1.1 and enforce TLS 1.2 or 1.3 to mitigate known cryptographic vulnerabilities.
- [ ] **Cipher Suites**: Prioritize modern AEAD suites (GCM/Poly1305) and disable legacy CBC ciphers to prevent padding oracle attacks.
- [ ] **Certificates**: Deploy valid CA-signed certificates and configure the `depth` parameter to match your CA hierarchy.
- [ ] **mTLS Enforcement**: Enable `fail_if_no_peer_cert` and set `verify_peer` to ensure mandatory, cryptographically validated client certificates.
- [ ] **CRL Management**: Maintain Certificate Revocation Lists (CRL) to reject compromised or decommissioned client credentials proactively. For details, see [CRL Check](../network/crl.md).
- [ ] **OCSP Stapling**: Enable server-side OCSP stapling to provide real-time certificate status to clients with reduced latency. For details, see [OCSP Stapling](../network/ocsp.md).

These Networking and TLS configurations provide the foundation for end-to-end encrypted communication. For detailed instructions on enabling SSL/TLS listeners and managing certificates, see the [Networking and TLS](../network/overview.md) section.

### Phase 4: Application Layer (MQTT)

This layer enforces granular access control and resource protection for authenticated clients. Authentication and Authorization mechanisms are essential for verifying client identities and controlling publish/subscribe operations.

- [ ] **Authentication**: Ensure a non-empty authentication chain is configured to prevent anonymous connections.
- [ ] **Authorization**: Implement ACLs with a strict "Deny by Default" final rule to isolate device traffic.
- [ ] **Resource Quotas**: Limit `max_packet_size` and topic depth to protect the broker from OOM and DoS events.

To learn how to configure specific mechanisms, such as X.509 certificates, JWT, or external databases like MySQL and Redis, refer to the [Authentication](./authn/authn.md) and [Authorization](./authz/authz.md) sections.

### Phase 5: Administration and Maintenance

Securing the control plane protects the cluster from unauthorized configuration changes.

- [ ] **Dashboard Security**: Change default administrator credentials immediately and enable HTTPS listeners to prevent credential interception. For details, see [First Login](../dashboard/introduction.md#first-login).
- [ ] **Dashboard Access**: Bind the Dashboard to `localhost` or restrict access via a VPN/Firewall to minimize exposure.
- [ ] **Identity Governance**: Use [SSO (LDAP/OIDC)](../dashboard/sso.md) for staff access and enforce [MFA (TOTP)](../multi-factor-authn/multi-factor-authentication.md) for all administrative accounts.
- [ ] **API Key Management**: Generate API keys with strict permission scopes and mandatory expiration dates to ensure periodic rotation. For how to generate an API Key and a secret key, see [API Key](../dashboard/system.md#api-key).
- [ ] **Backup Strategy**: Schedule automated `emqx ctl data export` backups via cron and ensure external certificates are backed up separately.
- [ ] **Audit and Observability**: Enable [audit logging](../dashboard/audit-log.md) and ship logs to external SIEM platforms (e.g., Splunk, ELK, [Datadog](../observability/datadog.md)) for anomaly detection.



<!-- Add reference links to the blogs-->
