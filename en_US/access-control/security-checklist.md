# Security Checklist

This checklist helps you review an EMQX deployment before exposing it to production traffic. It is organized by security layers so you can validate the full path from the operating system to the Dashboard. Use it during the initial rollout, after major topology changes, and as part of periodic security reviews.

## Phase 1: Infrastructure and OS

- Raise operating system file descriptor limits and service-level `LimitNOFILE` settings to match your connection scale so the node does not fail under normal or hostile connection pressure.
- Harden the TCP stack and firewall posture for long-lived MQTT traffic, including SYN flood protection, connection tracking capacity, and listener exposure on trusted interfaces only.
- Expose only the listeners your clients actually need. On untrusted networks, prefer encrypted listeners such as `8883` and `8084`, and restrict plaintext listeners such as `1883` to internal or transitional use cases. See [Listener Configuration](../configuration/listener.md) and [Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md).
- Restrict inter-node ports with security groups or firewall rules. For the port mapping used inside a cluster, see [Cluster Security](../deploy/cluster/security.md).
- If nodes have multiple interfaces, bind Erlang distribution traffic to the private network interface only.
- If you deploy EMQX behind a load balancer or TCP proxy, enable [Proxy Protocol](../deploy/cluster/lb.md) only on the listeners that need the real client IP address or client certificate details.
- If Proxy Protocol is enabled for a listener, expose that address and port only to the designated proxy or load balancer. Enforce this in EMQX with `listeners.{type}.{name}.access_rules = ["allow <trusted-LB-CIDR>", "deny all"]`, combined with network-level controls (firewall, private network, or Unix socket). Otherwise, a client that reaches the port directly can craft a PROXY v2 frame with arbitrary peer-cert fields and impersonate any identity.

## Phase 2: Erlang and Cluster

- Replace the default node cookie on every node in the cluster, and use the same high-entropy secret on all members. See [Set Node Cookie](../deploy/cluster/security.md#set-node-cookie).
- Protect `emqx.conf`, ACL files, certificates, private keys, and other secret material with strict file permissions and secure secret-management processes.
- Keep clustering ports internal, and enable TLS for inter-node communication when traffic crosses less-trusted networks or public cloud boundaries. See [Cluster Security](../deploy/cluster/security.md).
- Re-check firewall rules, certificates, and cluster membership controls after adding nodes, moving networks, or changing deployment topology.

## Phase 3: Transport Security

- Use TLS for production MQTT listeners whenever traffic crosses untrusted networks. See [Network and TLS](../network/overview.md).
- Disable legacy protocol versions and weak cipher suites according to your organization's security baseline, and validate the final listener settings in staging before rollout.
- Use certificates issued by a trusted CA or by your internal PKI, and rotate them before they expire.
- Enable mutual TLS when device identity should be verified through client certificates. In this model, validate both client certificate chains and certificate presence during the TLS handshake. See [X.509 Certificate Authentication](./authn/x509.md).
- If you map peer-certificate fields to the MQTT username or client ID (`peer_cert_as_username` / `peer_cert_as_clientid`), the listener **must** enforce mTLS (`verify = verify_peer`, `fail_if_no_peer_cert = true`) with a CA bundle you control. Without it, a client can present a self-signed certificate with an attacker-chosen CN/DN and impersonate any identity. As an additional layer for the empty-username case, set `listeners.{type}.{name}.enable_authn = quick_deny_anonymous`. See [Certificate Information Mapping](./authn/x509.md#certificate-information-mapping).
- If certificate revocation matters in your environment, evaluate [CRL checks](../network/crl.md) or [OCSP stapling](../network/ocsp.md).
- Enable TLS for outbound connections to external resources such as HTTP authenticators, databases, and other integrations.

## Phase 4: MQTT Access Control and Resource Protection

- Configure at least one authenticator before exposing public listeners. By default, EMQX allows all clients to connect if authentication is not enabled. See [Authentication](./authn/authn.md).
- Prefer per-device or per-application credentials instead of shared usernames, passwords, or certificates.
- Bind the MQTT client ID to the authenticated identity whenever the authentication mechanism allows it. For example, verify a JWT `clientid` claim, map a certificate field with [`peer_cert_as_clientid`](./authn/x509.md#certificate-information-mapping), have the HTTP authenticator reject mismatches, or pair the authenticator with a [Client-Info](./authn/cinfo.md) rule. Without this binding, a leaked credential can let an attacker create an unbounded number of sessions under random client IDs with a long [Session Expiry Interval](../messaging/mqtt-concepts.md), causing idle persistent sessions to accumulate until broker memory is exhausted.
- Choose an authentication mechanism that matches your trust model, such as X.509, JWT, SCRAM, or password-based authentication backed by a secure database.
- When using password-based authentication, store salted password hashes instead of plaintext secrets, and prefer strong algorithms such as `bcrypt` or `pbkdf2`.
- Define topic permissions as narrowly as possible and review wildcard usage carefully. See [Authorization](./authz/authz.md).
- When `${clientid}`, `${username}`, or `${client_attrs.X}` is used inside an ACL topic template (see [Authorization Placeholders](./authz/authz.md#authorization-placeholders)), validate those identity values so they cannot contain MQTT topic wildcards (`+`, `#`) or topic separators (`/`). An unvalidated identity substituted into a template like `clients/${clientid}/data` collapses into a wildcard pattern when the client ID is `+` (granting access to every other client's subtopics) or escapes its assigned subtree when the value is `tenantA/+` or contains `/`. Enforce a strict identity format upstream, for example with a [Client-Info](./authn/cinfo.md) rule, a JWT claim pattern, or rejection in the HTTP authenticator. Reject the connection rather than relying on the ACL to catch malformed substitutions.
- When you design outbound requests to external services, including HTTP [authentication](./authn/http.md), HTTP [authorization](./authz/http.md), and data-integration connectors, bridges, and actions, carry each secret in a field or header that EMQX recognizes as sensitive. This allows EMQX to mask the value as `******` when the containing data is processed by its redaction logic, including in relevant logs, traces, and configuration API responses. Redaction is driven by the field or header name: for credentials placed in HTTP headers, use the standard `Authorization` (or `Proxy-Authorization`) header, which EMQX always redacts; for secrets in other configuration fields, name the field with a recognized sensitive key such as `password`, `token`, `secret`, `secret_key`, or `jwt`. A custom header name such as `X-Api-Key`, or an unconventional field name, is not recognized, and its value can appear in clear text in `debug`-level logs and error messages.
- Remove or adjust permissive default rules before relying on authorization in production.
- For file-based ACLs, use a deny-by-default posture where appropriate, such as ending rules with `{deny, all}` and setting `authorization.no_match = deny`. See [Use ACL File](./authz/file.md).
- Review authorization cache settings and authorizer order so that policy changes take effect as expected.
- Constrain MQTT resource usage to reduce the impact of malformed or abusive clients. Review limits such as packet size, topic levels, subscriptions, inflight windows, and queued messages. See [MQTT Configuration](../configuration/mqtt.md).
- Apply listener-level rate controls where needed to limit connection and publish bursts. See [Rate Limiter Configuration](../configuration/limiter.md).
- Use [Banned Clients](./blacklist.md) and [Flapping Detect](./flapping-detect.md) to contain abusive or unstable clients when needed.
- If Cluster Linking is enabled, enforce authentication on the listener that accepts peer connections, restrict the `$LINK/` control namespace to the dedicated Cluster Linking ClientIDs, and deny it for everyone else. See [Secure Cluster Linking](../cluster-linking/security.md).

## Phase 5: Administration and Maintenance

- Change the default Dashboard password before production use, and review who has administrative access. See [System](../dashboard/system.md).
- Keep the Dashboard on trusted networks only. Prefer HTTPS for administrator access, and bind Dashboard listeners to localhost, a private interface, or a protected management network where possible. See [Dashboard Configuration](../configuration/dashboard.md).
- Enable SSRF protection under **Management** -> **Cluster Settings** -> **Rule Engine Security** to validate outbound targets for connectors, bridges, and actions at configuration-update time. This is especially important when delegated administrators can create or modify rule-engine resources. See [Rule Engine Security](../dashboard/cluster_settings.md#rule-engine-security) and [Mitigate SSRF with Rule Engine Policy and Firewall Rules](../deploy/cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules).
- If you expose the management API, use API keys instead of Dashboard credentials, grant only the minimum role required, and set expiration dates where possible. See [REST API](../admin/api.md) and [System](../dashboard/system.md#api-key).
- If you use EMQX Enterprise, consider [Single Sign-On (SSO)](../dashboard/sso.md) for administrative users, and enforce MFA in your identity provider when available.
- Schedule regular backups and rehearse restore procedures. Note that certificates or ACL files stored outside the EMQX data directory require separate backup. See [Backup and Restore](../operations/backup-restore.md).
- Enable audit trails where available, and centralize logs and metrics in your observability stack for anomaly detection and incident response. See [Audit Log](../dashboard/audit-log.md), [Logs Configuration](../configuration/logs.md), and [Logs and Observability](../observability/overview.md).

## Revalidate After Change

- Re-run this checklist after certificate rotation, listener changes, load balancer updates, cluster expansion, backup policy changes, or any change to the authentication and authorization chain.
- Validate the expected failure modes before production cutover, such as rejected anonymous clients, failed TLS handshakes for invalid certificates, and denied publish or subscribe operations outside allowed topics.
