---
description: "Choose TLS settings for EMQX client and service connections, certificate management, and revocation checks."
---

# Secure Client and Service Connections

Choose the connection you need to protect, then configure certificate trust at both ends. TLS encrypts one network connection; a proxy, broker, and downstream service can form several separate connections.

## Choose a TLS Task

| Connection | Start Here | Verify |
| --- | --- | --- |
| MQTT client → EMQX | [Enable TLS or mutual TLS](enable.md) | The client trusts the server CA and the certificate matches its hostname. |
| Browser → EMQX | [Secure WebSockets](../connect-emqx/mqtt-over-websocket.md) | Use WSS from an HTTPS page and a certificate trusted by the browser. |
| EMQX → HTTP service, database, or broker | [Configure outbound TLS](#outbound-tls) | EMQX validates the external server; provide a client certificate if that server requires one. |
| EMQX node → EMQX node | [Secure cluster communication](../../guides/cluster/security.md) | Protect both cluster coordination and data forwarding. |
| Client → load balancer → EMQX | [Choose a termination point](enable.md#choose-a-termination-point) | Check encryption and identity forwarding on each hop. |

## Certificates and Revocation

Use [certificate management](certificates.md) to issue a test certificate, reference PEM files, create managed bundles, select certificates with SNI, or rotate certificates. Managed bundles are available from EMQX 6.1.

Server authentication lets the client validate EMQX. Mutual TLS also requires EMQX to validate the client certificate. MQTT authentication and topic authorization are separate decisions; configure them for your application even when transport encryption is enabled.

| Requirement | Feature | Scope |
| --- | --- | --- |
| Reject a revoked client certificate | [CRL checking](crl.md) | MQTT SSL listeners; requires client certificate verification. |
| Provide a server certificate's status to clients | [OCSP stapling](ocsp.md) | MQTT SSL listeners; clients must inspect and validate the stapled response. |
| Connect an application securely | [Client TLS settings](client-examples.md) | CA trust, hostname verification, and optional client identity. |

CRL and OCSP listener options do not apply to WSS or QUIC listeners.

## Outbound TLS

Open the authentication backend or data integration connector, enable TLS, and configure the following settings. Configuration paths vary by backend or connector. Follow the complete example in the corresponding [authentication](../authentication/index.md) or [data integration](../data-integration/data-bridges.md) guide.

| Setting | What to Provide |
| --- | --- |
| `ssl.enable` | Enable TLS for the outbound connection. |
| `ssl.verify` | Use `verify_peer` to validate the external server's certificate chain. |
| `ssl.cacertfile` | A PEM trust bundle containing the CA certificates for that server. |
| SNI / `server_name_indication` | The expected server hostname, as supported by the connector. SNI selects a virtual host; sending SNI alone does not establish certificate trust. Keep peer verification enabled and check hostname validation for the connector. |
| `ssl.certfile` and `ssl.keyfile` | The client certificate chain and matching private key when the external server requires mutual TLS. |
| Certificate source | Enter PEM material or select an existing [managed bundle](certificates.md#managed-certificates). A connector uses one bundle reference rather than a listener's SNI selection list. |
| Middle Box Compatibility Mode | TLS 1.3 handshake compatibility for network devices that mishandle it. Change this only when diagnosing the intervening network. |

For a private service, distribute its CA certificate to EMQX. Do not solve an unknown CA or hostname mismatch by switching off verification. After saving, test the connector and verify an actual request or delivered message; an open TCP port alone does not validate TLS or application permissions.

## Addressing and Next Steps

Use [IPv6 configuration](../../guides/network/ipv6.md) for client, Dashboard, cluster, and outbound addressing. Then use a [language SDK](../connect-emqx/introduction.md) to subscribe and receive a known payload through the secured endpoint.
