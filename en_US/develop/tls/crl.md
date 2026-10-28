---
description: "Enable CRL checking on EMQX MQTT TLS listeners and verify that revoked client certificates are rejected."
---

# Reject Revoked Client Certificates

Enable Certificate Revocation List (CRL) checking when an MQTT TLS listener must reject client certificates revoked by their issuer.

## Before You Begin

CRL checking is available for MQTT `ssl` listeners from EMQX 5.0.3. It does not apply to WSS or QUIC listeners. First configure and verify [mutual TLS](enable.md#require-mutual-tls).

Prepare a client certificate with an HTTP or HTTPS CRL Distribution Point (CDP), and a CRL from its issuer containing the **Issuing Distribution Point** extension. EMQX reads the CDP from the client certificate; the CRL is not an arbitrary local denylist. Ensure every accepting node can fetch the CRL through its HTTP/HTTPS URL; other CDP protocols are not supported.

## Enable Checking

In **Management** -> **Listeners**, edit the SSL listener, enable peer verification, require a peer certificate, and enable CRL checking. For a file-managed deployment, apply this configuration through the [configuration workflow](../../guides/configuration/configuration.md):

```hocon
listeners.ssl.default {
  bind = "0.0.0.0:8883"
  ssl_options {
    certfile = "etc/certs/server.crt"
    keyfile = "etc/certs/server.key"
    cacertfile = "etc/certs/rootCA.crt"
    verify = verify_peer
    fail_if_no_peer_cert = true
    enable_crl_check = true
  }
}
```

Replace the example paths with the paths to your server certificate, private key, and trusted client CA bundle. The basic local CA example in [certificate management](certificates.md) does not create a CRL distribution service or issue CDP-enabled certificates.

## Caching and Refresh

EMQX fetches a CRL when it encounters a new distribution-point URL and caches it locally. The default cache refresh interval is `15m` and the HTTP fetch timeout is `15s`. A revocation added by the issuer is not necessarily visible until the relevant cache refreshes.

A failed refresh does not immediately invalidate a still-valid cached CRL. EMQX retries failed refreshes and evicts the cached entry after the configured `crl_cache.failure_threshold` is reached. A subsequent connection must fetch the CRL again. Without a usable cached CRL or a successfully fetched valid CRL, the TLS handshake is rejected. CRL expiry and cache refresh timing are separate conditions: a cached CRL must still be valid when used.

Inspect the certificate's distribution point and issuer before diagnosing a connection failure:

```bash
openssl x509 -in client.crt -noout -issuer -serial -text
```

Check CRL signatures, validity times, extensions, and endpoint reachability using your CA tooling. Do not confuse the certificate's CDP with the CRL's Issuing Distribution Point extension.

## Verify Enforcement

Use fresh connections for each case, on every node that accepts clients:

| Test | Required Result |
| --- | --- |
| Valid client certificate, current CRL | TLS and the authorized MQTT exchange succeed. |
| Revoked client certificate after the CRL is refreshed | TLS handshake is rejected. |
| No client certificate | TLS handshake is rejected. |
| No cached CRL and the HTTP/HTTPS endpoint is unreachable | TLS handshake is rejected because EMQX cannot obtain a CRL. |
| Valid, non-revoked client certificate and a still-valid cached CRL; endpoint becomes unreachable, but the cache entry has not been evicted | TLS can still succeed using the cached CRL. Endpoint failure alone does not immediately reject connections. |
| Refreshes keep failing until the cache entry is evicted; endpoint remains unreachable | A new connection cannot fetch the CRL, so the TLS handshake is rejected. |
| Cached CRL has expired and no valid replacement can be fetched | TLS handshake is rejected; an expired CRL is not sufficient to validate revocation status. |

Check TLS and CRL-related broker logs for rejected handshakes. Refresh timing and endpoint availability are part of the operational acceptance test; connecting once with a valid certificate does not demonstrate revocation enforcement.

To roll back a newly enabled check, restore the listener's previous CRL setting while retaining the intended peer-verification policy. Revocation checks occur during connection establishment; they do not automatically disconnect every already-connected device.
