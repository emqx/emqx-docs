# CRL Check

Starting with EMQX Open Source v5.0.22 and EMQX Enterprise v5.0.3, EMQX supports Certificate Revocation List (CRL) checks for MQTT SSL listeners. This feature is only available for listeners of type `ssl`. Secure WebSocket (`wss`) and QUIC listeners are not supported.

When CRL checking is enabled, EMQX verifies whether a connecting client’s certificate has been revoked by consulting the CRL Distribution Point specified in the client certificate. If the certificate is listed as revoked, the connection is rejected during the SSL/TLS handshake phase.

> **Note**
>
> For CRL enforcement to work, the CRL must include the *Issuing Distribution Point* extension, as defined in [RFC 3280, Section 5.2.5](https://www.rfc-editor.org/rfc/rfc3280#section-5.2.5).

## Enable CRL Check

To enable CRL checking, you must:

1. Enable the CRL check option on the SSL listener.
2. Set the listener’s `verify` option to `verify_peer`, ensuring that client certificates are validated.

The following example demonstrates how to configure an SSL listener with CRL checking enabled:


```hcl
listeners.ssl.default {
  bind = "0.0.0.0:8883"
  ssl_options {
    # PEM format file containing the trusted CA (certificate authority) certificates that the listener uses to verify the authenticity of the clients.
    cacertfile = "/etc/emqx/certs/ca.pem"
    # PEM format file containing the SSL/TLS certificate chain for the listener. If the certificate is not directly issued by a root CA, the intermediate CA certificates should be appended after the listener certificate to form a chain.
    certfile = "/etc/emqx/certs/server.pem"
    # PEM format file containing the private key corresponding to the SSL/TLS certificate.
    keyfile = "/etc/emqx/certs/server.key"
    # Must verify peer certificates
    verify = verify_peer
    # Force the client to send a non-empty certificate, otherwise fail the TLS handshake.
    fail_if_no_peer_cert = true
    # Also verify client certificate's revocation status
    enable_crl_check = true
  }
}
```

## CRL Caching

To avoid excessive HTTP requests to CRL Distribution Point endpoints, EMQX caches fetched CRLs locally.

When a client connects, and EMQX detects a new CRL URL for the first time, it fetches the CRL from the Distribution Point in the client's certificate. By default, EMQX refreshes cached CRLs every 15 minutes to ensure revocation information remains up to date.

