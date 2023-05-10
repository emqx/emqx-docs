# CRL Check

{% emqxce %}

Since EMQX 5.0.22, CRL(Certification Revocation List) Check is supported for MQTT SSL listeners.  Note that those do not include Secure WebSocket nor QUIC listeners: only listeners of type `ssl` support this feature.

{% endemqxce %}

{% emqxee %}

Since EMQX 5.0.3,  CRL(Certification Revocation List) Check is supported for MQTT SSL listeners.  Note that those do not include Secure WebSocket nor QUIC listeners: only listeners of type `ssl` support this feature.

{% endemqxee %}

With this feature enabled, EMQX will attempt to verify if connecting client certificates are not revoked according to the CRL Distribution Point described in the client's certificate, and deny connection to revoked client certificates during the SSL/TLS handshake phase of the connection.  Note that the CRL itself must contain the ["Issuing Distribution Point" extension](https://www.rfc-editor.org/rfc/rfc3280#section-5.2.5) in order for the revocation check to be enforced.

In order to enable this feature, we need to both enable the corresponding option in the listener and also set the `verify` option of the listener to `verify_peer`, so that the client must be checked against the CRL.

Example configuration to enable CRL Check:


```hcl
listeners.ssl.default {
  bind = "0.0.0.0:8883"
  max_connections = 512000
  ssl_options {
    keyfile = "/etc/emqx/certs/server.key"
    certfile = "/etc/emqx/certs/server.pem"
    cacertfile = "/etc/emqx/certs/ca.pem"
    verify = verify_peer
    enable_crl_check = true
  }
}
```
