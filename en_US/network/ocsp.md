### OCSP Stapling

{% emqxce %}

Since EMQX 5.0.23, Online Certificate Status Protocol (OCSP) Stapling is supported for MQTT SSL listeners.  Note that those do not include Secure WebSocket nor QUIC listeners: only listeners of type `ssl` support this feature.

{% endemqxce %}

{% emqxee %}

Since EMQX 5.0.3, Online Certificate Status Protocol (OCSP) Stapling is supported for MQTT SSL listeners.  Note that those do not include Secure WebSocket nor QUIC listeners: only listeners of type `ssl` support this feature.

{% endemqxee %}

Since EMQX version v5.0.23 and e5.0.3, 

With this feature enabled, the listener will send an OCSP Stapling response to connecting clients about EMQX's server certificate.  This OCSP data is periodically fetched from a configured OCSP Responder server and cached for a period of time (5 minutes by default).

In order to enable this feature, we need to both enable the corresponding option in the listener, and also specify the OCSP Issuer certificate and OCSP Responder URL from which EMQX should download and cache the OCSP response for its own server certificate.

Example configuration to enable OCSP Stapling:

```hcl
listeners.ssl.default {
  bind = "0.0.0.0:8883"
  max_connections = 512000
  ssl_options {
    keyfile = "/etc/emqx/certs/server.key"
    certfile = "/etc/emqx/certs/server.pem"
    cacertfile = "/etc/emqx/certs/ca.pem"
    ocsp {
      enable_ocsp_stapling = true
      issuer_pem = "/etc/emqx/certs/ocsp-issuer.pem"
      responder_url = "http://ocsp.responder.com:9877"
      refresh_interval = 15m
      refresh_http_timeout = 15s
    }
  }
}
```

Be sure to change the certificate/private key paths above with your corresponding files, and also to set your OCSP Responder URL accordingly.