---
description: "Configure OCSP stapling on an EMQX MQTT TLS listener and verify the server certificate status response."
---

# Staple the Server Certificate's OCSP Status

Configure EMQX to fetch an OCSP response for its server certificate and include that response in TLS handshakes. This helps clients check the **server** certificate's revocation status. To reject revoked **client** certificates, use [CRL checking](crl.md).

## Before You Begin

OCSP stapling is supported for MQTT `ssl` listeners from EMQX Enterprise 5.0.3. WSS and QUIC listeners do not support this listener option.

Prepare the server certificate/key, the certificate of its issuing CA, and a reachable OCSP responder URL. The certificate's Authority Information Access extension can identify the responder. This configuration requires an explicit server `certfile`; dynamic selection using a managed-certificate list is rejected when OCSP stapling is enabled.

## Configure the Listener

In **Management** -> **Listeners**, edit the SSL listener and enable **OCSP Stapling**. Enter the responder URL, issuer certificate, refresh interval, and HTTP timeout, then save.

The file-managed equivalent is below. Replace the certificate paths and responder URL before applying it through the [configuration workflow](../../guides/configuration/configuration.md).

```hocon
listeners.ssl.default {
  bind = "0.0.0.0:8883"
  ssl_options {
    certfile = "etc/certs/server.crt"
    keyfile = "etc/certs/server.key"
    verify = verify_none
    fail_if_no_peer_cert = false
    ocsp {
      enable_ocsp_stapling = true
      issuer_pem = "etc/certs/ocsp-issuer.pem"
      responder_url = "http://ocsp.example.com"
      refresh_interval = 5m
      refresh_http_timeout = 15s
    }
  }
}
```

This example retains server-only TLS authentication. Configure the peer-verification and CA settings separately if you also require mutual TLS.

| Field Under `ssl_options.ocsp` | Value |
| --- | --- |
| `enable_ocsp_stapling` | `true` to enable; disabled by default. |
| `issuer_pem` | PEM file containing the CA certificate that issued the server certificate. |
| `responder_url` | OCSP service for that issuer/certificate. |
| `refresh_interval` | Default `5m`; choose an interval compatible with response validity and responder limits. |
| `refresh_http_timeout` | Default `15s`. |

## Verify the Stapled Response

From a client with OpenSSL, request certificate status and verify the expected hostname:

```bash
openssl s_client -connect mqtt.example.com:8883 \
  -servername mqtt.example.com -verify_hostname mqtt.example.com \
  -CAfile rootCA.crt -verify_return_error -status < /dev/null
```

This command verifies the server certificate chain and hostname and displays the stapled OCSP response. It does not by itself validate the OCSP response's signature or enforce its status. Inspect the displayed response for the expected certificate, successful response status, certificate status `good`, and valid update times. A completed TLS handshake by itself does not prove that a response was stapled or that your MQTT client enforces OCSP validation.

Repeat after certificate renewal and with the responder temporarily unavailable in a staging environment. Distinguish these cases:

| Condition | EMQX Behavior | Client Check |
| --- | --- | --- |
| Responder is unreachable and a response is cached for the server certificate | A failed refresh does not immediately remove the cached response; EMQX can continue stapling it. | Validate the response's signature, certificate match, status, and validity times. A cached response is not necessarily still valid. |
| Responder is unreachable and the node's OCSP response cache is empty | EMQX can continue the TLS handshake without a stapled response; fetch failure alone does not force connection rejection. | Verify that the client handles a missing response according to your required OCSP policy. |

The MQTT client is responsible for validating the stapled response and applying its policy when the response is missing or expired, or reports an `unknown` or `revoked` certificate status. Enabling stapling on EMQX does not configure that client policy. Check broker fetch errors and test the intended client's rejection behavior with your PKI tooling before depending on this control.

If no response is stapled, check the issuer file, responder URL, network access from EMQX, server certificate configuration, and broker logs. To roll back the feature, disable stapling on that listener and recheck normal TLS connections.
