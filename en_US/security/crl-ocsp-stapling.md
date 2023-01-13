# CRL and OCSP Stapling

EMQX supports SSL/TLS, you can use X.509 certificate to enable client authentication and transport encryption.

Before the X.509 certificate expires, if the private key is exposed or the certificate is wrong, you need to revoke it to make sure your device is not being used illegally.
In this case you can use the CRL or OCSP Stapling feature to achieve a more secure setup.

## CRL

The CRL(Certificate Revocation List) is a list maintained by the CA that contains the serial numbers and revocation times of certificates that have been revoked.

You can configure the request endpoint of the CA on EMQX and refresh the CRLs regularly.

Different from the browser's mechanism for using CRLs, the MQTT client does not need to manage CRL and only need to verify via EMQX during the connection handshake.

All operations are run by EMQX, no special scripts or adaptations to the client are required.

### Configuration

## CRL Configuration

```bash
# Enable CRL check
listener.ssl.external.enable_crl_check = true

# Comma-separated URL list for CRL servers
listener.ssl.external.crl_cache_urls = http://my.crl.server/intermediate.crl.pem, http://my.other.crl.server/another.crl.pem

# CRL Request timeout, global for all listeners
crl_cache_http_timeout = 15s


# The period to refresh the CRLs from the servers, global for all listeners
crl_cache_refresh_interval = 15m
```

The list of CRL files can be requested from the CA or by the following command:

```bash
$ openssl x509 -in broker.emqx.io.crt -noout -text | grep crl

URI:http://crl3.digicert.com/RapidSSLGlobalTLSRSA4096SHA2562022CA1.crl
URI:http://crl4.digicert.com/RapidSSLGlobalTLSRSA4096SHA2562022CA1.crl
```

For more detailed configuration, please refer to [Enable CRL](../configuration/configuration.md#listener-ssl-external-enable-crl-check).

## OCSP Stapling

OCSP(Online Certificate Status Protocol) is another certificate revocation solution, and OCSP Stapling is the latest improvement to OCSP technology.

OCSP Stapling checks the status of certificates via EMQX without each client sending a request to the OCSP Responder.
When OCSP Stapling is enabled, EMQX will request certificate from the OCSP server and cache the result.

When the client sends an SSL handshake request to EMQX, EMQX sends the OCSP info of the certificate to the client along with the certificate chain(Stapling), and the client verifies the certificate validity.

OCSP Stapling improves the speed of client-side certificate checking and reduces the load on OCSP Responder.

### Configuring OCSP Stapling

```bash
# Enable OCSP Stapling
listener.ssl.external.enable_ocsp_stapling = true

# OCSP Responder URL
## Get from CA or with this command
## openssl x509 -in broker.emqx.io.crt -noout -ocsp_uri
listener.ssl.external.ocsp_responder_url = http://ocsp.digicert.com

# OCSP Responder PEM
listener.ssl.external.ocsp_issuer_pem = etc/certs/ocsp-issuer.pem

# OCSP Stapling request interval and timeout
listener.ssl.external.ocsp_refresh_interval = 5m
listener.ssl.external.ocsp_refresh_http_timeout = 15s
```

You can use this command to verify if OCSP Stapling is successfully enabled:

```bash
$ openssl s_client -connect broker.emqx.io:8883  -status -tlsextdebug < /dev/null 2>&1 | grep -i "OCSP response"

# Not enabled
OCSP response: no response sent

# Successfully enabled
OCSP response:
OCSP Response Data:
    OCSP Response Status: successful (0x0)
    Response Type: Basic OCSP Response
```

For more detailed configuration, please refer to [Enable OCSP Stapling](../configuration/configuration.md#listener-ssl-external-enable-ocsp-stapling).
