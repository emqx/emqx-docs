# CRL/OCSP Stapling

EMQX supports SSL/TLS, you can use X.509 certificate to enable client authentication and transport encryption.

Before the X.509 certificate expires, if the private key is exposed or the certificate is wrong, you need to revoke it to make sure your device is not being used illegally.
In this case, you can use the CRL or OCSP Stapling feature to achieve a more secure setup.

## CRL

The Certificate Revocation List (CRL) is a list maintained by the CA that contains the serial numbers and revocation times of certificates that have been revoked. 

You can configure the request endpoint of the CA on EMQX and refresh the CRLs regularly. Unlike CRLs on the browser, the MQTT client need not manage CRLs and EMQX is in charge of the verification during the connection handshake.

All operations are run by EMQX, no special scripts or adaptations to the client are required.

### Use CRL Feature

The instructions below demonstrate the revocation of a self-signed client certificate and the deployment process in EMQX.

::: tip Prerequisites
You have enabled SSL/TLS connection and have generated a self-signed CA certificate for the client. You also possess the CA private key.

For detailed instructions on enabling SSL/TLS in EMQX, refer to [Enable SSL/TLS for EMQX MQTT broker](https://www.emqx.com/en/blog/emqx-server-ssl-tls-secure-connection-configuration-guide).
:::

#### Revoke Client Certificate

1. Generate the `openssl.conf` file and `index.txt` database file.

```bash
 cat > openssl.cnf <<EOF
 [ ca ]
 default_ca = myca

 [ myca ]
 dir = .
 database = \$dir/index.txt
 certificate = \$dir/ca.crt
 private_key = \$dir/ca.key
 default_days = 365
 default_md = sha256

 default_crl_days = 365
 EOF

 touch index.txt
```

 The following configuration items are included:

| Configuration Item | Meaning                               |
| ------------------ | ------------------------------------- |
| dir                | Directory for storing the certificate |
| database           | Certificate database file             |
| certificate        | CA certificate                        |
| private_key        | CA private key                        |
| default_days       | Certificate validity period           |
| default_md         | Message digest algorithm              |
| default_crl_days   | CRL validity period                   |

2. Generate a CRL file through CA certificate, private key, and configuration file.

```bash
openssl ca -gencrl -keyfile ca.key -cert ca.crt -out ca.crl -config openssl.cnf
```

3. Revoke client certificate.

```bash
 $ openssl ca -revoke client.crt -config openssl.cnf

 Adding Entry with serial number 36071A4116484CEA69F906B3CDE74507CC2939F4 to DB for /C=CN/ST=YN/L=KM/O=EMQ/OU=EMQX/CN=emqx-c
 Revoking Certificate 36071A4116484CEA69F906B3CDE74507CC2939F4.
 Data Base Updated
```

4. Regenerate the CRL file.

```bash
openssl ca -gencrl -keyfile ca.key -cert ca.crt -out ca.crl -config openssl.cnf
```

#### Configure EMQX

1. Store the CRL file on the web server, assuming that the URL of the CRL file is http://localhost:8080/ca.crl.

2. Configure the CRL request endpoint to ensure that EMQX can access the CRL file. Below is an example of CRL configuration:

```bash
# Enable CRL check
listener.ssl.external.enable_crl_check = true

# Use comma to separate URL files if more than one
listener.ssl.external.crl_cache_urls = http://localhost:8080/ca.crl

# CRL Request timeout, global for all listeners
crl_cache_http_timeout = 15s


# The period to refresh the CRLs from the servers, global for all listeners
crl_cache_refresh_interval = 15m
```

::: tip

You can request the list of CRL files by consulting CA or using the following command, taking the broker.emqx.io domain certificate as an example:

```bash
$ openssl x509 -in broker.emqx.io.crt -noout -text | grep crl

URI:http://crl3.digicert.com/RapidSSLGlobalTLSRSA4096SHA2562022CA1.crl
URI:http://crl4.digicert.com/RapidSSLGlobalTLSRSA4096SHA2562022CA1.crl
```

For more detailed configuration, refer to [Enable CRL](../configuration/configuration.md#listener-ssl-external-enable-crl-check).

:::

3. Restart EMQX to apply the configurations.

#### Use MQTTX CLI to Verify

Use the MQTTX CLI to connect to EMQX.

```bash
mqttx conn -h localhost -p 8883 --ca ca.crt --cert client.crt --key client.key --insecure
```

MQTTX CLI fails to establish a connection due to verification failure, and EMQX logs the following error message:

```bash
2023-05-05T16:19:59.117098+08:00 [error] supervisor: 'esockd_connection_sup - <0.2576.0>', errorContext: connection_shutdown, reason: {ssl_error,{tls_alert,{certificate_revoked,"TLS server: In state wait_cert at ssl_handshake.erl:2098 generated SERVER ALERT: Fatal - Certificate Revoked\n"}}}...
```

Now you have successfully revoked the client certificate, and EMQX is able to recognize the revoked client certificate correctly.

## OCSP Stapling

Online Certificate Status Protocol (OCSP) is another certificate revocation solution, and OCSP Stapling is the latest improvement to OCSP technology.

OCSP Stapling checks the status of certificates via EMQX without each client sending a request to the OCSP Responder. When OCSP Stapling is enabled, EMQX will request the certificate from the OCSP server and cache the result.

When the client sends an SSL handshake request to EMQX, EMQX sends the OCSP info of the certificate to the client along with the certificate chain (Stapling), and the client verifies the certificate validity.

OCSP Stapling improves the speed of client-side certificate checking and reduces the load on OCSP Responder.

### Configure OCSP Stapling

Below is an example of OCSP stapling configuration:

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

Take the broker.emqx.io domain certificate as an example, you can consult CA or use the following command to obtain the OCSP responder URL:

```bash
$ openssl x509 -in broker.emqx.io.crt -noout -ocsp_uri
http://ocsp.dcocsp.cn
```

### Verify OCSP Stapling

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

For more detailed configuration, refer to [Enable OCSP Stapling](../configuration/configuration.md#listener-ssl-external-enable-ocsp-stapling).
