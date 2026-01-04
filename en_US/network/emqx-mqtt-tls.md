# Enable SSL/TLS Connection

EMQX can establish secure connections via SSL/TLS when accepting the access of an MQTT Client. The SSL/TLS encryption functionality encrypts network connections at the transport layer, enhancing the security of communication data while ensuring its integrity.

This page introduces the functionalities and advantages of the SSL/TLS connection and how to establish an SSL/TLS connection between the client and EMQX. 

## Safety Benefits

Enabling SSL/TLS connection provides the following safety benefits:

1. **Strong Authentication**: Both communicating parties will verify each other's identities by checking the X.509 digital certificate held by the other party. These types of digital certificates are usually issued by trusted Certificate Authorities (CAs) and cannot be forged.
2. **Confidentiality**: Each session will be encrypted using the session key negotiated by both parties. No third party can know the communication content, so even if the session key is compromised, it does not affect the security of other sessions.
3. **Integrity**: The possibility of data being tampered with in encrypted communication is extremely low.

## Two Usage Modes

You can enable SSL/TLS-encrypted connections for all connections, including the MQTT connection, to ensure the security of access and message transmission. For client SSL/TLS connections, you can choose one of the following two modes based on your usage scenario:

| Usage Mode                                                   | Advantages                                                   | Disadvantages                                                |
| ------------------------------------------------------------ | ------------------------------------------------------------ | ------------------------------------------------------------ |
| Directly establish an SSL/TLS connection between the client and EMQX. | Easy to use, no additional components required               | It will increase EMQX's resource consumption, and if the number of connections is huge, it may lead to high CPU and memory consumption. |
| Terminate TLS connection through a proxy or load balancer.   | No impact on EMQX performance, and provides load balancing capabilities. | Only a few cloud vendors' load balancers support TCP SSL/TLS termination. In addition, users need to deploy software such as HAProxy themselves. |

For information on how to terminate TLS connections through a proxy or load balancer, refer to [Cluster Load Balancing](../deploy/cluster/lb.md).

## One-Way/Two-Way Authentication

EMQX provides comprehensive SSL/TLS capability support, enabling both one-way and two-way client/server mutual trust authentication through X.509 certificates:

| Authentication Method  | Description                                                  | Verification Method                                          | Pros and Cons                                                |
| ---------------------- | ------------------------------------------------------------ | ------------------------------------------------------------ | ------------------------------------------------------------ |
| One-way Authentication | The client verifies the server's identity, but the server does not verify the client's identity. | Clients typically do not need to provide a certificate, and only need to verify that the server's certificate is issued by a trusted Certificate Authority (CA). | Can only ensure the confidentiality and integrity of communication data, but cannot guarantee the identity of the communication parties. |
| Two-way Authentication | Both the server and client mutually verify each other's identity. | Requires issuing certificates for each device, the server verifies the client's certificate to confirm its legitimacy. | Ensures mutual trust between the server and client, and prevents man-in-the-middle attacks. |

## SSL/TLS Certificates

Before enabling SSL/TLS, you must prepare SSL/TLS certificates for authenticating and securing connections.

EMQX supports both traditional file-based certificates and managed certificates (EMQX 6.1+), which provide centralized management, reuse across listeners and connectors, and optional automatic issuance with Automated Certificate Management Environment (ACME).

For a complete guide on obtaining, managing, and using SSL/TLS certificates in EMQX, see [SSL/TLS Certificates](./tls-certificate.md).

## Enable SSL/TLS with One-Way Authentication

By default, EMQX enables an SSL/TLS listener on port `8883` and configures it for one-way authentication, where the client verifies the server certificate, but the server does not verify the client's certificate.

You can configure the SSL/TLS listener via the Dashboard or configuration files to replace certificates and adjust other TLS-related settings.

### Enable via Dashboard

1. Go to **Management** -> **Listeners**.

2. Click the SSL listener named **default** to open the **Edit Listener** page.

3. Configure the following SSL/TLS settings:

   #### Authentication

   - **Verify Peer**: Disabled by default for one-way authentication. When disabled, EMQX does not verify client certificates.
   - **Force Verify Peer Certificate**: Only applicable when **Verify Peer** is enabled. For one-way authentication, this option should remain disabled.

   #### Certificate Source

   - **Certificate Source**: Choose how server certificates are provided:
     - **Enter Manually**: Use traditional file-based certificates.
     - **Select from Managed Certs**: Use managed certificate bundles (EMQX 6.1+).

   ##### Enter Manually (File-Based Certificates)

   When **Enter Manually** is selected, configure the following fields:

   - **TLS Cert**: Path to the server certificate file.
   - **TLS Key**: Path to the private key file.

   ##### Select from Managed Certs (EMQX 6.1+)

   When **Select from Managed Certs** is selected:

   - **Namespace**: The namespace where the managed certificate bundle is stored (for example, `global`).

   - **Managed Cert Bundle Name**: Select an existing managed certificate bundle. To create a new bundle, click **Create Managed Certs**. For details, see [Create Managed Certificates via Dashboard](./tls-certificate.md#create-managed-certificates-via-dashboard).

     > Managed certificate bundles can also be created and managed via HTTP API. For details, see [Managed Certificates API](./tls-certificate.md#managed-certificates-via-http-api).

   - **SNI** (optional): The Server Name Indication value used to match this certificate when multiple certificates are configured on the same listener.

   You can click the **+** button to add multiple managed certificate entries. 

   When multiple certificates are configured, EMQX selects the certificate dynamically based on the client’s SNI. If no SNI matches, the first certificate in the list is used as the default.

   #### TLS Protocol and Security Options

   - **SSL Versions**: Supported TLS versions. The default values are `tlsv1.3` and `tlsv1.2`.
   - **Cipher Suites**: Optional. Specify allowed cipher suites if required.
   - **CACert Depth**: The maximum allowed depth of the certificate chain. Default value: `10`.
   - **Key File Passphrase**: Password for the private key file, if encrypted.
   - **Enable OCSP Stapling**: Disabled by default. Enable this option if you need to check certificate revocation status via OCSP.
      See [OCSP Stapling](./ocsp.md).
   - **Enable CRL Check**: Disabled by default. Enable this option to verify whether certificates have been revoked.
      See [CRL Check](./crl.md).

4. After completing the configuration, click **Update** to apply the changes.

### Enable via Configuration File

You can also enable the SSL/TLS connection by modifying the `listeners.ssl.default` configuration group in the configuration file.

1. Place your private SSL/TLS certificate files in the `etc/certs` directory of EMQX.

2. Open the configuration file `base.hocon` (located in either the `./etc` or `/etc/emqx/etc` directory depending on your installation method). 

3. Modify the `listeners.ssl.default` configuration group. Replace the certificate files with your own certificate files.

   If you need to enable one-way authentication, add `verify = verify_none`:

```bash
listeners.ssl.default {
  bind = "0.0.0.0:8883"
  ssl_options {
    # PEM file containing the trusted CA (certificate authority) certificates that the listener uses to verify the authenticity of the client certificates.
    # For one-way authentication, the file content can be empty.
    cacertfile = "etc/certs/rootCAs.pem"
    # PEM file containing the SSL/TLS certificate chain for the listener.
    # If the certificate is not directly issued by a root CA, the intermediate CA certificates should be appended after the listener certificate to form a chain.
    certfile = "etc/certs/server-cert.pem"
    # PEM file containing the private key corresponding to the SSL/TLS certificate.
    keyfile = "etc/certs/server-key.pem"
    # Set `verify_peer` to verify the authenticity of the client certificates. Must be set to 'verify_peer' for two-way authentication (mTLS).
    # Set 'verify_none' to allow any client to connect, regardless of the client certificate.
    verify = verify_none
    # If set to `true`, the handshake fails if the client does not have a certificate to send. Must be set to `true` for two-way authentication (mTLS).
    # If set to `false`, it fails only if the client sends an invalid certificate (an empty certificate is considered valid). i.e. one-way authentication.
    fail_if_no_peer_cert = true
  }
}
```

### EMQX v4 configuration

**In the EMQX, the default listening port of `mqtt:ssl` is 8883.**

Copy the file `emqx.pem`, `emqx.key` and `ca.pem` generated by OpenSSL tool into the directory `etc/certs/` of EMQX, and refer the following configuration to modify `base.hocon`:

```shell
## listener.ssl.$name is the IP address and port that the MQTT/SSL
## Value: IP:Port | Port
listener.ssl.external = 8883

# PEM file containing the private key corresponding to the SSL/TLS certificate.
listener.ssl.external.keyfile = etc/certs/emqx.key

# PEM file containing the SSL/TLS certificate chain for the listener.
        fail_if_no_peer_cert = false
      }
    }
```

4. Restart EMQX to apply the configuration.

## Test Client Connection with One-way Authentication

You can use [MQTTX CLI](https://mqttx.app/cli) for testing. One-way authentication typically requires the client to provide a CA certificate, so the client can verify the server's identity:

```bash
mqttx sub -t 't/1' -h localhost -p 8883 \
  --protocol mqtts \
  --ca certs/rootCA.crt
```

If the server certificate Common Name (CN) does not match the server address specified by the client during connection, the following error will occur:

```bash
Error [ERR_TLS_CERT_ALTNAME_INVALID]: Hostname/IP does not match certificate's altnames: Host: localhost. is not cert's CN: Server
```

In this case, you can set the client certificate CN to match the server address, or ignore the certificate CN validation with the `--insecure` option:

```bash
mqttx sub -t 't/1' -h localhost -p 8883 \
  --protocol mqtts \
  --ca certs/rootCA.crt \
  --insecure
```

## Enable SSL/TLS with Two-Way Authentication

Two-way authentication is an extension of one-way authentication, where EMQX is further configured to verify client certificates, ensuring the legitimacy of the client's identity.

In addition to this, you will need to generate certificates for the client. For specific operations, refer to [Issue Client Certificates](./tls-certificate.md#issue-client-certificates).

For the Dashboard method, you can choose to **Enable** under **TLS Verify**, and configure the **Fail if No Peer Cert** option to `true` to enforce two-way authentication.

You can also add the following configuration to the `listeners.ssl.default` configuration group in the configuration file:

```bash
listeners.ssl.default {
  ...
  ssl_options {
    ...
    # Peer verification enabled
    verify = verify_peer
    # Forced two-way authentication. If the client cannot provide a certificate, the SSL/TLS connection will be rejected.
    fail_if_no_peer_cert = true
  }
}
```

## Test Client Connection with Two-way Authentication

You can use [MQTTX CLI](https://mqttx.app/cli) for testing. In addition to providing a CA certificate, two-way authentication also requires the client to provide its own certificate:

```bash
mqttx sub -t 't/1' -h localhost -p 8883 \
  --protocol mqtts \
  --ca certs/rootCA.crt \
  --cert certs/client-0001.crt \
  --key certs/client-0001.key
```

If the server certificate CN does not match the server address specified by the client during the connection, the following error will occur:

```bash
Error [ERR_TLS_CERT_ALTNAME_INVALID]: Hostname/IP does not match certificate's altnames: Host: localhost. is not cert's CN: Server
```

In this case, you can set the client certificate CN to match the server address, or ignore the certificate CN validation with the `--insecure` option:

```bash
mqttx sub -t 't/1' -h localhost -p 8883 \
  --protocol mqtts \
  --ca certs/rootCA.crt \
  --cert certs/client-0001.crt \
  --key certs/client-0001.key \
  --insecure
```
