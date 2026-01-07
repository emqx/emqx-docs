# Enable SSL/TLS Connections

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

EMQX supports both traditional path-based certificates and managed certificates (EMQX 6.1+), which provide centralized management, reuse across listeners and connectors.

For a complete guide on obtaining, managing, and using SSL/TLS certificates in EMQX, see [SSL/TLS Certificates](./tls-certificate.md).

## Enable SSL/TLS with One-Way Authentication

By default, EMQX enables an SSL/TLS listener on port `8883` and configures it for one-way authentication, where the client verifies the server certificate, but the server does not verify the client's certificate.

You can configure the SSL/TLS listener via the Dashboard or via the configuration file. In both cases, EMQX supports two certificate provisioning methods:

- Path-based certificates (traditional PEM files): certificates referenced directly by file paths in configuration.
- Managed certificates (introduced in EMQX 6.1): certificates managed as reusable resources and referenced by name.

Choose the method that best fits your deployment and operational model.

### Enable via Dashboard

1. Go to **Management** -> **Listeners**.

2. Click the SSL listener named **default** to open the **Edit Listener** page.

3. Configure the following SSL/TLS settings:

   #### Authentication

   - **Verify Peer**: Disabled by default for one-way authentication. When disabled, EMQX does not verify client certificates.
   - **Force Verify Peer Certificate**: Only applicable when **Verify Peer** is enabled. For one-way authentication, this option should remain disabled.

   #### Certificate Source

   - **Certificate Source**: Choose how server certificates are provided:
     - **Enter Manually**: Use traditional path-based certificates.
     - **Select from Managed Certs**: Use managed certificate bundles (EMQX 6.1+).

   ##### Enter Manually (Path-Based Certificates)

   When **Enter Manually** is selected, configure the following fields:

   - **TLS Cert**: Path to the server certificate file.
   - **TLS Key**: Path to the private key file.

   ##### Select from Managed Certs (EMQX 6.1+)

   When **Select from Managed Certs** is selected:

   - **Namespace**: The namespace where the managed certificate bundle is stored (default: `global`).

   - **Managed Cert Bundle Name**: Select an existing managed certificate bundle. To create a new bundle, click **Create Managed Certs**. For details, see [Create Certificate Bundles via Dashboard](./tls-certificate.md#create-certificate-bundles-via-dashboard).

   - **SNI** (optional): The Server Name Indication value used to match this certificate when multiple certificates are configured on the same listener.

   You can click the **+** button to add multiple managed certificate entries. 

   When multiple certificates are configured, EMQX selects the certificate dynamically based on the client’s SNI. If no SNI matches, the first certificate in the list is used as the default.

   #### TLS Protocol and Security Options

   - **SSL Versions**: All TLS/DTLS versions are supported. The default values are `tlsv1.3` and `tlsv1.2`. If PSK cipher suites are used for PSK authentication, make sure to configure `tlsv1.2` , `tlsv1.1` and `tlsv1` here. For more information on PSK authentication, see [Enable PSK Authentication](./psk-authentication.md).
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

2. Open the configuration file `base.hocon` (located in either the `./etc` or `/etc/emqx/etc` directory, depending on your installation method). 

3. Modify the `listeners.ssl.default` configuration group. 

   - If you use certificates stored as files on disk, replace the certificate files with your own. To enable one-way authentication, add `verify = verify_none`:

     ```hocon
     listeners.ssl.default {
       bind = "0.0.0.0:8883"
       
       ssl_options {
         cacertfile = "etc/certs/rootCAs.pem"
         certfile = "etc/certs/server-cert.pem"
         keyfile = "etc/certs/server-key.pem"
         
         verify = verify_none
         fail_if_no_peer_cert = true
       }
     }
     ```

     **Configuration details:**

     - **`cacertfile`**: Path to the PEM file containing trusted CA certificates used to verify client certificates. For one-way authentication, this file may be empty or contain no CA certificates.
     - **`certfile`**: Path to the PEM file containing the server’s SSL/TLS certificate chain. If the server certificate used by the listener is not issued directly by a root CA, append any intermediate CA certificates after the server certificate in `certfile` to form a complete chain.
     - **`keyfile`**: Path to the PEM file containing the private key corresponding to the server certificate.
     - **`verify`**: Controls whether EMQX verifies client certificates:
       - `verify_none`: Client certificates are not verified (one-way authentication).
       - `verify_peer`: Client certificates are verified (required for two-way authentication / mTLS).
     - **`fail_if_no_peer_cert`**: Determines handshake behavior when a client does not present a certificate:
       - `false`: Allows connections without a client certificate, and rejects the connection only if a certificate is presented but is invalid (one-way authentication).
       - `true`: Rejects connections if the client does not provide a certificate (required for mTLS).

   - If you use certificates managed centrally via EMQX and referenced by name, see the configuration examples below:

     > Managed certificate bundles must be created in advance via the Dashboard or HTTP API. The listener configuration only references existing managed certificates.

     **Example: referencing certificates in the global namespace**
     
     ```
     listeners.ssl.default {
       bind = "0.0.0.0:8883"
     
       ssl_options {
         managed_certs = [
           {
             bundle_name = "example-cert-1"
             sni  = "example.com"
           },
           {
             bundle_name = "example-cert-2"
             sni  = "api.example.com"
           }
         ]
     
         verify = verify_none
         fail_if_no_peer_cert = false
       }
     }
     ```
     
     > When referencing managed certificates in the global namespace, the `namespace` field must be omitted. The global namespace is used by default.
     
     **Example: referencing certificates in a non-global (tenant) namespace**
     
     ```
     listeners.ssl.default {
       bind = "0.0.0.0:8883"
     
       ssl_options {
         managed_certs = [
           {
             namespace = "tenant-a"
             bundle_name = "mqtt-cert"
             sni       = "mqtt.tenant-a.example.com"
           }
         ]
     
         verify = verify_none
         fail_if_no_peer_cert = false
       }
     }
     ```
     
     > When using managed certificates created in a non-global (tenant) namespace, the `namespace` field must be specified explicitly.
     
     When multiple managed certificates are configured:
     
     - EMQX selects the certificate based on the client’s SNI.
     - If no SNI match is found, the first certificate entry is used as the default.

4. Restart EMQX to apply the configuration.

After completing the SSL/TLS configuration, you can connect to EMQX using an MQTT client.

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

For the Dashboard method, you can choose to **Enable** under **Verify Peer**, and configure the **Force Verify Peer Certificate** option to `true` to enforce two-way authentication.

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
