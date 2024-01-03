# Enable SSL/TLS Connection

EMQX can establish secure connections via SSL/TLS when accepting the access of an MQTT Client. The SSL/TLS encryption functionality encrypts network connections at the transport layer, enhancing the security of communication data while ensuring its integrity.

This page introduces the funtionalities and advantages of the SSL/TLS connection and how to establish an SSL/TLS connection between the client and EMQX. 

## Safety Benefits

Enabling SSL/TLS connection provides the following safety benefits:

1. **Strong Authentication**: Both communicating parties will verify each other's identities by checking the X.509 digital certificate held by the other party. These types of digital certificates are usually issued by trusted Certificate Authorities (CAs) and cannot be forged.
2. **Confidentiality**: Each session will be encrypted using the session key negotiated by both parties. No third party can know the communication content, so even if the session key is compromised, it does not affect the security of other sessions.
3. **Integrity**: The possibility of data being tampered with in encrypted communication is extremely low.

## Two Usage Modes

You can enable SSL/TLS encrypted connections for all connections, including MQTT connection, to ensure the security of access and message transmission. For client SSL/TLS connections, you can choose one of the following two modes based on your usage scenario:

| Usage Mode                                                   | Advantages                                                   | Disadvantages                                                |
| ------------------------------------------------------------ | ------------------------------------------------------------ | ------------------------------------------------------------ |
| Directly establish SSL/TLS connection between the client and EMQX. | Easy to use, no additional components required               | It will increase EMQX's resource consumption, and if the number of connections is huge, it may lead to high CPU and memory consumption. |
| Terminate TLS connection through a proxy or load balancer.   | No impact on EMQX performance, and provides load balancing capabilities. | Only a few cloud vendors' load balancers support TCP SSL/TLS termination. In addition, users need to deploy software such as HAProxy themselves. |

For information on how to terminate TLS connections through a proxy or load balancer, refer to [Cluster Load Balancing](../deploy/cluster/lb.md).

## One-Way/Two-Way Authentication

EMQX provides comprehensive SSL/TLS capability support, enabling both one-way and two-way client/server mutual trust authentication through X.509 certificates:

| Authentication Method  | Description                                                  | Verification Method                                          | Pros and Cons                                                |
| ---------------------- | ------------------------------------------------------------ | ------------------------------------------------------------ | ------------------------------------------------------------ |
| One-way Authentication | The client verifies the server's identity, but the server does not verify the client's identity. | Clients typically do not need to provide a certificate, and only need to verify that the server's certificate is issued by a trusted Certificate Authority (CA). | Can only ensure the confidentiality and integrity of communication data, but cannot guarantee the identity of the communication parties. |
| Two-way Authentication | Both the server and client mutually verify each other's identity. | Requires issuing certificates for each device, the server verifies the client's certificate to confirm its legitimacy. | Ensures mutual trust between the server and client, and prevents man-in-the-middle attacks. |

## SSL/TLS Certificates

You need to prepare the SSL/TLS certificates for authentication before establishing an SSL/TLS connection. EMQX only provides a set of SSL/TLS certificates (located in the `etc/certs` directory of the installation package) for testing purposes. When used in a production environment, reliable certificates signed by a trusted CA should be used. For information on how to apply for relevant certificates, see [Obtain SSL/TLS Certificates](./tls-certificate.md).

## Enable SSL/TLS with One-Way Authentication

EMQX, by default, enables the SSL/TLS listener on port `8883` and sets it for one-way authentication. You can configure it through the Dashboard and configuration files to implement certificate replacement and modify other configuration items.

### Enable via Dashboard

1. Go to EMQX Dashboard. Click **Management** -> **Listeners** from the left navigation menu.

2. On the **Listeners** page, click **default** from the **Name** column of the SSL listener. 

   - **TLS Verify**: Disabled by default for one-way authentication.
   - **TLS Cert**, **TLS Key** and **CA Cert**: Replace the current certificate files with your private certificate files by clicking the **Reset** button.
   - **SSL Versions**: All TLS/DTLS versions are supported. The default values are `tlsv1.3` and `tlsv1.2`. If PSK cipher suits are used for PSK authentication, make sure to configure `tlsv1.2` , `tlsv1.1` and `tlsv1` here. For more information on PSK authentication, see [Enable PSK Authentication](./psk-authentication.md).
   - **Fail If No Peer Cert**: Used together with **TLS Verify** is enabled. Set to `false` by default.
     - If set to `true`, verification of the client's identity fails if the client sends an empty certificate. The SSL/TLS connection will be rejected.
     - If set to `false`, verification of the client's identity fails only if the client sends an invalid certificate (An empty certificate is considered to be valid). The SSL/TLS connection will be rejected.
   - **Intermediate Certificate Depth**: The allowed maximum depth of certification path; the default value is `10`.
   - **Key Password**: Type the password if the private key file is password-protected.
   - **Enable OCSP Stapling**: Disabled by default; If you need to obtain the revocation status of SSL/TLS certificates, you can enable it by clicking the toogle switch. For more information, see [OCSP Stapling](./ocsp.md).
   - **Enable CRL Check**: Disabled by default; If you need to verify whether connecting client certificates are not revoked, you can enable it by clicking the toogle switch. For more information, see [CRL Check](./crl.md).

3. After you complete the editing, click the **Update** button.

   <img src="./assets/edit-listener.png" alt="edit-listener" style="zoom:40%;" />

### Enable via Configuration File

You can also enable the SSL/TLS connection by modifying the `listeners.ssl.default` configuration group in the configuration file.

1. Place your private SSL/TLS certificate files in the `etc/certs` directory of EMQX.

2. Open the configuration file `emqx.conf` (located in either the `./etc` or `/etc/emqx/etc` directory depending on your installation method). 

3. Modify the `listeners.ssl.default` configuration group. Replace the certificate files with your own certificate files.

   If you need to enable one-way authentication, add `verify = verify_none`:

   ```bash
   listeners.ssl.default {
      bind = "0.0.0.0:8883"
      ssl_options {
        cacertfile = "etc/certs/rootCA.crt"
   
        certfile = "etc/certs/server.crt"
        keyfile = "etc/certs/server.key"
        # Enter the password when the private key file is password protected
        # password = "123456"
   
        # One-way authentication, peer verification not enabled
        verify = verify_none
      }
    }
   ```

4. Restart EMQX to apply the configuration.

## Test Client Connection with One-way Authentication 

You can use [MQTTX CLI](https://mqttx.app/) for testing. One-way authentication typically requires the client to provide a CA certificate, so the client can verify the server's identity:

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

You can use [MQTTX CLI](https://mqttx.app/) for testing. In addition to providing a CA certificate, two-way authentication also requires the client to provide its own certificate:

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
