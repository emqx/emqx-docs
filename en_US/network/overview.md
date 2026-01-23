# Network and TLS

Security is essential for end-to-end encrypted communication in IoT scenarios. Secure Sockets Layer (SSL) and Transport Layer Security (TLS) protocols are often adopted in network communications to ensure that the data transmission remains confidential and cannot be intercepted or modified by an attacker. The SSL/TLS encryption function encrypts network connections at the transport layer and involves the use of digital certificates to authenticate the identity of the parties involved and to establish a secure communication channel.

EMQX adopts SSL and TLS cryptographic protocols to ensure secure network communication when:

- Establishing a connection between MQTT clients and EMQX
- Connecting to external resources, such as a database
- Different EMQX nodes in a cluster communicate with each other

EMQX provides comprehensive support for SSL/TLS capabilities, including support for one-way/two-way authentication and X.509 certificate authentication.

## TLS for Client Connections

EMQX supports SSL/TLS encryption for MQTT client connections, enabling secure, encrypted communication between clients and the broker. You can configure TLS listeners to support one-way authentication (server authentication only) or two-way authentication (mTLS), depending on your security requirements.

The [Enable SSL/TLS Connections](./emqx-mqtt-tls.md) section in this chapter provides a step-by-step guide to configuring TLS listeners for MQTT clients, including listener configuration via the Dashboard and configuration files.

### Certificate Management

EMQX provides a unified certificate management model for SSL/TLS, covering how certificates are obtained, stored, managed, and reused across listeners and other TLS-enabled components. This includes traditional path-based certificates as well as managed certificates with centralized lifecycle management.

For a complete overview of certificate concepts and workflows in EMQX, see [SSL/TLS Certificates](./tls-certificate.md).

### Certificate Validation and Revocation

To further enhance TLS security, EMQX supports certificate validation and revocation checks:

- **CRL Check**: Verifies whether certificates have been revoked using Certificate Revocation Lists. See [CRL Check](./crl.md).
- **OCSP Stapling**: Checks certificate revocation status via the Online Certificate Status Protocol. See [OCSP Stapling](./ocsp.md).

These features can be enabled when configuring TLS listeners and help protect against the use of compromised certificates.

### Client-Side TLS Examples

The [Client TLS](./mqtt-client-tls.md) section provides sample MQTT client code and example projects demonstrating how to connect securely to EMQX using SSL/TLS. These examples include practical guidance on configuring client certificates and TLS options.

## TLS for External Resource Access

EMQX also supports SSL/TLS encryption when accessing external resources, such as HTTP-based authentication services, databases, or other data integration services. You can enable TLS for these connections directly from the EMQX Dashboard by turning on the **Enable TLS** option when configuring the corresponding feature.

### TLS Configuration Options in Dashboard

After enabling TLS, the following options are available:

- **TLS Verify**: Controls whether the server certificate of the external resource is verified. When enabled, EMQX validates the server’s certificate chain.
- **Middle Box Compatibility Mode**: Enables compatibility mode for TLS 1.3 connections. Some network middle boxes may mishandle standard TLS 1.3 handshakes. When this option is enabled, EMQX adapts the TLS 1.3 handshake to resemble TLS 1.2, improving connection success in such environments.
- **SNI**: Server Name Indication, indicating whether the server domain name and certificate are verified to be the same during the TLS handshake. If left empty, no hostname validation is performed.
- **Certificate Source**: Determines how client-side certificates are provided when required:
  - **Enter Manually**: Provide certificate contents or files directly.
  - **Select from Managed Certs**: Reference an existing certificate bundle managed centrally by EMQX (EMQX 6.1+). This allows certificate reuse and simplifies certificate rotation. For details on creating and managing certificate bundles, see [Managed Certificates](./tls-certificate.md#managed-certificates).
- **TLS Cert / TLS Key**: Required when the external server verifies the client certificate (mutual TLS).
- **CA Cert**: Required when **TLS Verify** is enabled, to validate the server’s certificate.

<img src="./assets/enable-TLS-dashboard.png" alt="enable-TLS-dashboard" style="zoom:50%;" />

### TLS Configuration via Configuration File

You can also enable TLS for external resource access by configuring the corresponding `ssl` options in the configuration file. For example, when configuring an HTTP-based authentication backend:

```hocon
authentication {
  url = "https://127.0.0.1:8080"
  backend = "http"

  ...

  ssl {
    enable = true
    cacertfile = "etc/certs/cacert.pem"
    certfile = "etc/certs/cert.pem"
    keyfile = "etc/certs/key.pem"
    verify = verify_peer
  }
}
```

**SSL Configuration Items**:

- **`enable`**: Enables SSL/TLS for outbound connections to the external resource.
- **`cacertfile`**: Path to a PEM format file containing the trusted certificate authority (CA) certificates that the HTTP client uses to verify the authenticity of the HTTP server.
- **`certfile`**: Path to a PEM format file containing the SSL/TLS certificate chain for the client to present. If the certificate is not directly issued by a root CA, the intermediate CA certificates should be appended after the client certificate to form a complete chain.
- **`keyfile`**: Path to a PEM format private key corresponding to the client certificate specified by `certfile`.
- **`verify`**: Controls server certificate verification behavior:
  - **`verify_peer`**: Verify the server’s certificate chain.
  - **`verify_none`**: Do not verify the server’s certificate.

Managed certificates can also be referenced in configuration files for external resources, following the same certificate management model used by listeners. For more details about configuration options, see [Enable SSL/TLS with One-Way Authentication](./emqx-mqtt-tls.md#enable-via-configuration-file) listeners via configuration file.

## TLS for Nodes Communication

Instructions on how to enable SSL/TLS for cluster connections are not covered in this chapter, and you can refer to [Cluster Security](../deploy/cluster/security.md) for details.
