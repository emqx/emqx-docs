# SSL/TLS Certificates

SSL/TLS certificates are a core part of EMQX’s security architecture. They provide authentication, encryption, and data integrity for network communication. In EMQX, SSL/TLS certificates are required for establishing secure connections for the following scenarios:

- MQTT connections over TLS (MQTTS)
- MQTT connections over WebSocket Secure (WSS)
- HTTPS services and Dashboard access
- TLS-enabled external connections (for example, data integrations)

Starting from EMQX 6.1, certificates are treated as reusable resources rather than simple file paths. EMQX supports two certificate management approaches:

1. Path-based certificates (traditional): certificates referenced directly by file paths in configuration.
2. Managed certificates (EMQX 6.1+): certificates managed as reusable resources and referenced by name.

Both approaches use standard PEM-encoded files and are fully compatible with EMQX’s SSL/TLS implementation.

This page covers the following topics:

- Obtaining SSL/TLS certificates
- Managing certificates in EMQX
- Supporting multiple certificates
- Updating SSL/TLS Certificates

## Obtain SSL/TLS Certificates

Before certificates can be used by EMQX, they must be obtained from a trusted source. The method you choose depends on your environment and security requirements.

### Options for Obtaining Certificates

You can obtain TLS certificates in the following ways:

- **Self-signed certificates**

    Certificates issued by your own Certificate Authority (CA). These are recommended only for testing or controlled environments, as they are not trusted by default.

- **Apply for or purchase certificates issued by a trusted CA**

   Certificates obtained from a public or enterprise CA, such as:

   - [Let's Encrypt](https://letsencrypt.org/)
   - Cloud providers (for example, Huawei Cloud, Tencent Cloud)
   - Commercial CAs (for example, [DigiCert](https://www.digicert.com/))

   For production and enterprise deployments, certificates with OV or higher assurance levels are generally recommended.

<!--- **Automatic issuance using ACME**-->

<!--EMQX can automatically obtain and renew server certificates using the ACME protocol (for example, Let’s Encrypt).-->

### Create Self-Signed CA Certificates

Self-signed certificates are useful for testing, development, or private environments.

::: tip Prerequisite

[OpenSSL](https://www.openssl.org/) is installed.

:::

1. Run the following command to generate a key pair. The command will prompt you to enter a password to protect the key, which will be required for generating, issuing, and verifying the certificate. Keep the key and password secure.

   ```bash
   openssl genrsa -des3 -out rootCA.key 2048
   ```

2. Run the following command to generate a CA certificate using the private key from the key pair. The command will prompt you to set the certificate's Distinguished Name (DN).

   ```bash
   openssl req -x509 -new -nodes -key rootCA.key -sha256 -days 3650 -out rootCA.crt
   ```

### Issue Server Certificates

Use the CA certificate you just generated to issue a server certificate, which is used by EMQX listeners to prove their identity to clients. The server certificate is usually issued to the hostname, server name, or domain name (such as [www.emqx.com](https://www.emqx.com/en)). We need to use the CA key (rootCA.key), CA certificate (rootCA.crt), and server Certificate Signing Request (CSR) (server.csr) to generate the server certificate.

1. Run the following command to generate a key pair for the server certificate:

   ```bash
   openssl genrsa -out server.key 2048
   ```

2. Run the following command to create a CSR using the server key pair. After the CSR is signed by the CA root certificate private key, a certificate public key file can be generated and issued to the user. This command will also prompt you to set the Distinguished Name (DN) for the certificate.

   ```bash
   openssl req -new -key server.key -out server.csr
   ```

   The system will prompt the following information, with corresponding meanings explained as below:

   ```bash
   You are about to be asked to enter information that will be incorporated
   into your certificate request.
   What you are about to enter is what is called a Distinguished Name or a DN.
   There are quite a few fields but you can leave some blank
   For some fields there will be a default value,
   If you enter '.', the field will be left blank.
   -----
   Country Name (2 letter code) [AU]: # country/region
   State or Province Name (full name) [Some-State]: # state/province
   Locality Name (eg, city) []: # The city or locality
   Organization Name (eg, company) [Internet Widgets Pty Ltd]: # The full name of the organization (or company name), e.g. EMQ
   Organizational Unit Name (eg, section) []: # The name of the department or division within the organization，e.g. EMQX
   Common Name (e.g. server FQDN or YOUR name) []: # The fully-qualified domain name (FQDN) of the server that will use the certificate, e.g. mqtt.emqx.com
   ...
   ```

3. Use the server CSR to generate the server certificate and specify the validity period of the certificate, which is set to 365 days in this case:

   ```bash
   openssl x509 -req -in server.csr -CA rootCA.crt -CAkey rootCA.key -CAcreateserial -out server.crt -days 365
   ```

   You now have a set of certificates.

   ```bash
   .
   ├── rootCA.crt
   ├── rootCA.key
   ├── rootCA.srl
   ├── server.crt
   ├── server.csr
   └── server.key
   ```

### Issue Client Certificates

Client certificates are used for two-way (mutual) TLS authentication.

The steps are similar to issuing server certificates, except that:

- The Common Name (CN) should uniquely identify the client (for example, a username or client ID).
- The certificate is signed using the same CA certificate. Therefore, the client certificate can also be signed with the aforementioned CA certificate.

## Certificate Management in EMQX

After certificates are obtained, EMQX supports two ways to manage and reference them.

### Path-Based Certificates

Path-based certificates are configured by specifying explicit file paths in listener or connector SSL options, such as:

- `certfile`
- `keyfile`
- `cacertfile`

With path-based certificates:

- Certificate files are managed entirely by the user or external tooling.
- Listeners reference certificate files directly.
- Certificate updates typically involve replacing files on disk.

EMQX provides sample certificates in the `etc/certs` directory for testing purposes only.

Path-based certificates remain fully supported and are compatible with all EMQX versions.

### Managed Certificates

Starting from EMQX 6.1, EMQX introduces managed certificates, a centralized mechanism for managing TLS certificate files that can be reused across multiple components.

Managed certificates can be referenced and reused across multiple resources, including:

- MQTT SSL listeners
- WSS listeners
- HTTPS / Dashboard listeners
- TLS-enabled connectors (e.g., data integrations)

They can be created and managed via the Dashboard or REST API, and are stored on disk under the EMQX data directory: `data/certs2/`.

Internally, EMQX continues to use path-based PEM paths when interfacing with Erlang/OTP’s SSL library, ensuring:

- Full backward compatibility with existing TLS behavior
- Automatic certificate reload
- No listener or connector restart required when certificates are updated

#### Managed Certificate Bundle

A managed certificate bundle represents a logical set of TLS-related files, including:

- Server certificate
- Private key
- Optional CA certificates

Each bundle is identified by a name and an optional namespace, and can be referenced by multiple listeners or connectors.

Managed certificates are a reusable resource. Updating a managed certificate bundle automatically affects all listeners or connectors that reference it.

#### Certificate Format and File Structure

Managed certificates in EMQX continue to use PEM-encoded files, consistent with EMQX’s existing TLS certificate deployment approach.

Using PEM files provides several benefits:

- **Transparency**: Certificate files can be easily inspected and verified using standard tools such as `openssl`.
- **Compatibility**: PEM is natively supported by Erlang/OTP’s SSL library, which provides:
  - File-based certificate caching
  - Automatic reload of updated certificate files

##### Certificate File Structure

Each managed certificate bundle is stored on disk as a directory identified by its certificate name and namespace.

Examples:

```
mqtt.example.com
tenant1/certs1
```

A managed certificate bundle may contain the following files:

| File name          | Required | Description                                       |
| ------------------ | -------- | ------------------------------------------------- |
| `key.pem`          | Yes      | Private key                                       |
| `chain.pem`        | Yes      | Certificate chain (excluding the root CA)         |
| `ca.pem`           | Optional | Root CA bundle used for validating peers          |
| `key-password.pem` | Optional | Key to decrypt the private key if it's encrypted. |

<!--> **Note**:-->

<!--> Not all files are required in every scenario. For example, `acc-key.pem` is present only when the certificate is issued or managed via ACME.-->

#### Multiple Certificates with SNI

When multiple managed certificate bundles are configured on the same listener, EMQX performs dynamic certificate selection based on Server Name Indication (SNI).

SNI is a TLS extension that allows a client to specify the hostname it is connecting to during the TLS handshake, before the secure connection is fully established.

##### How SNI Is Used

- Each managed certificate reference may optionally specify an `sni` value.
- During the TLS handshake:
  1. The client sends an SNI hostname.
  2. EMQX matches the SNI against configured certificate entries.
  3. If a match is found, the corresponding certificate bundle is used.
  4. If no match is found, the first certificate in the list is used as the default.

This mechanism allows a single listener to securely serve multiple domains or tenants using different TLS certificates on the same IP address and port.

##### Example

```hocon
listeners.ssl.default {
  bind = "0.0.0.0:8883"

  ssl_options {
    managed_certs = [
      {
        bundle_name = "default-cert"
        sni = "example.com"
      },
      {
        bundle_name = "example-cert-1"
        sni = "mqtt.example.com"
      }
    ]
  }
}
```

## Create and Manage Managed Certificates

This section covers the creation and management of managed certificate bundles using the Dashboard and REST API.

### Create Certificate Bundles via Dashboard

You can create managed certificate bundles directly from the Dashboard.

1. Go to **Management** -> **Certificate**.

2. Click **+ Create**.

4. In the **Create Managed Certs** panel, provide the following information:

   - **Name** (required): A unique name for the managed certificate bundle.

   - **Namespace**: Controls whether the managed certificate bundle is created in the global namespace or in a specific tenant namespace. 

     The switch is disabled by default, which means the certificate bundle is created in the global (`global`) namespace. When enabled, you can select a specific namespace and create the certificate bundle within that namespace.

     - Global administrators can create certificate bundles in the `global` namespace or in any tenant (non-global) namespace.
     - Namespace-scoped users can create certificate bundles only within their own namespace.

   - **TLS Cert** (required): The server certificate in PEM format. You can paste the certificate content directly or click **Select file** to upload a file. The certificate should include the complete certificate chain if required by clients.

   - **TLS Key** (required): The private key corresponding to the server certificate, in PEM format. You can paste the key content directly or upload a file.

   - **Key Password**: Optional. The passphrase for the private key, if the key is encrypted.

   - **CA Cert**: Optional. Certificate Authority certificates in PEM format. This is typically required when the certificate bundle is intended for:

     - two-way (mutual) TLS authentication, or
     - reuse by connectors that require CA certificates.

5. Click **Create** to save the managed certificate bundle.

Once created, the certificate bundle becomes available for selection and can be reused by multiple listeners or connectors.

> **Note**
> Managed certificate bundles are stored on disk and automatically reloaded by EMQX. Updating a managed certificate does not require restarting EMQX or its listeners.

### Manage Certificate Bundles via Dashboard

After a certificate bundle is created, it appears in the certificates list in the Dashboard, where you can view and manage all managed certificate bundles.

- Use the namespace drop-down at the top to switch between the global namespace and specific tenant (non-global) namespaces. The list updates automatically based on the selected namespace.
- Each certificate bundle is displayed with its **Name** and available **Actions**.

From this page, you can:

- View certificate bundles in the selected namespace.
- Edit a certificate bundle to update certificate contents, private keys, or CA certificates.
- Delete a certificate bundle that is no longer needed.

![certificate_bundle_list](./assets/certificate_bundle_list.png)

### Manage Certificate Bundles via REST API

In addition to the Dashboard, EMQX provides an REST API for managing TLS certificate files. Managed certificates created via the API are identical to those created via the Dashboard and can be referenced by listeners and connectors in the same way.

#### Upload Certificate Files

Upload certificate files to create or update a managed certificate bundle.

Supported file types:

- `key`: private key
- `chain`: certificate chain (excluding root CA)
- `ca`: CA certificate bundle
- `acc-key`: ACME account private key (server certificates only)

Upload a certificate file under a specified namespace：

```
POST /certs/ns/:NAMESPACE/name/:NAME?file=key|chain|ca|key-password
```

Upload a certificate file in the global namespace：

```
POST /certs/global/name/:NAME?file=key|chain|ca|key-password
```

#### List Certificate Bundles

List managed certificate bundles in a namespace:

```
GET /certs/ns/:NAMESPACE
```

List manged certificate bundles in the global namespace:

```
GET /certs/global/list
```

#### Delete Certificate Bundles

Delete an entire managed certificate bundle from a namespace:

```
DELETE /certs/ns/:NAMESPACE/name/:NAME
```

Delete an entire managed certificate bundle from the global namespace:

```
DELETE /certs/global/name/:NAME
```

<!--## Automatic Certificate Issuance with ACME (Managed Certificates)

EMQX supports automatic issuance and renewal of server-side TLS certificates through the ACME protocol (for example, Let’s Encrypt). This feature is optional and is intended for deployments where clients connect directly to EMQX over TLS using publicly accessible domain names. It is not suitable when TLS is terminated by a load balancer or when only private or internal hostnames are used.

ACME is enabled and configured by an EMQX administrator via configuration files. It cannot be enabled through the Dashboard and is not available to namespace-scoped users.

ACME applies only to server certificates used by MQTTS, HTTPS/Dashboard, and WSS listeners and does not apply to client certificates.

### How ACME Works with Managed Certificates

ACME integrates directly with managed certificates.

When ACME is enabled:

1. EMQX acts as an ACME client and requests certificates from the configured ACME CA.
2. Certificates are automatically renewed before expiration. 
3. The generated files are written into the managed certificates directory. The bundle contains:
   - `key.pem`
   - `chain.pem`
   - `acc-key.pem` (if applicable)
4. EMQX automatically creates a managed certificate bundle.

ACME-issued certificates behave exactly the same as manually uploaded managed certificates. Users only need to select or reference the generated managed certificate bundle when configuring listeners; no manual certificate upload is required.

### Enable ACME via Configuration File

This operation requires:

- Access to the EMQX host
- Permission to modify EMQX configuration files
- Administrative control over the deployment

Example configuration:

```hocon
## ACME CA directory URL (for example, Let's Encrypt)
acme.directory_url = "https://acme-v02.api.letsencrypt.org/directory"

## Contact email for ACME account registration
acme.contact_email = "admin@example.com"

## Domains to be included in the certificate
## Domain ownership is verified using the HTTP-01 challenge
acme.domains = ["mqtt1.example.com", "mqtt2.example.com"]

## Directory for storing issued certificates
## This directory is integrated with managed certificates
acme.cert_dir = "${EMQX_MANAGED_CERTS_DIR}"
```

-->

## Update SSL/TLS Certificates

SSL/TLS certificates must be updated before they expire to maintain secure connections. How certificates are updated in EMQX depends on the certificate management method in use.

### Update Path-Based Certificates

Path-based certificates are referenced directly by file paths in configuration (for example, `certfile`, `keyfile`, and `cacertfile`).

To update path-based certificates:

1. Replace the existing certificate files in the `./etc` or `/etc/emqx/etc` directory with the new certificate, key, or CA files.
2. Ensure the file paths referenced in the configuration remain unchanged.

EMQX automatically reloads updated certificate files:

- Certificate files are checked and reloaded periodically.
- By default, certificates are reloaded every 120 seconds.
- No listener restart is required in most cases.

### Update Managed Certificates

Managed certificates are updated by modifying the certificate bundle.

To update a managed certificate, upload a new certificate, private key, or CA certificate to the existing managed certificate bundle using the Dashboard, or the Managed Certificates API.

Once the bundle is updated:

- All listeners and TLS-enabled components that reference the managed certificate automatically use the updated certificate.
- No listener restart or EMQX restart is required.

Managed certificates are designed to be reusable. Updating a bundle affects all resources that reference it.

<!--### Automatic Renewal with ACME (Managed Certificates Only)

For managed certificates issued using ACME:

- EMQX automatically renews certificates before expiration.
- Renewed certificate files are written to the managed certificates directory.
- The corresponding managed certificate bundle is updated automatically.

After ACME is enabled and configured:

- No manual certificate upload is required.
- Listeners continue to reference the same managed certificate bundle.
- Certificate rotation occurs without service interruption.-->

## Next Steps

Once you obtain the SSL/TLS certificates, you can enable the client's SSL/TLS connections.

- [Enable SSL/TLS Connections](./emqx-mqtt-tls.md)