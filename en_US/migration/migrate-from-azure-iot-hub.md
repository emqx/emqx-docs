# Migrating from Azure IoT Hub to EMQX

This guide provides a practical walkthrough for migrating IoT devices from Azure IoT Hub to EMQX. It covers two migration paths:

1. **X.509 certificate authentication**: For devices using client certificates
2. **SAS token authentication**: For devices using Shared Access Signature (SAS) tokens with HTTP-based authentication

## Migration at a Glance

For devices using X.509 certificates, the migration is primarily a configuration change. Device certificates and private keys remain unchanged; only the broker endpoint and server CA certificate need updates. EMQX must be configured to trust the same CA that Azure trusts and to replicate Azure's identity mapping model, where the certificate Common Name (CN) equals the deviceId.


The migration process consists of three main phases:

1. **Locate Your CA Certificate**. Find the CA certificate that signed your device certificates.

2. **Configure EMQX for mTLS**. Set up an SSL/TLS listener on the EMQX broker, enable mandatory peer verification, and configure the listener to trust your CA and map the certificate CN to deviceId.

3. **Update Device Clients**. Update device code to connect to the EMQX endpoint and trust the EMQX server CA certificate. Devices can continue to use the Azure IoT SDK or utilize standard MQTT clients.

The following table summarizes the parameter changes:

| **Parameter** | **Azure IoT Hub (Example)** | **EMQX (Example)** | **Notes** |
| ------------- | -------------------------- | ------------------ | --------- |
| **Endpoint Hostname** | `my-hub.azure-devices.net` | `mqtt.example.com` | Update device client code |
| **Device Certificate** | `device-001.cert.pem` | `device-001.cert.pem` | No change. Devices continue using the existing certificate |
| **Device Private Key** | `device-001.key.pem` | `device-001.key.pem` | No change. Devices continues using the existing private key |
| **Server Verification** (Device trusts Server) | Device trusts Azure's public CA | Device must trust `emqx-server-ca.pem` | Deploy EMQX server CA to devices |
| **Client Verification** (Server trusts Device) | Azure trusts your CA (registered via CA upload or thumbprint) | EMQX `cacertfile` must be set to your CA | Same CA used in Azure |
| **Identity Mapping** | Azure extracts `CN=deviceId` | Enable `mqtt.peer_cert_as_clientid = cn` | Preserves deviceId-based authorization |

## Phase 1: Locate Your CA Certificate

**What you need**: The CA certificate that signed your device certificates (in PEM format, e.g., `device-ca.pem`). This certificate is essential for EMQX to verify device identities during mTLS authentication.

Azure IoT Hub supports two X.509 registration methods:
- **CA registration**: You uploaded a CA to Azure IoT Hub. You must locate the same CA file that you originally uploaded.
- **Thumbprint registration**: You registered each device individually by its certificate thumbprint. Although no CA was uploaded to Azure, the device certificates were still signed by a CA (such as an internal CA, a self-signed CA, or an enterprise PKI). You must locate the CA that issued these certificates.

Regardless of the method, the certificate hierarchy is the same: your devices are always signed by your own CA. For migration to EMQX, you must obtain this CA certificate so EMQX can verify your devices.

### Identify the CA That Issued Your Device Certificates

Use OpenSSL to inspect the Issuer field in a device certificate:

```bash
openssl x509 -in device-001.cert.pem -noout -issuer
```

Expected output:

```
issuer=CN = Azure-Device-CA
```

The corresponding CA file (e.g., `Azure-Device-CA.pem`) is the CA certificate you must provide to EMQX. This is the most reliable way to determine the correct CA, especially when using thumbprint registration.

### Verify Certificate Requirements

Azure requires that the certificate Subject Common Name (CN) match the deviceId (or `deviceId/moduleId` for modules). You can verify this using:

```bash
openssl x509 -in device-001.cert.pem -noout -subject
```

Expected output:
```
subject=CN = device-001
```

EMQX extracts this CN during mTLS authentication and use it as the device’s identity.

### Confirm Device Credential Access

Each device retains secure access to its own credentials:
- The device's leaf certificate (`device-001.cert.pem`)
- The device's private key (`device-001.key.pem`)

Because Azure IoT Hub and EMQX both use standard X.509 authentication, no certificate re-provisioning is required for this migration path.

## Phase 2: Configure EMQX for Azure-Compatible mTLS

Configure EMQX to authenticate devices using the same CA and identity-mapping rules that Azure IoT Hub uses for X.509 authentication.

### Enable and Configure the mTLS Listener

Configure EMQX to enable two-way SSL/TLS authentication (mTLS) on the SSL listener. For detailed information on SSL/TLS configuration, see [Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md).

Open the EMQX configuration file (`emqx.conf`) and configure the SSL/TLS listener, or use the Dashboard (**Management** -> **Listeners**):

```hocon
listeners.ssl.default {
  bind = "0.0.0.0:8883"

  ssl_options {
    # Your EMQX server's certificate
    certfile = "etc/certs/server-cert.pem"

    # Your EMQX server's private key
    keyfile = "etc/certs/server-key.pem"

    # --- mTLS Configuration for Device Authentication ---

    # The CA certificate that signed your device certificates
    cacertfile = "etc/certs/azure-device-ca.pem"

    # Enable client certificate verification
    verify = verify_peer

    # Reject clients that do not present a certificate
    fail_if_no_peer_cert = true
  }
}
```

::: tip
Both Azure IoT Hub and EMQX use port `8883` as the default for MQTT over TLS/SSL, so no port changes are needed in device clients.
:::

**Key Configuration Parameters**:
* `cacertfile`: Path to your CA certificate (or bundle of self-signed device certificates). EMQX will use this to verify device certificates.
* `verify`: Must be set to `verify_peer` to enable mTLS.
* `fail_if_no_peer_cert`: Must be set to `true` to enforce certificate requirement.

### Replicate Azure's CN=deviceId Identity Mapping

Azure IoT Hub extracts the certificate's Common Name and uses it as the deviceId for authorization. Replicate this in EMQX:

```hocon
mqtt.peer_cert_as_clientid = cn
mqtt.peer_cert_as_username = cn
```

This configuration ensures that:
- The MQTT ClientID is automatically set to the certificate CN (deviceId)
- The username is also set to the certificate CN
- You can configure EMQX ACL rules using `${clientid}` or `${username}` to match the deviceId, replicating Azure's authorization model

For devices using modules (`deviceId/moduleId` format), the CN contains both identifiers and can be used directly in EMQX ACLs.

### Apply Configuration Changes

After updating the configuration file, reload the configuration:

```bash
emqx ctl conf reload
```

If you made changes via the Dashboard, click **Update** to apply them. The listener will restart automatically to apply the new settings.

Verify the listener is enforcing mTLS:

```bash
openssl s_client -connect mqtt.example.com:8883 -showcerts
```

The connection should fail without a client certificate.

## Phase 3: Update Device Clients and Verify Migration

The final phase is to update the device client code to connect to EMQX instead of Azure IoT Hub.

### Prepare EMQX Server CA Certificate

Before updating the device code, you need to obtain the EMQX server's CA certificate. This is the CA that signed the EMQX server's TLS certificate.

**For self-signed EMQX server certificates**, you must add the server CA to your device's trusted certificate store:

**Linux**:

```bash
# Copy CA to system trust store
sudo cp emqx-server-ca.pem /usr/local/share/ca-certificates/emqx-ca.crt
sudo update-ca-certificates
```

**macOS**:

```bash
# Add to system keychain
sudo security add-trusted-cert -d -r trustRoot -k /Library/Keychains/System.keychain emqx-server-ca.pem
```

**Windows**:

```powershell
# Import certificate to Trusted Root CA store
Import-Certificate -FilePath emqx-server-ca.pem -CertStoreLocation Cert:\LocalMachine\Root
```

::: tip
If your EMQX server uses a certificate from a public CA (like Let's Encrypt), this step is not needed as the CA is already trusted by the system.
:::

### Update Device Client Code

The Azure IoT SDK for Python (and other languages) supports connecting to custom MQTT brokers through the `server_verification_cert` and custom `hostname` parameters. This allows for minimal code changes.

**Python Example**:

```python
from azure.iot.device import IoTHubDeviceClient, X509

# Load device credentials
x509 = X509(
    cert_file="certs/device-001.cert.pem",
    key_file="certs/device-001.key.pem"
)

# Read EMQX server CA certificate content
with open("certs/emqx-server-ca.pem", "r") as f:
    emqx_server_ca = f.read()

# Create client pointing to EMQX
client = IoTHubDeviceClient.create_from_x509_certificate(
    x509=x509,
    hostname="mqtt.example.com",  # EMQX hostname instead of Azure
    device_id="device-001",
    server_verification_cert=emqx_server_ca  # CA cert content as string
)

# Connect and use as before
client.connect()
client.send_message("Hello from migrated device")
```

::: tip
- The `server_verification_cert` parameter expects the certificate **content as a string**, not a file path.
- If you've added the EMQX server CA to your system's trusted certificate store (recommended), you can omit this parameter and let the system handle verification.
- Using the Azure IoT SDK preserves your existing application code structure, requiring only configuration changes. This is the simplest migration path for devices already using X.509 authentication.
:::

### Device-Side Parameter Summary

These are the parameter changes needed:

1. **Endpoint/Hostname**:
   - Azure: `my-hub.azure-devices.net`
   - EMQX: `mqtt.example.com`

2. **Server CA Certificate**:
   - Azure: Uses the system trust store or Azure CA
   - EMQX: Must explicitly provide `emqx-server-ca.pem`

3. **Device Credentials** (no changes):
   - Certificate: Keep the existing device certificate
   - Private key: Keep the existing private key

4. **ClientId**: Set to deviceId (matching certificate CN)

### Validation Checklist

- Device appears in EMQX Dashboard with `clientid = deviceId`.
- TLS handshake succeeds, and the device certificate is verified.
- The device can publish to authorized topics.
- The device can subscribe to authorized topics.
- No authentication errors in EMQX logs.

## Variations of the Standard Migration Path

Besides the core migration workflow described above, some device fleets follow simple variations that still fit within the same X.509-based migration process. This section highlights two common variations and explains how EMQX accommodates them without requiring changes to device certificates or firmware.

### CA-Signed Device Fleets

- Upload the CA certificate to EMQX.
- All devices whose certificates were signed by this CA will be trusted automatically.
- Certificate lifecycle management remains centralized and simple.
- New devices can be added without any EMQX configuration changes.

This scenario mirrors Azure IoT Hub’s CA-based provisioning model and offers the most scalable migration path for large fleets.

### Devices Using Modules (`deviceId/moduleId`)

- Devices whose certificates include a Common Name (CN) in the format `deviceId/moduleId` are fully supported.
- EMQX can use the full CN for identity mapping and authentication.
- Authorization rules (ACLs) can reference the entire CN, preserving Azure’s module-level access control behavior.

This allows devices using Azure’s module hierarchy to migrate seamlessly, without certificate modification or custom identity logic.

## Alternative: SAS Token Authentication with HTTP Authenticator

If your devices use Azure SAS tokens, you can continue using this authentication method in EMQX by implementing a simple HTTP authentication service. For detailed information on HTTP authentication, see [Use HTTP Service](../access-control/authn/http.md).

### How SAS Token Authentication Works

Azure IoT Hub sends SAS credentials through the MQTT username and password fields:

- **Username**: `{iothubhostname}/{deviceId}/?api-version=2021-04-12`
- **Password**: `SharedAccessSignature sr={resource}&sig={signature}&se={expiry}`

EMQX forwards these values to your HTTP service, which performs the actual SAS token validation.

### Implement HTTP Authentication for SAS Tokens

1. Create an HTTP authentication service. Your service should perform the following tasks:
   - Receive the username and password from EMQX.
   - Extract the `deviceId` from the username.
   - Parse the SAS token from the password field.
   - Validate the token signature using the device's symmetric key.
   - Check the token expiry value (`se` field).
   - Return `{"result": "allow"}` or `{"result": "deny"}` based on the validation result.

2. Configure EMQX HTTP Authenticator to use your HTTP service. Add an HTTP authenticator either through the Dashboard or configuration file:

```hocon
authentication = [
  {
    mechanism = password_based
    backend = http
    method = post
    url = "http://your-auth-service:8080/auth"
    body {
      username = "${username}"
      password = "${password}"
      clientid = "${clientid}"
    }
    headers {
      "Content-Type" = "application/json"
    }
  }
]
```

3. Provision Device Credentials. Export device identities and symmetric keys from the Azure IoT Hub identity registry. Store them in the database used by your HTTP authentication service so it can validate SAS signatures.

### Example HTTP Authentication Service Response

Your service should respond with JSON similar to the following:

```json
{
  "result": "allow",
  "is_superuser": false,
  "client_attrs": {
    "device_id": "device-001"
  }
}
```

::: tip
This approach allows SAS token-based devices to migrate without firmware changes. However, for long-term portability and security, migrating to X.509 certificate authentication is recommended.
:::

## Conclusion

Migrating devices from Azure IoT Hub to EMQX can follow one of two authentication paths, depending on how your devices are currently provisioned.

### X.509 Certificate–Based Devices

This is the simplest and most direct migration path. Your existing device certificates and private keys remain unchanged, and only the following updates are required:

- Configure EMQX to trust the same CA used in Azure
- Enable mTLS and certificate-based identity mapping
- Update device endpoints and server CA certificates

With these adjustments, devices can connect to EMQX while preserving the same security model and certificate workflow.

### SAS Token–Based Devices

Devices using Azure SAS tokens can continue using them in EMQX by implementing an HTTP authentication service that validates token signatures and expiry. This enables migration without firmware changes.

However, for long-term portability and stronger security, transitioning to X.509 certificates is recommended.

::: tip
If your deployment includes both X.509 and SAS token devices, consider migrating the X.509 fleet first to reduce effort and accelerate validation. Then evaluate whether SAS token devices should use an HTTP authentication service for immediate compatibility or migrate to X.509 certificates for long-term maintainability.
:::
