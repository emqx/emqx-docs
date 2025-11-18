# Migrating from Azure IoT Hub to EMQX

This guide provides a practical walkthrough for migrating IoT devices from Azure IoT Hub to EMQX. It covers two migration paths:

1. **X.509 certificate authentication** - Devices using client certificates
2. **SAS token authentication** - Devices using Shared Access Signature tokens with HTTP-based authentication

## Migration at a Glance

For devices using X.509 certificates, the migration is primarily a configuration change. Device certificates and private keys remain unchanged; only the broker endpoint and server CA certificate need updates. EMQX must be configured to trust the same CA that Azure trusts and to replicate Azure's identity mapping model where the certificate Common Name (CN) equals the deviceId.


The migration process consists of three main phases:

1. **Locate Your CA Certificate**. Find the CA certificate that signed your device certificates.

2. **Configure EMQX for mTLS**. Set up an SSL/TLS listener on the EMQX broker, enable mandatory peer verification, and configure the listener to trust your CA and map certificate CN to deviceId.

3. **Update Device Clients**. Update device code to connect to the EMQX endpoint and trust the EMQX server CA certificate. Devices can continue using Azure IoT SDK or use standard MQTT clients.

The following table summarizes the parameter changes:

| **Parameter** | **Azure IoT Hub (Example)** | **EMQX (Example)** | **Notes** |
| ------------- | -------------------------- | ------------------ | --------- |
| **Endpoint Hostname** | `my-hub.azure-devices.net` | `mqtt.example.com` | Update device client code |
| **Device Certificate** | `device-001.cert.pem` | `device-001.cert.pem` | No change. Device continues using existing certificate |
| **Device Private Key** | `device-001.key.pem` | `device-001.key.pem` | No change. Device continues using existing private key |
| **Server Verification** (Device trusts Server) | Device trusts Azure's public CA | Device must trust `emqx-server-ca.pem` | Deploy EMQX server CA to devices |
| **Client Verification** (Server trusts Device) | Azure trusts your CA (registered via CA upload or thumbprint) | EMQX `cacertfile` must be set to your CA | Same CA used in Azure |
| **Identity Mapping** | Azure extracts `CN=deviceId` | Enable `mqtt.peer_cert_as_clientid = cn` | Preserves deviceId-based authorization |

## Phase 1: Locate Your CA Certificate

**What you need**: The CA certificate that signed your device certificates (in PEM format, e.g., `device-ca.pem`).

Azure IoT Hub has two X.509 registration methods:
- **CA registration**: You uploaded the CA to Azure IoT Hub
- **Thumbprint registration**: You registered devices individually by certificate thumbprint

**Both methods use the same certificate structure** - your device certificates were signed by a CA. For EMQX migration, you need that CA certificate.

### Verify Certificate Requirements

Azure requires that the certificate Subject Common Name (CN) matches the deviceId (or `deviceId/moduleId` for modules). Verify with:

```bash
openssl x509 -in device-001.cert.pem -noout -subject
```

The output should show:
```
subject=CN = device-001
```

This CN value will be used by EMQX to identify the device.

### Confirm Device Credential Access

Ensure each device retains secure access to:
- Its leaf certificate (`device-001.cert.pem`)
- Its private key (`device-001.key.pem`)

No certificate re-provisioning is needed for this migration path.

## Phase 2: Configure EMQX for Azure-Style mTLS

Configure the EMQX broker to authenticate devices using the same certificates trusted by Azure IoT Hub.

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
Both Azure IoT Hub and EMQX use port 8883 as the default for MQTT over TLS/SSL, so no port changes are needed in device clients.
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

The final phase is to update device client code to connect to EMQX instead of Azure IoT Hub.

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

# Create client pointing to EMQX
client = IoTHubDeviceClient.create_from_x509_certificate(
    x509=x509,
    hostname="mqtt.example.com",  # EMQX hostname instead of Azure
    device_id="device-001",
    server_verification_cert="certs/emqx-server-ca.pem"  # EMQX server CA
)

# Connect and use as before
client.connect()
client.send_message("Hello from migrated device")
```

**C# Example**:

```csharp
var auth = new DeviceAuthenticationWithX509Certificate(
    deviceId: "device-001",
    certificate: new X509Certificate2("device-001.pfx", "password")
);

var options = new ClientOptions
{
    // Point to EMQX instead of Azure IoT Hub
    ModelId = "",
    CertificateValidationCallback = (sender, certificate, chain, errors) =>
    {
        // Validate against EMQX CA
        return ValidateServerCertificate(certificate, "emqx-server-ca.pem");
    }
};

var client = new DeviceClient(
    hostname: "mqtt.example.com",  // EMQX hostname
    authenticationMethod: auth,
    transportType: TransportType.Mqtt_Tcp_Only,
    options: options
);

await client.OpenAsync();
```

::: tip
Using the Azure IoT SDK preserves your existing application code structure, requiring only configuration changes. This is the simplest migration path for devices already using X.509 authentication.
:::

### Device-Side Parameter Summary

These are the parameter changes needed:

1. **Endpoint/Hostname**:
   - Azure: `my-hub.azure-devices.net`
   - EMQX: `mqtt.example.com`

2. **Server CA Certificate**:
   - Azure: Uses system trust store or Azure CA
   - EMQX: Must explicitly provide `emqx-server-ca.pem`

3. **Device Credentials** (no changes):
   - Certificate: Keep existing device certificate
   - Private key: Keep existing private key

4. **ClientId**: Set to deviceId (matching certificate CN)

### Validation Checklist

1. Device appears in EMQX Dashboard with `clientid = deviceId`
2. TLS handshake succeeds and device certificate is verified
3. Device can publish to authorized topics
4. Device can subscribe to authorized topics
5. No authentication errors in EMQX logs

## Happy Path Variations

### CA-Signed Fleet

- Upload the CA certificate to EMQX
- All devices signed by this CA are automatically trusted
- Simplified certificate lifecycle management
- Easy to add new devices without EMQX reconfiguration

### Modules (deviceId/moduleId)

- Certificates with CN in format `deviceId/moduleId`
- EMQX can use the full CN for authorization
- Reflect the same structure in ACL rules

## Alternative: SAS Token Authentication with HTTP Authenticator

Devices using Azure Shared Access Signature (SAS) tokens can continue using them with EMQX by implementing an **HTTP Authentication** service. For detailed information on HTTP authentication, see [Use HTTP Service](../access-control/authn/http.md).

### How SAS Token Authentication Works

Azure SAS tokens are passed in the MQTT password field with a specific format:
- **Username**: `{iothubhostname}/{deviceId}/?api-version=2021-04-12`
- **Password**: `SharedAccessSignature sr={resource}&sig={signature}&se={expiry}`

### Implement HTTP Authentication for SAS Tokens

1. **Create an HTTP authentication service** that:
   - Receives the username and password from EMQX
   - Extracts the deviceId from the username
   - Parses the SAS token from the password field
   - Validates the token signature using the device's symmetric key
   - Checks the token expiry (`se` field)
   - Returns `{"result": "allow"}` or `{"result": "deny"}`

2. **Configure EMQX HTTP Authenticator** via Dashboard or configuration file:

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

3. **Provision Device Credentials**: Export device identities and symmetric keys from Azure IoT Hub identity registry and provision them in your authentication service's database.

### Example HTTP Authentication Service Response

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

Migrating devices from Azure IoT Hub to EMQX offers flexible paths depending on your authentication method:

**For X.509 certificate-based devices**: The migration is straightforward when using your own Certificate Authority. Device certificates and private keys remain unchanged, requiring only endpoint updates and server CA deployment. Follow the three phases: locating your CA certificate, configuring EMQX for mTLS with CN-based identity mapping, and updating device clients to successfully migrate while maintaining the same security model.

**For SAS token-based devices**: Devices can continue using SAS tokens by implementing an HTTP authentication service that validates token signatures and expiry. This allows migration without firmware changes, though transitioning to X.509 certificates is recommended for long-term portability.

::: tip
Focus your initial migration on X.509 CA-signed devices to achieve quick wins. For SAS token devices, evaluate whether to implement HTTP authentication for immediate migration or refactor to X.509 certificates for better long-term maintainability.
:::
