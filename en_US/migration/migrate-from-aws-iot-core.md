# Migrate from AWS IoT Core to EMQX

This page provides a comprehensive walkthrough for migrating IoT devices from AWS IoT Core to EMQX. It outlines the process for reconfiguring devices and the EMQX broker to ensure a seamless transition for an entire device fleet.

The guide focuses on the most common and robust authentication method supported by both platforms: X.509 Client Certificate (mTLS) Authentication. This guide assumes you are using your own custom Certificate Authority (CA) registered with AWS IoT Core to sign device certificates. If your devices were provisioned using AWS-issued (“one-click”) certificates, you cannot reuse those certificates. AWS does not expose the intermediate CAs used to sign these certificates, so they cannot be trusted by MQTT brokers such as EMQX. In this case, you must create your own CA and re-issue device certificates.

## Migration at a Glance: Standard Flow

For devices using X.509 client certificates (mTLS) signed by your own CA with AWS IoT Core, the migration to EMQX is straightforward. Standard-compliant clients, including the official AWS IoT Device SDKs, can connect to EMQX with minimal changes to the client-side code: only the endpoint and server CA certificate need to be updated. Your existing device certificates and private keys remain valid.

The migration process consists of three main phases:

1. **Prepare Your CA Certificate**. Locate your custom CA certificate that was registered with AWS IoT Core and is currently used to sign your device certificates.

2. **Configure EMQX for mTLS**. Set up an SSL/TLS listener on the EMQX broker, enable mandatory peer verification, and configure the listener to trust your CA.

3. **Update Device Clients**. Update the device client code with the new EMQX endpoint address and the EMQX server's CA certificate for server verification.

The following table summarizes the parameter changes required at a high level.

| **Parameter** | **AWS IoT Core (Example)** | **EMQX (Example)** | **Notes** |
| ------------- | -------------------------- | ------------------ | --------- |
| **Endpoint Hostname**  | `agwba84cbf2pn-ats.iot.eu-west-1.amazonaws.com` | `mqtt.example.com` | Update device client code/firmware |
| **Endpoint Port**  | `8883` (MQTT/TLS), `443` (MQTT/TLS or Websocket/TLS) | `8883` (MQTT/TLS), `8084` (Websocket/TLS) | Devices using port `8883` require no change. If devices connect via WebSocket (`443`), update device client code/firmware to use port `8084` in EMQX. |
| **Device Certificate** | `device-001.cert.pem` | `device-001.cert.pem` | No change. The device continues using its existing certificate signed by your CA. |
| **Device Private Key** | `device-001.key.pem` | `device-001.key.pem` | No change. The device continues using its existing private key. |
| **Server Verification** (Device trusts Server) | Device client uses `AmazonRootCA1.pem` | Device client must be updated to use `emqx-server-ca.pem` | The client must trust the CA that issued the EMQX server's certificate. |
| **Client Verification** (Server trusts Device) | AWS IoT Core trusts your registered CA | EMQX listener's `cacertfile` must be set to `your-ca.pem`, and `verify` must be set to `verify_peer` | Configure EMQX to trust the same CA you registered with AWS IoT Core. |

## Phase 1: Prepare Your CA Certificate

This guide assumes you are using your own custom Certificate Authority (CA) that was registered with AWS IoT Core using the "Bring Your Own CA" (BYOCA) feature. Your device certificates are signed by this CA, not by AWS's proprietary intermediate CAs.

**Action**: Locate your CA certificate file (e.g., `my-company-ca.pem`). This is the same CA certificate that you registered with AWS IoT Core and used to sign your device certificates.

You can verify which CA signed your device certificate by inspecting it:

```bash
openssl x509 -in device-001.cert.pem -text -noout | grep "Issuer"
```

The issuer should match your organization's CA, not AWS intermediate CAs.

::: tip
**If you are using AWS-issued certificates**, AWS IoT Core's "one-click" certificate generation uses proprietary intermediate CAs that are not accessible to customers. If your devices use AWS-issued certificates, you will need to create your own CA and re-issue certificates to your devices before migrating to EMQX. This is beyond the scope of this guide, but you can use standard tools like OpenSSL or a PKI solution to create a CA and issue device certificates.
:::

## Phase 2: Configure EMQX for mTLS Authentication

With your CA certificate located, the next step is to configure the EMQX broker to accept and authenticate devices using certificates signed by your CA.

### Enable and Configure the mTLS Listener

The core of the migration is enabling two-way SSL/TLS authentication (mTLS) on an EMQX listener. This configuration instructs EMQX to demand a certificate from the connecting client and verify its authenticity against your CA.

For detailed information on SSL/TLS configuration options, see [Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md). For certificate management, see [TLS Certificates](../network/tls-certificate.md).

**Action**: Open the EMQX configuration file (e.g., `emqx.conf`) and configure the SSL/TLS listener, or use the Dashboard (**Management** -> **Listeners**):

```hocon
listeners.ssl.default {
  bind = "0.0.0.0:8883"

  ssl_options {
    # Your EMQX server's certificate
    certfile = "etc/certs/server-cert.pem"

    # Your EMQX server's private key
    keyfile = "etc/certs/server-key.pem"

    # --- mTLS Configuration for Device Authentication ---

    # This is YOUR CA certificate (from Phase 1)
    # The same CA that was registered with AWS IoT Core
    cacertfile = "etc/certs/my-company-ca.pem"

    # Enable client certificate verification
    verify = verify_peer

    # Reject clients that do not present a certificate
    fail_if_no_peer_cert = true
  }
}
```

::: tip
Both AWS IoT Core and EMQX use port `8883` as the default for MQTT over TLS/SSL, so no port changes are needed in your device clients.
:::

**Key Configuration Parameters**:

* `cacertfile`: Path to your CA certificate file that you registered with AWS IoT Core. This allows EMQX to verify that connecting device certificates are authentic.
* `verify`: Must be set to `verify_peer` to enable mTLS.
* `fail_if_no_peer_cert`: Must be set to `true` to reject connections without a client certificate, enforcing mTLS.
* `certfile` and `keyfile`: Your EMQX server's own certificate and private key. Clients will verify this certificate to ensure they're connecting to the correct broker.

After updating the configuration file, reload the configuration:

```bash
emqx ctl conf reload
```

If you made changes via the Dashboard, click **Update** to apply them. The listener will restart automatically to apply the new settings.

### (Optional) Map Certificate CN to ClientID or Username

In many AWS IoT Core implementations, authorization policies rely on variables populated from the certificate, such as using the certificate's Common Name (CN) as the `iot:ClientId`. EMQX can replicate this behavior seamlessly, allowing for easier migration of authorization rules.

**Action**: To automatically populate the MQTT ClientID or Username from the device certificate, add the following to your `emqx.conf`:

```hocon
# To use the certificate Common Name (CN) as the ClientID
mqtt.peer_cert_as_clientid = cn

# To use the certificate Common Name (CN) as the Username
mqtt.peer_cert_as_username = cn
```

This configuration instructs EMQX to extract the Common Name (or other fields like `dn` for Distinguished Name) from the peer certificate during the TLS handshake and use it as the ClientID or Username for the MQTT session. This ensures that existing authorization logic (e.g., ACLs based on `${clientid}` or `${username}`) will continue to function after migration.

For example, if your device certificates have CN=device-001, enabling `mqtt.peer_cert_as_clientid = cn` will automatically set the MQTT client ID to "device-001" during connection.

## Phase 3: Update Device Clients and Verify Migration

The final phase is to update the device client code to point to the new EMQX broker endpoint. This process is demonstrated using the official [AWS IoT Device SDK for Python v2](https://github.com/aws/aws-iot-device-sdk-python-v2).

AWS IoT SDKs are not locked into the AWS platform. They function as standard-compliant MQTT-over-TLS clients. This means existing application code built on these SDKs can be preserved, requiring changes only to the connection endpoint and server CA certificate parameters.

### Client-Side Code Modifications (Python Example)

Based on the `mqtt5_client_builder` module from the `aws-iot-device-sdk-python-v2`, you need to update the connection parameters to migrate from AWS IoT Core to EMQX:

1. **Update the Endpoint**:
   * AWS: `endpoint="agwba84cbf2pn-ats.iot.eu-west-1.amazonaws.com"`
   * EMQX: `endpoint="mqtt.example.com"` (Your EMQX broker's hostname/FQDN)

2. **Update the Server CA Certificate** (`ca_filepath`):
   * This is how the device verifies the EMQX server's identity.
   * AWS: Often omitted (defaults to system trust store) or `ca_filepath="AmazonRootCA1.pem"` (Amazon Root CA 1)
   * EMQX: `ca_filepath="emqx-server-ca.pem"` (The CA that issued your EMQX server certificate)

3. **Device Certificates Remain the Same**:
   * **Device certificate** (`cert_filepath`): No change needed. Devices continue using their existing certificates signed by your CA.
   * **Device private key** (`pri_key_filepath`): No change needed. Devices continue using their existing private keys.

### Full Example: Connecting the AWS SDK to EMQX

The following example demonstrates how to connect to EMQX using the AWS IoT Device SDK for Python v2. The `samples/mqtt/mqtt5_x509.py` sample script can be used with minimal modifications.

**AWS IoT Core version** (before migration):
```bash
python3 mqtt5_x509.py \
  --endpoint agwba84cbf2pn-ats.iot.eu-west-1.amazonaws.com \
  --cert device-001.cert.pem \
  --key device-001.key.pem \
  --client_id basicPubSub \
  --topic sdk/test/python \
  --count 10
```

**EMQX version** (after migration):
```bash
python3 mqtt5_x509.py \
  --endpoint mqtt.example.com \
  --cert device-001.cert.pem \
  --key device-001.key.pem \
  --client_id basicPubSub \
  --topic sdk/test/python \
  --count 10
```

Note that the certificate and key parameters remain unchanged. Only the endpoint changes.

If you need to explicitly specify the server CA certificate (when not using the system trust store), you can modify the SDK sample to add the `ca_filepath` parameter:

```python
# In mqtt5_x509.py, locate the mqtt5_client_builder.mtls_from_path() call
# and add the ca_filepath parameter:

client = mqtt5_client_builder.mtls_from_path(
    endpoint=args.input_endpoint,
    cert_filepath=args.input_cert,
    pri_key_filepath=args.input_key,
    ca_filepath="emqx-server-ca.pem",  # Add this line
    on_publish_received=on_publish_received,
    on_lifecycle_stopped=on_lifecycle_stopped,
    on_lifecycle_attempting_connect=on_lifecycle_attempting_connect,
    on_lifecycle_connection_success=on_lifecycle_connection_success,
    on_lifecycle_connection_failure=on_lifecycle_connection_failure,
    on_lifecycle_disconnection=on_lifecycle_disconnection,
    client_id=args.input_clientId
)
```

**Key Changes Summary**:
- **Endpoint**: Changed from AWS IoT Core endpoint to your EMQX broker hostname.
- **Server CA**: Optionally specify the CA that signed your EMQX server certificate.
- **Device Certificates**: No changes. Devices continue using existing certificates and private keys.
- **Application logic**: No changes required. Publish, subscribe, and message handling code remains identical.

Running this updated command will verify a successful connection, subscribe, and publish, confirming that the device migration is complete.

## Advanced Migration Scenarios

The same migration approach applies to more advanced connection scenarios.

### Migrate Devices Using PKCS11 (HSMs)

For devices that store private keys in a Hardware Security Module (HSM) for enhanced security, the migration process is straightforward. The private keys remain in the HSM, and the device certificates remain valid as long as they were signed by your custom CA.

**Client-Side Code Modification**:

The EMQX server-side configuration (Phase 2) remains the same. On the client side, use the `mtls_with_pkcs11` builder with the updated endpoint:

```python
client = mqtt5_client_builder.mtls_with_pkcs11(
    # CHANGED: Your EMQX broker's hostname
    endpoint="mqtt.example.com",

    # CHANGED: Point to your EMQX server's CA
    ca_filepath="emqx-server-ca.pem",

    # Device certificate (no change - same cert signed by your CA)
    cert_filepath="device-001.cert.pem",

    # HSM Configuration (no changes)
    pkcs11_lib="/path/to/pkcs11.so",
    user_pin="YOUR-HSM-PIN",
    slot_id=pkcs11_slot_id,
    token_label="YOUR-TOKEN-LABEL",
    private_key_label="device-001-key",

    on_publish_received=on_publish_received,
    # ... other callbacks ...
    client_id="device-001"
)
```

### Migrate Devices Connecting via an HTTP Proxy

For devices in restricted networks that must connect via an HTTP Proxy, the migration process remains the same as the standard path. The mTLS connection is tunneled through an HTTP CONNECT request.

The EMQX server-side configuration (Phase 2) is **the same**. The proxy is transparent to the EMQX listener, which only sees the incoming mTLS connection.

The client SDK configuration must include the proxy settings in addition to the updated endpoint.

**Client-Side Code Modification (Python Example)**:

```python
from awscrt import http

# 1. Configure the HTTP Proxy
http_proxy_options = http.HttpProxyOptions(
    host_name="my-proxy.my-network.com",
    port=8888
)

# 2. Create client with proxy options
client = mqtt5_client_builder.mtls_from_path(
    # CHANGED: Your EMQX broker's hostname
    endpoint="mqtt.example.com",

    # CHANGED: Point to your EMQX server's CA
    ca_filepath="emqx-server-ca.pem",

    # Device credentials (no changes - same certs signed by your CA)
    cert_filepath="device-001.cert.pem",
    pri_key_filepath="device-001.key.pem",

    # Add the proxy configuration
    http_proxy_options=http_proxy_options,

    on_publish_received=on_publish_received,
    # ... other callbacks ...
    client_id="device-001"
)
```

## Conclusion

Migrating mTLS-based devices from AWS IoT Core to EMQX is straightforward when using your own custom Certificate Authority. The process is primarily a configuration change rather than a re-provisioning effort.

By following the three phases outlined in this guide:
1. Locating your custom CA certificate
2. Configuring the EMQX broker for mTLS authentication with your CA
3. Updating device client endpoints

Your device fleet can be successfully migrated to EMQX while maintaining the same robust mTLS authentication. The migration preserves existing device certificates, private keys, and application logic in AWS IoT Device SDKs, requiring only minimal connection parameter updates. Organizations can efficiently transition their IoT infrastructure to EMQX with minimal disruption while maintaining security best practices.

::: tip
If your devices are currently using AWS-issued certificates (one-click method), you will need to establish your own CA infrastructure and re-provision device certificates before migration. This is a prerequisite for moving away from AWS's proprietary certificate chain.
:::
