# Use AVEVA PI System with EMQX

AVEVA PI System includes a PI Connector for MQTT that can subscribe to MQTT topics and ingest data directly into PI Data Archive via the PI Asset Framework. This page explains how to configure EMQX as the MQTT broker for the PI Connector, enabling field devices to publish sensor data over MQTT and have it land in PI System without any additional middleware.

## Choose an Integration Approach

EMQX supports two approaches for integrating with AVEVA PI System:

- **PI Connector for MQTT (this page)**: The PI Connector for MQTT connects to EMQX as an MQTT client and pulls data by subscribing to topics. This approach requires AVEVA's PI Connector software but needs minimal EMQX configuration beyond standard broker setup.
- **[HTTP integration](../../data-integration/aveva-pi-system.md)**: EMQX actively pushes data to PI System by POSTing OMF messages to the PI Web API endpoint. This approach works with any PI Server that has PI Web API installed, gives you full control over payload transformation through the Rule Engine, and does not require the PI Connector for MQTT to be installed.

Choose the PI Connector approach when your devices already publish OMF-formatted payloads and you want PI System to manage the data ingestion directly. Choose the HTTP integration when you need fine-grained payload transformation, when PI Web API is already part of your infrastructure, or when you want to avoid installing additional AVEVA connector software.

## How It Works

The PI Connector for MQTT acts as an MQTT client. It connects to EMQX, subscribes to one or more topics, receives messages from field devices, and forwards the data to PI Server using OSIsoft Message Format (OMF).

```
Field Device → MQTT Publish → EMQX → PI Connector for MQTT → PI Server
```

EMQX handles all MQTT broker responsibilities: connection management, authentication, access control, and message routing. The PI Connector handles the translation from MQTT messages to PI Data Archive writes. EMQX's Rule Engine is available to filter or transform messages before they reach the PI Connector's subscribed topics, if needed.

## Prerequisites

- EMQX Enterprise v6.0 or later
- AVEVA PI Server 2018 or later
- PI Connector Relay with the PI Connector for MQTT installed (available from AVEVA's software distribution)
- Network connectivity between the PI Connector host and the EMQX broker

## Configure EMQX for the PI Connector

The PI Connector connects to EMQX like any other MQTT client. You need to configure a listener, authentication credentials, and access control for the connector's client identity.

### Step 1: Verify the MQTT Listener

EMQX listens on TCP port `1883` by default. For production deployments, configure a TLS listener on port `8883` instead. To verify or adjust listener settings, go to **EMQX Dashboard** -> **Management** -> **Listeners**.

For TLS, ensure that a server certificate and key are configured, and that the PI Connector host trusts the CA certificate used to sign the server certificate.

### Step 2: Create Authentication Credentials

The PI Connector authenticates with EMQX using a username and password. Create a dedicated credential for it:

1. In the EMQX Dashboard, click **Access Control** -> **Authentication**.
2. Select your authentication backend (for example, Built-in Database).
3. Add a new user entry, for example:
   - **Username**: `pi-connector`
   - **Password**: a strong, randomly generated password

Note the username and password, as you will need them when configuring the PI Connector.

### Step 3: Configure Topic Access Control

Restrict the PI Connector to only the topics it needs to subscribe to:

1. In the EMQX Dashboard, click **Access Control** -> **Authorization**.
2. Add a rule that allows the `pi-connector` user to **subscribe** to the relevant topic patterns, for example `sensors/#`.
3. Deny all other operations for this user if your authorization backend supports default-deny.

## Configure the PI Connector for MQTT

The PI Connector for MQTT is configured via the PI Connector Relay configuration files or its administration interface, depending on the version. The key parameters to set are:

| Parameter | Value |
|---|---|
| **Broker Address** | Hostname or IP address of your EMQX node |
| **Port** | `1883` (TCP) or `8883` (TLS) |
| **Client ID** | A unique string, for example `pi-connector-01` |
| **Username** | `pi-connector` (as created in EMQX) |
| **Password** | The password set in EMQX |
| **Topic Subscriptions** | The MQTT topics to subscribe to, for example `sensors/#` |
| **QoS** | `1` (at least once) is recommended for reliable delivery |

For TLS connections, also configure:

| Parameter | Value |
|---|---|
| **CA Certificate** | The CA certificate that signed the EMQX server certificate |
| **TLS Version** | TLS 1.2 or 1.3 |

Refer to the AVEVA PI Connector for MQTT documentation for the exact configuration file path and field names for your installed version.

## Message Format Requirements

<!-- TODO: Verify the exact mechanism the PI Connector for MQTT uses to distinguish Type/Container/Data message types (topic suffix vs. message header field) for the specific version being documented. This varies by connector version. Check the AVEVA PI Connector for MQTT release notes or admin guide for the installed version. -->
The PI Connector for MQTT expects messages in OMF (OSIsoft Message Format). Devices must publish OMF-compliant JSON payloads to the subscribed topics. OMF messages carry one of three message types, identified by an MQTT topic suffix or a header field depending on the connector version:

- **Type messages**: Define the data schema (sent once during setup)
- **Container messages**: Create named PI tags (sent once during setup)
- **Data messages**: Carry time-series values (sent continuously)

A minimal OMF data message looks like this:

```json
[{
  "containerid": "sensor-001-temperature",
  "values": [{
    "Timestamp": "2024-01-15T10:30:00Z",
    "Value": 23.5
  }]
}]
```

If your devices publish non-OMF payloads (for example, plain JSON with arbitrary fields), use the EMQX Rule Engine to reformat messages on a separate topic before the PI Connector subscribes to them. See [Data Integration Rules](../data-integration/rules.md) for details on Rule Engine configuration.

## Verify the Connection

After configuring both sides:

1. Start the PI Connector for MQTT on the connector host.
2. In the EMQX Dashboard, click **Monitoring** -> **Clients** and confirm that a client with the ID `pi-connector-01` (or whichever Client ID you set) appears as connected.
3. Have a device publish a test message to a subscribed topic, for example:

   ```bash
   mqttx pub -t "sensors/building-a/room-1" \
     -m '[{"containerid":"sensor-001-temperature","values":[{"Timestamp":"2024-01-15T10:30:00Z","Value":23.5}]}]'
   ```

4. Use PI System Explorer, PI Vision, or PI DataLink to confirm that the value has been written to the target PI tag.

## Use the Rule Engine for Payload Transformation

If devices cannot publish OMF-formatted payloads, the EMQX Rule Engine can reformat arbitrary JSON before the PI Connector receives it. The recommended pattern is:

1. Devices publish raw payloads to an input topic, for example `factory/line-1/+`.
2. A Rule matches that topic, extracts fields, and republishes a reformatted OMF payload to an output topic, for example `omf/factory/#`.
3. The PI Connector subscribes to `omf/factory/#`.

### Example

A temperature and humidity sensor publishes the following raw JSON payload to `factory/line-1/sensor-001`:

```json
{
  "device": "sensor-001",
  "temp_c": 72.4,
  "humidity": 58.2,
  "ts": "2024-01-15T10:30:00Z"
}
```

The PI Connector cannot consume this directly. [Create a Rule](../data-integration/rule-get-started.md#define-rule-sql) in the EMQX Dashboard with the following SQL, which extracts the device ID, sensor values, and timestamp:

```sql
SELECT
  concat('pi-', payload.device, '-temp') AS container_id_temp,
  concat('pi-', payload.device, '-humidity') AS container_id_hum,
  payload.temp_c AS temp_value,
  payload.humidity AS hum_value,
  payload.ts AS timestamp
FROM
  "factory/line-1/+"
```

Attach a [**Republish**](../data-integration/rule-get-started.md#add-republish-action) action to this Rule with the topic set to `omf/factory/line-1/${payload.device}` and the following payload template:

```json
[
  {
    "containerid": "${container_id_temp}",
    "values": [{ "Timestamp": "${timestamp}", "Value": ${temp_value} }]
  },
  {
    "containerid": "${container_id_hum}",
    "values": [{ "Timestamp": "${timestamp}", "Value": ${hum_value} }]
  }
]
```

EMQX republishes this OMF-formatted message to `omf/factory/line-1/sensor-001`. The PI Connector, subscribed to `omf/factory/#`, receives it and writes two values to PI Data Archive:

- `72.4` to PI tag `pi-sensor-001-temp`
- `58.2` to PI tag `pi-sensor-001-humidity`

Both values are timestamped `2024-01-15T10:30:00Z`.

### When to Use Message Transformation

The Rule Engine SQL above works when devices publish plain JSON. If devices publish binary-encoded payloads such as Protobuf or Avro, add an EMQX [Message Transformation](../data-integration/message-transformation.md) before the Rule Engine processes the message. Message Transformation decodes the binary payload into JSON first, after which the Rule Engine SQL can extract fields as normal. For plain JSON payloads, Message Transformation is not needed.

## Security Recommendations

- Use TLS (port 8883) for the connection between the PI Connector and EMQX in production.
- Use a dedicated EMQX client credential for the PI Connector with the minimum necessary topic access.
- Rotate the PI Connector's EMQX password periodically.
- Enable mutual TLS (mTLS) if your security policy requires client certificate authentication.
