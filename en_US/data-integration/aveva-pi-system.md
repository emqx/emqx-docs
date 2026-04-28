# Ingest MQTT Data into AVEVA PI System

AVEVA PI System is a widely used industrial data infrastructure platform that collects, stores, and visualizes time-series data from industrial assets. EMQX can forward MQTT messages from field devices directly into PI System using the HTTP data integration and the PI Web API OMF (OSIsoft Message Format) endpoint, enabling real-time operational data pipelines without additional middleware.

This page provides an overview of the integration architecture and step-by-step instructions for configuring EMQX to send data to AVEVA PI System via PI Web API.

## Choose an Integration Approach

EMQX supports two approaches for integrating with AVEVA PI System:

- **HTTP integration (this page)**: EMQX actively pushes data to PI System by POSTing OMF messages to the PI Web API endpoint. This approach works with any PI Server that has PI Web API installed, gives you full control over payload transformation through the Rule Engine, and does not require the PI Connector for MQTT to be installed.
- **[PI Connector for MQTT](../../connect-emqx/aveva-pi-system.md)**: The PI Connector for MQTT connects to EMQX as an MQTT client and pulls data by subscribing to topics. This approach requires AVEVA's PI Connector software but needs minimal EMQX configuration beyond standard broker setup.

Choose the HTTP integration when you need fine-grained payload transformation, when PI Web API is already part of your infrastructure, or when you want to avoid installing additional AVEVA connector software. Choose the PI Connector approach when your devices already publish OMF-formatted payloads and you want PI System to manage the data ingestion directly.

## How It Works

EMQX receives MQTT messages from industrial devices and forwards the data to PI System through the EMQX Rule Engine and an HTTP Server Sink. The workflow is as follows:

1. **Devices publish telemetry**: Field devices or edge gateways publish sensor readings to EMQX over MQTT. Payloads are typically JSON-formatted and contain one or more measurement values.
2. **Rule Engine processes messages**: The Rule Engine matches incoming messages by topic and extracts the relevant fields using SQL. It can filter, transform, and enrich the data before forwarding.
3. **HTTP Server Sink sends OMF data**: The processed data is forwarded to the PI Web API OMF endpoint via an HTTPS POST request. PI Web API ingests the OMF message and writes the values into PI Data Archive.

```
Field Device → MQTT → EMQX → Rule Engine → HTTP Sink → PI Web API (OMF) → PI Data Archive
```

AVEVA's OSIsoft Message Format (OMF) is the standard data format for this path. An OMF message is a JSON payload that carries one of three message types:

- **Type**: defines the schema (the shape of a data container)
- **Container**: creates a named instance of a type, mapped to a PI tag
- **Data**: sends actual time-series values into an existing container

PI tag registration (Type and Container messages) is a one-time setup step. Ongoing EMQX data integration handles only Data messages.

## Prerequisites

- EMQX Enterprise v6.0 or later
- AVEVA PI Server 2018 or later with PI Web API 2019 or later installed and enabled
- PI Web API configured to accept OMF requests (the OMF endpoint must be enabled in PI Web API Admin)
- A PI Web API service account with write access to the target PI Data Archive
- An EMQX deployment with network access to the PI Web API host

## Register PI Tags via OMF

Before streaming data, you must register the Type and Container in PI Web API so that PI Data Archive knows the schema of the incoming data. This is a one-time step performed from any HTTP client such as `curl` or Postman.

The PI Web API OMF endpoint URL follows this pattern:

```
https://<pi-web-api-host>/piwebapi/omf
```

### Step 1: Create a Type

A Type defines the data schema. The following example defines a simple type with a single float value and a timestamp:

```bash
curl -k -X POST "https://<pi-web-api-host>/piwebapi/omf" \
  -H "Content-Type: application/json" \
  -H "X-Requested-With: XMLHttpRequest" \
  -H "omfversion: 1.1" \
  -H "action: create" \
  -H "messageformat: json" \
  -H "messagetype: type" \
  -u "<username>:<password>" \
  -d '[{
    "id": "emqx-sensor-type",
    "type": "object",
    "classification": "dynamic",
    "properties": {
      "Timestamp": { "type": "string", "format": "date-time", "isindex": true },
      "Value": { "type": "number", "format": "float64" }
    }
  }]'
```

### Step 2: Create a Container

A Container is a named instance of a Type. Each Container maps to one PI tag in PI Data Archive.

```bash
curl -k -X POST "https://<pi-web-api-host>/piwebapi/omf" \
  -H "Content-Type: application/json" \
  -H "X-Requested-With: XMLHttpRequest" \
  -H "omfversion: 1.1" \
  -H "action: create" \
  -H "messageformat: json" \
  -H "messagetype: container" \
  -u "<username>:<password>" \
  -d '[{
    "id": "sensor-001-temperature",
    "typeid": "emqx-sensor-type"
  }]'
```

Replace `sensor-001-temperature` with the PI tag name you want to create for each sensor. Repeat this step for each data stream.

## Create a Connector

This section describes how to create an HTTP Server Connector that connects the EMQX Sink to the PI Web API OMF endpoint.

1. In the EMQX Dashboard, click **Integration** -> **Connector**.
2. Click **Create** in the top-right corner of the page, select **HTTP Server**, and click **Next**.
3. Enter a name for the Connector, for example, `aveva_pi_connector`.
4. Set **URL** to `https://<pi-web-api-host>/piwebapi/omf`.
5. Under **Headers**, add an `Authorization` header with a Basic Auth value. Encode your PI Web API service account credentials as a Base64 string (`username:password`) and set the header value to `Basic <base64-encoded-credentials>`.
   <!-- TODO: Some PI deployments use Kerberos (Windows authentication) instead of Basic Auth. If this is common in the target audience, add a note here explaining that Kerberos-authenticated PI Web API endpoints are not directly supported by EMQX's HTTP connector and may require a reverse proxy or a middleware layer that handles Kerberos negotiation. -->
6. If your PI Web API uses a self-signed certificate, click **Enable TLS** and configure the CA certificate, or disable certificate verification for testing only.
7. Before clicking **Create**, you can click **Test Connectivity** to verify that EMQX can reach the PI Web API endpoint.
8. Click **Create** to complete the Connector creation. A **Created Successfully** dialog will appear, asking whether to create a rule now. Click **Create Rule** to go directly to the rule creation page with the Connector pre-selected, or click **Back to Connector List** to create the rule later.

## Create a Rule with HTTP Server Sink

This section describes how to create a rule that extracts data from MQTT messages and an HTTP Server Sink that forwards that data to PI Web API in OMF format.

1. If you clicked **Create Rule** in the previous step, the **Add Action** panel opens automatically with the action type set to `HTTP Server` and the Connector pre-selected, so skip to step 5. Otherwise, go to **Integration** -> **Rules** in the Dashboard and click **Create** in the top-right corner.
2. Enter a rule ID such as `aveva_pi_rule`.
3. In the **SQL Editor**, enter a statement that matches your device topic and extracts the fields you need. The following example reads messages from `sensors/#` and extracts the sensor ID, value, and timestamp:

   ```sql
   SELECT
     payload.sensor_id AS container_id,
     payload.value AS value,
     timestamp AS ts
   FROM
     "sensors/#"
   ```

4. Click **+ Add Action**, select `HTTP Server` from the **Type of Action** dropdown, and keep the **Action** dropdown set to the default **Create Action**.
5. Enter a name for the Sink, for example, `pi_omf_sink`.
6. Select the `aveva_pi_connector` you created from the **Connector** dropdown.
7. Set **Method** to `POST`.
8. Set **URL** to `https://<pi-web-api-host>/piwebapi/omf`.
9. Under **Headers**, add the following key-value pairs:

   | Key | Value |
   |---|---|
   | `Content-Type` | `application/json` |
   | `X-Requested-With` | `XMLHttpRequest` |
   | `omfversion` | `1.1` |
   | `action` | `create` |
   | `messageformat` | `json` |
   | `messagetype` | `data` |

10. In the **Body** field, enter the OMF data message template. Use `${field}` syntax to reference fields extracted by the Rule Engine:

    ```json
    [{
      "containerid": "${container_id}",
      "values": [{
        "Timestamp": "${ts}",
        "Value": ${value}
      }]
    }]
    ```

11. **Fallback Actions (Optional)**: To improve reliability when message delivery fails, configure one or more fallback actions for the Sink. See [Fallback Actions](./data-bridges.md#fallback-actions) for details.
12. For advanced settings such as buffer queue, request TTL, and query mode, expand **Advanced Settings**. The default values are suitable for most deployments. See [Features of Sink](./data-bridges.md#features-of-sink) for details.
13. Click **Create** to complete the Sink configuration. The new Sink appears under the **Action Outputs** tab on the rule creation page.
14. Click **Create** on the rule page to activate the rule.

## Test the Integration

Use [MQTTX](https://mqttx.app/) or any MQTT client to publish a test message:

```bash
mqttx pub -t "sensors/building-a/room-1" -m '{"sensor_id":"sensor-001-temperature","value":23.5}'
```

After publishing, verify the result in PI System:

- Use PI Vision, PI DataLink, or PI System Explorer to query the `sensor-001-temperature` tag.
- Confirm that a new time-series value of `23.5` appears with the correct timestamp.

You can also check the EMQX Dashboard rule statistics: click the rule name on the **Rule** page and confirm that the incoming and outgoing message counts have incremented.

## Advanced Configuration

### Map Multiple Sensors to Different PI Tags

If devices publish multiple sensor readings in a single message, use separate Rules or expand the Rule SQL to process each field. The `container_id` field in the OMF body template determines which PI tag receives the value, so different messages or separate Sinks with different body templates can write to different tags.

### Handle Timestamps

PI Data Archive expects timestamps in ISO 8601 format (`2024-01-15T10:30:00Z`). EMQX's `timestamp` field is a Unix epoch millisecond integer. If your device payload includes a pre-formatted ISO 8601 timestamp, reference that field directly. Otherwise, use the Rule Engine's `date_to_unix_ts` and related functions, or handle timestamp conversion on the device or edge side before publishing to EMQX.

### Security Recommendations

- Use HTTPS for all communication between EMQX and PI Web API.
- Use a dedicated PI Web API service account with the minimum required write permissions.
- Configure EMQX MQTT listeners with TLS (port 8883) and require client certificate authentication for field devices.
- Do not disable certificate verification in production.
