# Ingest MQTT Data into Bigtable

[Cloud Bigtable](https://cloud.google.com/bigtable) is a fully managed, wide-column NoSQL database service on Google Cloud. It is designed for large-scale, low-latency workloads, such as time series data, telemetry storage, event records, and high-throughput IoT data ingestion.

EMQX supports integration with Bigtable through the rule engine and a Bigtable Sink. You can process MQTT messages with rule SQL, map the selected fields to Bigtable row keys and cell mutations, and append the processed data to a Bigtable table in real time.

This page introduces how the Bigtable data integration works and provides a draft workflow for creating and testing the integration in the EMQX Dashboard.

## How It Works

Bigtable data integration is an out-of-the-box feature in EMQX 6.3. It helps users stream MQTT data into Google Cloud and store device telemetry or event data in Bigtable for later query, analysis, or downstream processing.

EMQX forwards MQTT data to Bigtable through the rule engine and Sink. The complete process is as follows:

1. **IoT Devices Publish Messages**: Devices publish telemetry, status, or event data to MQTT topics.
2. **Rule Engine Processes Messages**: The rule engine matches MQTT messages by topic and uses SQL to extract or transform the fields that Bigtable requires.
3. **Writing to Bigtable**: The Bigtable Sink writes the rule output to a Bigtable table. Each rule output record is converted into a Bigtable row mutation by using the configured row key and `set_cell` mutation fields.

## Features and Benefits

Integrating EMQX with Bigtable provides the following benefits:

- **High-Throughput IoT Data Ingestion**: Write MQTT messages to Bigtable for large-scale telemetry and event workloads.
- **Flexible Field Mapping**: Use rule SQL to explicitly select and alias the fields used as the Bigtable row key, column family, column qualifier, timestamp, and cell value.
- **Batch and Asynchronous Writes**: Use batch mode and asynchronous request mode to improve write throughput and reduce the impact on MQTT message publishing.
- **Google Cloud Integration**: Store MQTT data in Bigtable and use it with other Google Cloud services for analytics, processing, or application development.

## Before You Start

This section describes the preparations you need to complete before creating the Bigtable data integration.

### Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [Data Integration](./data-bridges.md)
- A Google Cloud project with Bigtable enabled
- A Bigtable instance, table, and at least one column family

### Create Service Account Key in GCP

To allow EMQX to connect to Bigtable, create a Google Cloud service account and generate a key in JSON format.

1. Create a [Service Account](https://developers.google.com/identity/protocols/oauth2/service-account#creatinganaccount) in your GCP account.
2. Grant the service account permissions to write to the Bigtable instance and table. For example, assign a Bigtable role that allows data read/write operations on the target table.
3. Click the email address of the service account you created.
4. Click the **Keys** tab. In the **Add key** dropdown list, select **Create new key** and download the key in JSON format.

   ::: tip

   Store the service account key securely. You will use it later when creating the Bigtable connector.

   :::

### Set Up Workload Identity Federation in GCP

Workload Identity Federation (WIF) allows EMQX to access GCP resources without a long-lived service account key file. EMQX exchanges a token from an external identity provider, such as Microsoft Azure, for a temporary GCP token through GCP Security Token Service, then uses it to impersonate a GCP service account. Token renewal is handled automatically.

To use WIF, complete the following in your GCP project before creating the connector:

1. In the Google Cloud console, go to **IAM & Admin** -> **Workload Identity Federation**, create a workload identity pool, and note the **Pool ID** and **Project Number**.
2. Add a provider to the pool and note the **Provider ID**. For OIDC-based authentication, obtain the OAuth 2.0 client credentials from your external identity provider.
3. Grant the workload identity pool permission to impersonate the GCP service account with access to the Bigtable instance and table.

::: tip

See [Configure Workload Identity Federation](https://cloud.google.com/iam/docs/workload-identity-federation-with-other-providers) for detailed instructions.

:::

### Create and Manage Bigtable Resources in GCP

Before configuring the Bigtable data integration in EMQX, create the target Bigtable resources in Google Cloud.

1. In the Google Cloud console, go to the **Bigtable** page.
2. Create or select a Bigtable instance. Note the instance ID, for example, `emqxinst`.
3. Create a table. Note the table ID, for example, `mqtt_messages`.
4. Create at least one column family in the table, for example, `cf`.

   ::: tip

   The **Instance ID** and **Table ID** used in EMQX are simple identifiers, such as `emqxinst` and `mqtt_messages`. They are not fully qualified resource names such as `projects/<project-id>/instances/<instance-id>`.

   :::

## Create a Bigtable Connector

Before adding a Bigtable Sink action, create a Bigtable connector to establish the connection between EMQX and Bigtable.

1. Go to the EMQX Dashboard and click **Integration** -> **Connector**.
2. Click **Create** in the top right corner of the page, select **Bigtable**, and click **Next**.
3. Enter a connector name and description, such as `my_bigtable`. The name is used to associate the Bigtable Sink with the connector and must be unique within the cluster.
4. Configure the connection and authentication options:
   - **Endpoint**: The Bigtable endpoint. The default endpoint is `https://bigtable.googleapis.com:443`.
   - **Connect Timeout**: Timeout for establishing the connection.
   - **Pool Size**: Size of the connection pool to Bigtable.
   - **Authentication**: Select one of the supported authentication methods:
     - **Service Account JSON**: Upload the service account key JSON file.
     - **Workload Identity Federation (WIF)**: Configure the GCP project, workload identity pool, provider, service account email, and initial OIDC client credentials.
     - **Attached Service Account**: Use the service account attached to the runtime environment, such as a Google Compute Engine or GKE environment where the metadata service is available.
5. Before clicking **Create**, you can click **Test Connectivity** to verify that EMQX can connect to Bigtable.
6. Click the **Create** button to complete the connector setup. A **Created Successfully** dialog appears asking whether to create a rule now. Click **Create Rule** to proceed directly to rule creation with the connector pre-selected, or click **Back To Connector List** to return and create a rule later.

## Configuration Example

The following example shows the main Bigtable connector and action configuration items. You can use it as a reference when checking the Dashboard fields or preparing configuration through API/config files.

```hocon
connectors.bigtable.my_bigtable {
  enable = true
  connect_timeout = "5s"
  pool_size = 8
  authentication {
    type = service_account_json
    service_account_json = "{...}"
  }
}

actions.bigtable.my_bigtable_sink {
  enable = true
  connector = my_bigtable
  parameters {
    instance_id = "emqxinst"
    table_id = "mqtt_messages"
    row_key = "rk"
    mutations = [
      {
        type = set_cell
        family_name = "fn"
        column_qualifier = "cq"
        timestamp_micros = "tm"
        value = "v"
      }
    ]
  }
  resource_opts {
    batch_size = 1000
    batch_time = "500ms"
    query_mode = async
    request_ttl = "45s"
    worker_pool_size = 16
    inflight_window = 100
  }
}
```

## Create a Rule with Bigtable Sink

This section demonstrates how to create a rule that writes MQTT messages to Bigtable.

1. If you clicked **Create Rule** in the previous step, the **Add Action** panel opens automatically with **Type of Action** set to `Bigtable` and the connector pre-selected. Skip to step 5 to configure the action first; after the action is created, return to the rule page and complete the rule ID and SQL settings. Otherwise, go to the Dashboard **Integration** -> **Rules** page and click **Create** in the top right corner.
2. Enter `my_rule` as the rule ID.
3. In the **SQL Editor**, enter the rule SQL. The Bigtable Sink uses field names configured in the Sink to look up values from the rule output. Therefore, the SQL must explicitly select and alias all fields required by the Bigtable mutation.

   Example:

   ```sql
   SELECT
     clientid AS rk,
     'cf' AS fn,
     '' AS cq,
     payload AS v,
     publish_received_at * 1000 AS tm
   FROM
     "t/bigtable"
   ```

   In this example:

   - `rk` is used as the Bigtable row key.
   - `fn` is used as the column family name.
   - `cq` is used as the column qualifier.
   - `tm` is used as the timestamp in microseconds.
   - `v` is used as the cell value.

   ::: tip

   The Bigtable Sink fields are key names that refer to rule output fields. They are not template expressions. If a required key is not selected by the rule SQL, the Sink cannot build the Bigtable mutation for that message.

   :::

4. Click **Add Action**. Select `Bigtable` from the **Type of Action** dropdown list.
5. Keep **Action** as `Create Action`, or select an existing Bigtable Sink. If you entered rule creation from the connector success dialog, confirm that the connector is already pre-selected.
6. Enter a Sink name.
7. Select the Bigtable connector created in [Create a Bigtable Connector](#create-a-bigtable-connector) if it is not already selected.
8. Configure the Bigtable action parameters:

   | Field | Description | Example |
   | --- | --- | --- |
   | **Instance ID** | Bigtable instance ID. Use the simple instance ID, not the fully qualified resource name. | `emqxinst` |
   | **Table ID** | Bigtable table ID. Use the simple table ID. | `mqtt_messages` |
   | **Row Key** | Rule output field name that contains the row key. | `rk` |
   | **Mutations** | List of cell mutations to apply for each message. The current integration supports `set_cell` mutations. | - |
   | **Family Name** | Rule output field name that contains the column family name. | `fn` |
   | **Column Qualifier** | Rule output field name that contains the column qualifier. | `cq` |
   | **Timestamp in Microseconds** | Rule output field name that contains the cell timestamp in microseconds. | `tm` |
   | **Value** | Rule output field name that contains the cell value. | `v` |

9. Configure **Fallback Actions** if you want to improve reliability when message delivery fails. See [Fallback Actions](./data-bridges.md#fallback-actions).
10. Configure **Advanced Settings** as needed. See [Advanced Settings](#advanced-settings).
11. Click **Create** to complete the Sink configuration.
12. Back on the **Create Rule** page, click **Create** to create the rule.

## Test the Rule

1. Use MQTTX to publish a message to the topic `t/bigtable`:

   ```bash
   mqttx pub -i emqx_c -t t/bigtable -m '{ "msg": "hello Bigtable" }'
   ```

2. Check the rule and Sink metrics. The matched and successful counts should increase.
3. In Google Cloud, query the target Bigtable table and verify that a row was written with:
   - Row key: the MQTT client ID, for example, `emqx_c`
   - Column family: `cf`
   - Column qualifier: empty string
   - Cell value: the MQTT payload

## Advanced Settings

This section describes common advanced settings for the Bigtable Sink.

| Field | Description | Default Value |
| --- | --- | --- |
| **Buffer Pool Size** | Number of buffer worker processes used to process and send data to Bigtable. | `16` |
| **Request TTL** | Maximum time a request can stay valid after entering the buffer. If the request expires before it is sent or acknowledged, it is considered expired. | `45s` |
| **Health Check Interval** | Interval for checking the health of the Bigtable connection. | `15s` |
| **Health Check Timeout** | Timeout for connector health checks. | `60s` |
| **Max Buffer Queue Size** | Maximum buffer queue size for each buffer worker. | `256MB` |
| **Query Mode** | Request mode. In asynchronous mode, writing to Bigtable does not block MQTT message publishing. | `Async` |
| **Batch Size** | Maximum number of records to write in one batch. Set it to `1` to disable batching. | `1000` |
| **Batch Time** | Maximum waiting time before a non-empty batch is sent. | `500ms` |
| **Inflight Window** | Maximum number of in-flight requests in asynchronous mode. Set it to `1` if strict ordering is required for messages from the same MQTT client. | `100` |

For high-throughput deployments, tune **Pool Size**, **Buffer Pool Size**, **Batch Size**, **Batch Time**, and **Inflight Window** together based on your expected cluster workload. For example, if the target workload is around 11,000,000 messages per 2 minutes across the cluster with 5,000 to 10,000 MQTT connections, validate the configuration with a representative benchmark before production use.
