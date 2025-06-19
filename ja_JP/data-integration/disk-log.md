# Ingest MQTT Data into Disk Log
The Disk Log data integration allows EMQX to persist event data to disk in [JSON Lines](https://jsonlines.org/) format, similar to traditional rotating log files. This enables long-term event retention for troubleshooting or historical tracking.

This page provides a detailed introduction to the data integration between EMQX with Disk Log and offers practical guidance on the rule and Sink creation.

## How It Works

While EMQX includes built-in system logging for monitoring operational events (such as errors, warnings, and system activities), the Disk Log integration serves a different purpose: it enables EMQX to persist actual MQTT message data and client-level events to disk for retention and offline processing.

Implemented using EMQX’s rule engine and Sink mechanism, the Disk Log integration enables users to define exactly what data is captured and how it should be stored:

1. Rules are used to filter, transform, and extract the data of interest from MQTT messages or client events.
2. A Disk Log Sink is attached to the rule to define how and where to store the data. The Sink forwards the formatted data (as JSON) to the corresponding Connector.
3. The Disk Log Connector manages the physical writing of data to the file system. It handles the log file path configuration, log file rotation policy, etc.
4. Once the rule is triggered and the data is passed to the Sink, the Sink invokes the configured Connector to write the data in JSON Lines format to a specified local directory, making it easy to consume using standard tools and downstream data systems.

### Log Rotation

Disk Log integration writes messages to a specified local directory on the local file system. To manage storage usage, each log file is rotated based on file size and file count thresholds:

- EMQX opens a new file and continues writing when the configured maximum file size is reached.
- When the configured maximum number of files is reached, the oldest file is truncated and opened for writing new entries.
- Each log file is guaranteed to contain at least one complete entry, even if that entry exceeds the specified file size limit.

## Features and Benefits

Disk Log integration provides a flexible, lightweight, and local-first solution for MQTT message persistence. Below are the key features and advantages:

- **Fine-Grained Data Control**: Log only the messages or events you care about using SQL-based rules. Apply transformation, filtering, and enrichment to message data before logging.
- **Structured Output Format**: Stores data in JSON Lines for easy machine processing.
- **Lightweight and Self-Contained**: No need to connect to external storage systems or databases.
- **Observability and Debugging**: Enables message-level visibility for troubleshooting or audits. Complements EMQX system logs by recording data flow instead of system events.

## Before You Start

This section introduces the preparations required before creating a Disk Log Sink in EMQX.

### Prerequisites

- Understanding of [rules](./rules.md)
- Understanding of [data integration](./data-bridges.md)

### Create a Log Directory

Create a writable directory on the EMQX host for storing log files. The EMQX system user must have read/write permissions for this directory.

## Create a Connector

Before adding the Disk Log Sink, you need to create the corresponding connector.

1. Go to the Dashboard **Integration** -> **Connector** page.
2. Click the **Create** button in the top right corner.
3. Select **Disk Log** as the connector type and click **Next**.
4. Enter the connector name, a combination of upper and lowercase letters and numbers. Here, enter `my-disk-log`.
5. Enter the connector parameters.
   - **Log Filepath**: Path to the directory where logs will be stored.
   - **Maximum File Size**: Maximum file size for each file before rotation. Note: At least one entry is written to each log, so the final file size may exceed this maximum if a single log entry exceeds this value.
   - **Maximum Number of Files**: Maximum number of files to retain before rotating over older logs.
6. Before clicking **Create**, you can click **Test Connectivity** to test if the connector can write logs to the configured path.
7. Click the **Create** button at the bottom to complete the connector creation.

You have now completed the connector creation and will proceed to create a rule and Sink for specifying the data to be written into the Disk Log.

## Create a Rule with Disk Log Sink

This section demonstrates how to create a rule in EMQX to process messages from the source MQTT topic `t/#` and write the processed results to local log files through the configured Sink.

1. Go to the Dashboard **Integration** -> **Rules** page.

2. Click the **Create** button in the top right corner.

3. Enter the rule ID `my_rule`, and input the following rule SQL in the SQL editor:

   ```sql
   SELECT
     *
   FROM
       "t/#"
   ```

   ::: tip

   If you are new to SQL, you can click **SQL Examples** and **Enable Debug** to learn and test the rule SQL results.

   :::

4. Add an action, select `Disk Log` from the **Action Type** dropdown list, keep the action dropdown as the default `create action` option, or choose a previously created Disk Log action from the action dropdown. Here, create a new Sink and add it to the rule.

5. Enter the Sink's name and description.

6. Select the `my-disk-log` connector created earlier from the connector dropdown. You can also click the create button next to the dropdown to quickly create a new connector in the pop-up box. The required configuration parameters can be found in [Create a Connector](#create-a-connector).

7. Select the desired **Write Mode** (async or sync).

8. Configure the **Message Template**, which must render to a valid JSON object.

9. **Fallback Actions (Optional)**: If you want to improve reliability in case of message delivery failure, you can define one or more fallback actions. These actions will be triggered if the primary Sink fails to process a message. See [Fallback Actions](./data-bridges.md#fallback-actions) for more details.

10. Expand **Advanced Settings** and configure the advanced setting options as needed (optional). For more details, refer to [Advanced Settings](#advanced-settings).

11. Use the default values for the remaining settings. Click the **Create** button to complete the Sink creation. After successful creation, the page will return to the rule creation, and the new Sink will be added to the rule actions.

12. Back on the rule creation page, click the **Create** button to complete the entire rule creation process.

You have now successfully created the rule. You can see the newly created rule on the **Rules** page and the new Disk Log Sink on the **Actions (Sink)** tab.

You can also click **Integration** -> **Flow Designer** to view the topology. The topology visually shows how messages under the topic `t/#` are written to the Disk Log after being parsed by the rule `my_rule`.

## Test the Rule

This section shows how to test the rule configured with the direct upload method.

Use MQTTX to publish a message to the topic `t/1`:

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Disk Log" }'
```

After sending a few messages, check the configured Disk Log directory for the last changed file and check its contents to see the produced event.

## Advanced Settings

This section delves into the advanced configuration options available for the Disk Log Sink. In the Dashboard, when configuring the Sink, you can expand **Advanced Settings** to adjust the following parameters based on your specific needs.

| Field Name                | Description                                                  | Default Value  |
| ------------------------- | ------------------------------------------------------------ | -------------- |
| **Buffer Pool Size**      | Specifies the number of buffer worker processes, which are allocated to manage the data flow between EMQX and Disk Log. These workers temporarily store and process data before sending it to the disk log, crucial for optimizing performance and throughput. | `16`           |
| **Request TTL**           | The "Request TTL" (Time To Live) configuration setting specifies the maximum duration, in seconds, that a request is considered valid once it enters the buffer. This timer starts ticking from the moment the request is buffered. If the request stays in the buffer for a period exceeding this TTL setting or if it is sent but does not receive a timely response or acknowledgment for being persisted by Disk Log, the request is deemed to have expired. |                |
| **Health Check Interval** | Specifies the time interval (in seconds) for the Sink to perform automatic health checks on Disk log. | `15`           |
| **Max Buffer Queue Size** | Specifies the maximum number of bytes that can be buffered by each buffer worker process in the Disk Log Sink. The buffer workers temporarily store data before sending it to Disk Log, acting as intermediaries to handle the data stream more efficiently. Adjust this value based on system performance and data transmission requirements. | `256`          |
| **Query Mode**            | Allows you to choose between `synchronous` or `asynchronous` request modes to optimize message transmission according to different requirements. In asynchronous mode, writing to Disk Log does not block the MQTT message publishing process. However, this may lead to clients receiving messages before they are written to Disk Log. | `Asynchronous` |
| **Batch Size**            | Specifies the maximum size of data batches written from EMQX to Disk Log in a single batch operation. By adjusting the size, you can fine-tune the efficiency and performance of data transfer between EMQX and Disk Log.<br />If the "Batch Size" is set to "1," data records are sent individually, without being grouped into batches.  This Action tends to benefit from generous batching sizes. | `1000`                   |
| **Inflight  Window**     | "In-flight queue requests" refer to requests that have been initiated but have not yet received a response or acknowledgment. This setting controls the maximum number of in-flight queue requests that can exist simultaneously during Sink communication with Disk Log. <br/>When **Request Mode** is set to `asynchronous`, the "Request In-flight Queue Window" parameter becomes particularly important. If strict sequential processing of messages from the same MQTT client is crucial, then this value should be set to `1`. | `100`          |
