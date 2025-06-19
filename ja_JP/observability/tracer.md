# Log Trace

EMQX 5.0 introduces the Log Trace feature, enabling real-time debug-level log outputs for specific client IDs, topics, IP addresses, or rule IDs. This allows detailed debugging in production environments without affecting system performance due to excessive logs, increasing the efficiency of diagnosing and resolving EMQX issues.

## How Log Trace Works 

The Log Trace feature is implemented using the built-in Erlang Logger Filter function, which has a negligible impact on the overall message throughput. EMQX uses independent File Handlers to persist Trace disk logs and creates a separate process for each client connection to process its messages.

When a client sends a message, the independent process responsible for that connection will first check if the message complies with the rules set by the customized Trace Filter. For example, the process may check if the message is from a specified client ID:

- If the message is from the specified client ID, the process will convert the message into binary data and then asynchronously send it to the appropriate File Handler. 
- If it is not from the specified client ID, EMQX will execute the original transfer logic. 

The File Handlers are responsible for persisting the binary data into Trace files on disk. 

## Why Use Log Trace

The Log Trace feature offers several key benefits that make it an effective tool for debugging and monitoring in production environments.

- **Safety**: The filtering process is performed independently for each client, which prevents the File Handler from being overloaded with incoming messages. Since most of the logs are filtered out, this approach is safe for production environments. 
- **Reliability**: This feature ensures that trace logging does not impact the overall message throughput of EMQX and provides a reliable and efficient way to store and retrieve log data.
- **Agility**: Log Trace can be used for various scenarios, such as debugging abnormal messages or data losses, client disconnection, subscription failure, and etc. For system malfunctions that occur at a specific time, you can set the task start/stop time for automatic log collection, which is very convenient.

## Create Log Trace

This section demonstrates how to create Log Trace rules in the Dashboard. You can trace interactions based on Client ID, Topic, IP address, or Rule ID.

1. Click **Diagnose** -> **Log Trace** on the left navigation menu. 
2. On the **Log Trace** page, click **Create** to configure your trace rules. 

### Configure Common Trace Options

On the **Create Trace** dialog, configure the following options that apply to all trace types:

- **Name**: Enter a descriptive name for the trace to identify it in the logs. This name will appear in the trace list and should provide useful context, such as the type of trace (e.g., "Client ID Trace" or "Topic Trace") for quicker search and identification. 
- **Start Time / End Time**: Select the start and end times for the trace. If the start time is earlier than or the same as the current time, the trace will begin from the current time.
- **Formatter**: Select the formatter to specify how the log output should be formatted. Options include `JSON` and `Text`.
- **Payload Encode**: Specify the format in which the payload will be encoded in the trace log file. Choose one of the following options:
  - `Text`: A text-based or plain text protocol. Recommended for JSON-encoded payloads.
  - `HEX`: Binary hexadecimal encoding. Recommended for custom binary protocols.
  - `Hidden`: Obfuscates the payload as `******` (useful for masking sensitive information).
- **Payload Limit**: Set the maximum number of bytes that will be printed for the payload in the trace file. This option is only effective when **Payload Encode** is set to either `Text` or `HEX`. If the payload exceeds this limit, it will be truncated. The default value is `1024 B`. When the Payload Limit is disabled, the trace will not impose a limit on the payload size. It is enabled by default.

### Trace by Client ID

1. On the Create Trace dialog, select `Client ID` from the **Type** drop-down list.
2. Type the Client IDs to be traced.
3. Configure the common options. See [Configure Common Trace Options](#configure-common-trace-options).
4. Click **Create** to complete.

The log trace will contain interactions for the specified Client ID with the EMQX connection.

### Trace by Topic

1. On the Create Trace dialog, select `Topic` from the **Type** drop-down list.
2. Type the topic to be traced. Wildcard characters are supported, for example, `/pay/#`.
3. Configure the common options. See [Configure Common Trace Options](#configure-common-trace-options).
4. Click **Create** to complete.

The log trace will contain information about publishing, subscription and unsubscription of the specified topic.

### Trace by IP Address

1. On the Create Trace dialog, select `IP Address` from the **Type** drop-down list.
2. Type the IP address to be traced, for example, `192.168.0.5`.
3. Configure the common options. See [Configure Common Trace Options](#configure-common-trace-options).
4. Click **Create** to complete.

The log trace will contain interactions for the specified IP address with the EMQX connection.

### Trace by Rule ID

1. On the Create Trace dialog, select `Rule ID` from the **Type** drop-down list.
2. Enter the rule ID you need to trace. You can find the rule ID on the **Integration** -> **Rules** page.
3. Configure the common options. See [Configure Common Trace Options](#configure-common-trace-options).
4. Click **Create** to complete.

The trace results will include the execution results of the rule SQL and the execution logs for all actions added to the rule, useful for debugging and optimizing the rule.

The [Test Rules](../data-integration/rule-get-started.md#test-rules) operation can automatically create and manage this trace type. When testing a rule, EMQX will automatically generate a trace task and delete it automatically after the test stops.

## View Log Trace 

The created trace records will be listed. You can create up to 30 traced logs. The log file size viewed in the list is the sum of the uncompressed file sizes. You can click the **Stop** button to stop logging manually or wait until the specified end time.

Click a specific trace record by the name, you can select to download the log on different nodes.

<img src="./assets/log-trace-node-ee.png" alt="log-trace-node-ee" style="zoom:50%;" />

Trace logs have a maximum capacity of 512MB logs per node. Once the generated log file reaches the maximum limit, it stops appending any further logs and raises an alert in the primary log file. In the event of a timeout during Dashboard downloading, you can locate the log file in the `/data/trace` directory on the server. When an EMQX cluster is restarted, the unfinished log trace will be resumed.





