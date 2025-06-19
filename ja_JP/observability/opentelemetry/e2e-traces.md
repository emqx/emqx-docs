# OpenTelemetry-Based End-to-End MQTT Tracing

In modern distributed systems, tracking the flow of requests and analyzing performance is essential for ensuring reliability and observability. End-to-end tracing is a concept designed to capture the full path of a request from start to finish, enabling users to gain deep insights into system behavior and performance.

Starting from version 5.8.3, EMQX integrates an OpenTelemetry-based end-to-end tracing feature tailored for the MQTT protocol. This functionality allows users to clearly trace the publishing, routing, and delivery of messages, particularly in multi-node cluster environments. It not only aids in optimizing system performance but also helps in rapid fault localization and enhancing system reliability.

This page provides a detailed guide on how to enable the end-to-end tracing feature in EMQX to achieve a comprehensive visualization of MQTT message flows.

## Set Up OpenTelemetry Collector

Refer to [Setting Up OpenTelemetry Collector](./traces.md#setting-up-opentelemetry-collector) for configuration details.

## Enable End-to-End Tracing in EMQX

::: tip

Since end-to-end tracing can affect the system performance, only enable it when necessary.

:::

This section guides you through enabling OpenTelemetry-based end-to-end tracing in EMQX and demonstrates MQTT distributed tracing capabilities in a multi-node setup.

### Configure End-to-End Tracing via Dashboard

1. Click **Management** -> **Monitoring** from the Dashboard menu on the left.
2. Select the **Integration** tab on the Monitoring page.
3. Configure the following settings:
   - **Monitoring platform**: Select `OpenTelemetry`.
   - **Feature Selection**: Select `Traces`.
   - **Endpoint**: Set the trace data export address, which defaults to `http://localhost:4317`. `http://localhost:4317`.
   - **Enable TLS**: Enable TLS encryption for secure communication as needed, typically for security requirements in production environments.
   - **Trace Mode**: Select `End-to-End` to enable end-to-end tracing functionality.
   - **Cluster Identifier**: Add a property value to the span attributes to help identify which EMQX cluster the data comes from. The property key will be `cluster.id`. Typically, set a simple and easily identifiable name or use the cluster name to differentiate between EMQX clusters. The default is `emqxcl`.
   - **Traces Export Interval**: Set the time interval for exporting trace data, with a default of `5` seconds.
   - **Max Queue Size**: Set the maximum size of the trace data queue. The default is `2048` entries.

4. Click **Trace Advanced Configuration** to configure advanced settings if necessary.

   - **Trace Configuration**: Used to set additional trace options, including whether to trace specific events (such as client connections, message transmissions, rule-engine executions, etc.).
     - **Follow Traceparent**: Set whether to follow the `traceparent`. When set to `true`, EMQX will attempt to retrieve the `traceparent` identifier from the `User-Property` sent by the client and associate the end-to-end tracing with it. Otherwise, EMQX will generate a new trace for the end-to-end tracing. The default value is `true`.
   - **Client ID White List**: Set a whitelist to restrict which clients' connections or messages will be traced. This can help avoid unnecessary tracing and reduce additional system resource consumption.
   - **Topic White List**: Set a topic whitelist, allowing only matching topics to be traced. This works similarly to the client whitelist, helping to control the scope of the tracing.

   Click **Confirm** after you save the configuration and close the window.

5. Click **Save Changes** to save the configuration.

<img src="./assets/e2e-dashboard-conf-page-en.png" alt="Otel-E2E-Trace-dashboard-page" style="zoom:67%;" />

### Configure End-to-End Tracing via Configuration File

Add the following configuration to the EMQX `cluster.hocon` file (assuming EMQX is running locally).

For more details on configuration options, refer to the OpenTelemetry subsection of [EMQX Dashboard Monitoring Integration](http://localhost:18083/#/monitoring/integration).

```bash
opentelemetry {
  exporter { endpoint = "http://localhost:4317" }
  traces {
   enable = true
   # End-to-end tracing mode
   trace_mode = e2e
   # End-to-end tracing options
   e2e_tracing_options {
     ## Trace client connection/disconnection events
     client_connect_disconnect = true
     ## Trace client subscription/unsubscription events
     client_subscribe_unsubscribe = true
     ## Trace client messaging events
     client_messaging = true
     ## Trace Rule-Engine Executions
     trace_rule_engine = true
     ## Maximum whitelist length for client IDs
     clientid_match_rules_max = 30
     ## Maximum whitelist length for topic filters
     topic_match_rules_max = 30
     ## Cluster identifier
     cluster_identifier = emqxcl
     ## Message trace level (QoS)
     msg_trace_level = 2
     ## Sampling rate for events not in the whitelist
     ## Note: Sampling applies only when tracing is enabled
     sample_ratio = "100%"
     ## Follow traceparent
     ## Whether end-to-end tracing follows the `traceparent` passed in by the client
     follow_traceparent
    }
  }
  max_queue_size = 50000
  scheduled_delay = 1000
 }
}
```

## Demonstrate End-to-End Tracing in EMQX

1. Start EMQX nodes, for example, start a two-node cluster with node names `emqx@172.19.0.2` and `emqx@172.19.0.3` to demonstrate distributed tracing functionality.

2. Use MQTTX CLI as a client to subscribe to the same topic on different nodes.

   - Subscribe on the `emqx@172.19.0.2` node:

     ```bash
     mqttx sub -t t/1 -h 172.19.0.2 -p 1883
     ```

   - Subscribe on the `emqx@172.19.0.3` node:

     ```bash
     mqttx sub -t t/1 -h 172.19.0.3 -p 1883
     ```

3. After approximately 5 seconds (the default interval for exporting trace data in EMQX), navigate to the Jaeger WEB UI at [http://localhost:16686](http://localhost:16686/) to view trace data.

   Select the `emqx` service and click **Find Traces**. If the `emqx` service does not appear immediately, wait a moment and refresh the page. You should see traces for client connection and subscription events:

   ![Jaeger-WEB-UI-e2e-Client-Events](./assets/e2e-client-events.png)

4. Publish a message:

   ```bash
   mqttx pub -t t/1 -h 172.19.0.2 -p 1883
   ```

5. After a short delay, you can find detailed traces of the MQTT message in the Jaeger WEB UI.

   Click a trace to view detailed span information and the trace timeline. Depending on the number of subscribers, cross-node message routing, QoS levels, and the `msg_trace_level` configuration, an MQTT message trace may include varying numbers of spans.

   Below is an example trace timeline and span information when two clients have QoS 2 subscriptions, the publisher sends a QoS 2 message, and the `msg_trace_level` is set to 2.

   Notably, since the client `mqttx_9137a6bb` is connected to a different EMQX node than the publisher, two additional spans (`message.forward` and `message.handle_forward`) appear to represent cross-node transmission.

   ![Jaeger-WEB-UI-e2e-Message](./assets/e2e-message.png)

   In addition, for messages or events that trigger the execution of the rule engine, when the rule engine tracking option is enabled, the tracking information of rule and action execution can also be obtained.

   ![Jaeger-WEB-UI-e2e-With-Rule-Engine](./assets/e2e-with-rule-engine.png)

   ::: tip

   The end-to-end tracing with rule-engine execution feature is supported only in EMQX version 5.9.0 and later.

   :::

   ::: warning Important Notice

   Please enable this feature with caution. When a message or event triggers multiple rules and actions, a single trace may generate a large number of spans, increasing system load.
   Please estimate an appropriate sampling rate based on message volume and the number of rules and actions.

   :::

## Manage Trace Span Overload

EMQX accumulates trace spans and periodically exports them in batches. The export interval is controlled by the `opentelemetry.trace.scheduled_delay` parameter, which defaults to 5 seconds. The batch trace span processor includes overload protection, allowing accumulation of spans up to a limit, which defaults to 2048 spans. You can adjust this limit using the following configuration:

```yaml
opentelemetry {
  traces {
    max_queue_size = 50000
    scheduled_delay = 1000
  }
}
```

When the `max_queue_size` limit is reached, new trace spans are dropped until the current queue is exported.

::: tip Note

If the traced messages are distributed to a large number of subscribers, or if the message volume is high and the sampling rate is set too high, only a small portion of spans may be exported, with most spans discarded due to overload protection.

For end-to-end tracing mode, consider increasing the `max_queue_size` value based on message volume and sampling rate, and reducing the `scheduled_delay` configuration to increase span export frequency. This helps avoid loss of spans due to overload protection.

**However, note that higher export frequency and larger queue sizes may increase system resource consumption. You should carefully estimate factors such as message TPS and available system resources before enabling this feature and apply appropriate configurations.**

:::
