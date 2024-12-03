# Integrate OpenTelemetry Tracing

[OpenTelemetry tracing](https://opentelemetry.io/docs/concepts/signals/traces/) is a specification for tracing the flow of requests in a distributed system, allowing you to trace how requests flow through a distributed system and providing the ability to visualize and analyze the performance and behavior of requests. In the MQTT scenario, this concept can be used to trace requests across different participants in MQTT message transmission (Publisher - MQTT server - Subscriber).

"Trace context" is a mechanism used in distributed tracing to track and identify requests or transactions that span multiple systems and services. In the [W3C Trace Context MQTT](https://w3c.github.io/trace-context-mqtt/) document, this concept is applied to the MQTT protocol to enable tracking of requests across different participants in MQTT message transmission. This allows system administrators or developers to understand how messages flow through the system.

EMQX's inherent ability to propagate trace context enables it to seamlessly participate in distributed tracing systems. This propagation is achieved simply by forwarding the `traceparent` and `tracestate` user properties from the message publisher to the subscriber. When EMQX forwards an application message to a client, it ensures the integrity of the trace context is maintained and transmitted unchanged. This method is fully compliant with [MQTT specification 3.3.2.3.7](https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901116), ensuring consistency and reliability in trace data transmission.

::: tip Note

User-Property was introduced in MQTT 5.0, so EMQX can extract and propagate Trace Context only when MQTT 5.0 is used.

For non-MQTT 5.0 clients, it is necessary to enable the **Traces All Messages** option in EMQX. EMQX will automatically add a trace ID to messages for internal distributed tracing.

:::

With OpenTelemetry distributed tracing, EMQX system administrators or developers can monitor and analyze the performance and behavior of IoT applications in real-time. It allows for quick detection and resolution of issues when they occur.

This page introduces how to integrate OpenTelemetry tracing with EMQX, detailing the setup of the OpenTelemetry Collector and the enabling and configuring of OpenTelemetry trace integration in EMQX, as well as managing tracing span overload.

## Set Up OpenTelemetry Collector

Before integrating EMQX with OpenTelemetry traces, you need to deploy and configure [OpenTelemetry Collector](https://opentelemetry.io/docs/collector/getting-started) and preferably an OpenTelemetry-compatible observability platform, for example, [Jaeger](https://www.jaegertracing.io/docs/latest/deployment/). The following steps outline the deployment and configuration process:

1. Create the OpenTelemetry Collector configuration file, `otel-trace-collector-config.yaml`:

   ```yaml
   receivers:
     otlp:
       protocols:
         grpc:

   exporters:
     otlp:
       endpoint: jaeger:4317
       tls:
         insecure: true

   processors:
     batch:

   extensions:
     health_check:

   service:
     extensions: [health_check]
     pipelines:
       traces:
         receivers: [otlp]
         processors: [batch]
         exporters: [otlp]
   ```

2. In the same directory, create a Docker Compose file, `docker-compose-otel-trace.yaml`:

   ```yaml
   version: '3.9'
   services:
     jaeger:
       image: jaegertracing/all-in-one:1.51.0
       restart: always
       ports:
         - "16686:16686"

     otel-collector:
       image: otel/opentelemetry-collector:0.90.0
       restart: always
       command: ["--config=/etc/otel-collector-config.yaml", "${OTELCOL_ARGS}"]
       volumes:
         - ./otel-trace-collector-config.yaml:/etc/otel-collector-config.yaml
       ports:
         - "13133:13133" # Health check extension
         - "4317:4317"   # OTLP gRPC receiver
       depends_on:
         - jaeger
   ```

3. Start the services using Docker Compose:

   ```bash
   docker compose -f docker-compose-otel-trace.yaml up
   ```

4. After starting, the OpenTelemetry Collector listens on the default gRPC port (4317) on the host machine and Jaeger WEB UI can be accessed at http://localhost:16686.


## Enable OpenTelemetry Tracing in EMQX

This section guides you through enabling OpenTelemetry tracing in EMQX, demonstrating the distributed tracing capabilities in a multi-node setup.

1. Add the following configuration to the EMQX `cluster.hocon` file (assuming EMQX is running locally):

   ```bash
   opentelemetry {
     exporter { endpoint = "http://localhost:4317" }
     traces {
      enable = true
      # Whether to trace all messages
      # If a trace ID cannot be extracted from the message, a new trace ID will be generated.
      # filter.trace_all = true
    }
   }
   ```

   You can also go to **Management** -> **Monitoring** in Dashboard to configure OpenTelemetry traces integration under the **Integration** tab on the page.

2. Start the EMQX node, for example, a two-node cluster with the node names `emqx@127.0.0.1` and `emqx1@127.0.0.1`, to demonstrate the distributed tracing capabilities.

3. Subscribe clients to the same topics using [MQTTX CLI](https://mqttx.app/cli) client on different nodes and ports.

   - On the `emqx@127.0.0.1` node (default MQTT listener on port 1883):

     ```bash
     mqttx sub -t t/trace/test -h localhost -p 1883
     ```

   - On the `emqx1@127.0.0.1` node (listener on port 1884):

     ```bash
     mqttx sub -t t/trace/test -h localhost -p 1884
     ```

4. Publish a message with Trace Context by sending a message to the topic including a valid `traceparent` User-Property:

   ```bash
   mqttx pub -t t/trace/test -h localhost -p 1883 -up "traceparent: 00-cce3a024ca134a7cb4b41e048e8d98de-cef47eaa4ebc3fae-01"
   ```

5. After approximately 5 seconds (EMQX’s default interval for exporting trace data), navigate to the Jaeger WEB UI at [http://localhost:16686](http://localhost:16686/) to observe trace data:

   - Select the `emqx` service and click **Find Traces**. If the `emqx` service doesn't appear immediately, refresh the page after a few moments. You should see the message trace:

     ![Jaeger-WEB-UI-find-traces](./assets/jaeger-find-traces-en.png)

   - Click on a trace to view detailed span information and the trace timeline

     ![Jaeger-WEB-UI-trace-details](./assets/jaeger-trace-details-en.png)

In this example, EMQX traces two distinct types of spans:

 - `process_message` span starts when PUBLISH packet is received and parsed by an EMQX node and ends when the message is dispatched to local subscribers and/or forwarded to other nodes that have active subscribers.  Each span corresponds to one traced published message.

 - `send_published_message` span starts when a traced message is received by a subscriber's connection control process and ends when the outgoing packet is serialized and sent to the connection socket. Each active subscriber generates one `send_published_message` span.

## Manage Tracing Span Overload

EMQX accumulates tracing spans and exports them periodically in batches.
The exporting interval is controlled by the `opentelemetry.trace.scheduled_delay` parameter, which defaults to 5 second.
The batching trace spans processor incorporates an overload protection mechanism, allowing accumulating spans only up to a certain limit, which defaults to 2048 spans. You can configure this limit using the following configuration:

```bash
opentelemetry {
  traces { max_queue_size = 2048 }
}
```

Once the `max_queue_size` limit is reached, new tracing spans will be dropped until the current queue is exported.

::: tip Note

If a traced message is dispatched to a high number of subscribers (much higher than the value of `max_queue_size`),
it is expected that only a small number of spans will be exported and most of the spans will be dropped by the overload protection.

Increasing `max_queue_size` should always be done with extra care, as it can affect performance and memory consumption.
:::
