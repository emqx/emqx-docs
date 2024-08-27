# Message Transformation

::: tip Note

Message Transformation is an EMQX Enterprise feature.

:::

## Overview

Message Transformation allows you to modify and format messages based on user-defined rules before they are processed further or delivered to subscribers. This feature is highly customizable, supporting multiple encodings and advanced transformations.

## Workflow

When a message is published, it undergoes the following workflow:

1. **Schema Validation**: When a message is published and passes authorization, it is first checked against [Schema Validation](./schema-validation.md). If the message passes, it moves to the next step.

2. **Message Transformation Pipeline**: 

   - **Transformation Matching**: The message is matched against a list of user-defined transformations based on its topic. Multiple transformations can be set for different topics or topic filters.
   - **Transformation Execution**: The matched transformations are executed in the order they are configured. The pipeline supports various encoders and decoders, such as JSON, Protobuf, and Avro, and allows for [Variform expressions](../configuration/configuration.md#variform-expressions) to enrich or modify the message.
   - **Post-Transformation Processing**: Once the message successfully passes through the transformation pipeline, it proceeds to the next steps, such as triggering the rule engine or dispatching the message to subscribers.

3. **Failure Handling**: If a transformation fails, user-configured actions are executed:

   - **Discard Message**: Terminate the publish and discard the message, returning a specific reason code (131 - Implementation Specific Error) for QoS 1 and QoS 2 messages via PUBACK.
   - **Disconnect and Discard Message**: Discard the message and disconnect the publishing client.
   - **Ignore**: No additional actions are taken.

   A log entry can be generated whenever a transformation fails, regardless of the configured action. Users can configure the log's output level, which defaults to `warning`. Additionally, a transformation failure can trigger a rule engine event (`$events/message_transformation_failed`), enabling users to implement custom handling, such as republishing the erroneous message to another topic or sending it to Kafka for further analysis.

## User Guide

This section demonstrates how to configure the message transformation feature and how to test your setup.

### Configure Message Transformation in Dashboard

This section demonstrates how to create and configure a message transformation in the Dashboard.

1. Go to Dashboard, and click **Integrations** -> **Message Transform** in the left navigation menu.
2. Click **Create** at the top right of the **Message Transform** page.
3. On the Create Message Transform page, configure the following information:
   - **Name**: Enter the name of the transformation.
   - **Message Source Topic**: Set the topics whose messages need to be transformed. Multiple topics or topic filters can be set.
   - **Note** (optional): Enter any notes.
   - **Message Format Transformation**:
     - **Source Format**: Specifies the payload decoder to be applied to messages entering the transformation pipeline. The available options are `None` (no decoding), `JSON`, `Avro`, or `Protobuf`. These decoders convert the binary input payload into a structured map. If you select `Avro` or `Protobuf`, their schemas must first be defined in the [Schema Registry](./schema-registry.md). In a pipeline with multiple transformations, decoding is not required at each step. For example, if transformation `T1` has already decoded the payload, subsequent transformation `T2` can skip decoding, relying on the payload already being in the correct format.
     - **Target Format**: Specifies the payload encoder to encode the final message payload at the end of the transformation pipeline as a binary value. The encoder options are the same as those for the **Source Format**: `None`, `JSON`, `Avro`, or `Protobuf`. Only the last transformation in the pipeline needs to ensure the payload is encoded as a binary value; intermediate transformations do not need to handle binary encoding.
   - **Message Properties Transformation**:
     - **Properties**: Specifies the destination where the transformed value, resulting from an expression, will be written. Valid destinations include `payload`, `topic`, `qos`, `retain` (to set the corresponding flag), and `user_property` (for the `User-Property` MQTT property). When using `user_property`, exactly one key must be specified under this field (e.g., `user_property.my_custom_prop`). The `payload` can either be used as-is, overwriting the entire message payload, or a specific nested key path can be designated, treating the payload like a nested JSON object (e.g., `payload.x.y`).
     - **Target Value**: Defines the value to be written to the configured property. This value can either be copied from other fields such as `qos`, `retain`, `topic`, `payload`, and `payload.x.y`, or it can be generated using a [variform expression](../configuration/configuration.md#variform-expressions).
   - **Transformation Failure Operation**:
     - **Action After Failure**: Select the actions to perform if a transformation fails:
       - **Drop Message**: Terminate the publishing process and discard the message, returning a specific reason code for QoS 1 and QoS 2 messages via PUBACK.
       - **Disconnect and Drop Message**: Discard the message and disconnect the client that published it.
       - **Ignore**: Perform no additional action.
   - **Output Logs**: Select whether to generate a log entry when a transformation fails; logging is enabled by default.
   - **Logs Level**: Set the log output level; the default level is `warning`.
4. Click **Create** to complete the settings.

You can test your transformation before creating it by clicking **Preview**. This opens a new pane where you can enter the context for the incoming message, such as QoS, payload, whether the retain flag is set, and the publisher's username and client ID, among other fields. After providing the necessary details, click **Execute Transformation** to run the transformation with the specified context and view the resulting output.

Once the transformation is created, it will appear in the list on the Message Transformation page as enabled by default. You can disable it if needed or update the transformation settings by clicking **Settings** in the **Actions** column. To delete the transformation or change its position, click **More**.

### Configure Message Transformation in Configuration File

For configuration details, see [Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/).

### REST API

For detailed information on how to use message transformation through the REST API, see [EMQX Enterprise API](https://docs.emqx.com/en/enterprise/v@EE_MINOR_VERSION@/admin/api-docs.html).

### Create Decode / Encode Schemas

For more information on how to create decoder and encoder schemas, see the [Schema Registry](./schema-registry) section.

## Statistics and Indicators

When enabled, the message transformation exposes statistics and indicators on the Dashboard. You can click the name of the transformation on the Message Transformation page to see the following:

**Statistics**:

- **Total**: The total number of triggers since the system started.
- **Success**: Number of successful data transformations.
- **Failed**: Number of failed data transformations.

**Rate Indicators**:

- Current transformation speed
- Speed in the last 5 minutes
- Historical maximum speed

Statistics can be reset and are also available via Prometheus at `/prometheus/message_transformation`.
