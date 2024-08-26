# Message Transformation

::: tip Note

Message Transformation is an EMQX Enterprise feature.

:::

## Workflow

When a message is published, it first goes through the [Schema Validation](./schema-validation) pipeline, if any matches the incoming message topic.  Please see the corresponding chapter for more information on Schema Validation.  If the messages passes the schema validation, then it enters the message transformation pipeline, if any transformation matches the incoming message topic.

1. When a message is published and passes both authorization and schema validations, transformation rules are matched based on the publishing topic from a user-configured list of transformations. A transformation can be set for multiple topics or topic filters.

2. Once one or more transformations are matched, the message then enters the transformation pipeline, which obeys the order in which the user configured the transformations.

   - Supports multiple types of decoders and encoders: JSON, Protobuf, and Avro.
   - Supports [Variform expressions](../configuration/configuration.md#variform-expressions) that can enrich and transform the outgoing message.

3. Once the messages successfully passes through the transformation pipeline, the message continues to the next process, such as triggering the rule engine or dispatching to subscribers.

4. If transformation fails, the following user-configured actions can be executed:

   - **Discard Message**: Terminate the publish and discard the message, returning a specific reason code (131 - Implementation Specific Error) for QoS 1 and QoS 2 messages via PUBACK.
   - **Disconnect and Discard Message**: Discard the message and disconnect the publishing client.
   - **Ignore**: No additional actions are taken.

   Regardless of the configured action, a log entry can be generated upon transformation failure; users can configure the log's output level, which defaults to `warning`.  A transformation failure can also trigger a rule engine event `$events/message_transformation_failed`, allowing users to catch this event for custom handling, such as publishing the erroneous message to another topic or sending it to Kafka for analysis.

## User Guide

This section demonstrates how to configure the message transformation feature and how to test your setup.

### Configure Message Transformation in Dashboard

This section demonstrates how to create and configure a message transformation in the Dashboard.

1. Click on **Integrations** -> **Message Transform** in the left navigation of the Dashboard.
2. Click **Create** at the top right of the **Message Transform** page.
3. On the Create Message Transform page, configure the following information:
   - **Name**: Enter the name of the transformation.
   - **Message Source Topic**: Set the topics whose messages need to be transformed. Multiple topics or topic filters can be set.
   - **Note** (optional): Enter any notes.
   - **Message Format Transformation**:
     - **Source Format**: the payload decoder that should be applied to messages entering the transformation pipeline.  Can be one of `none` (for no decoding at all), `JSON`, `Avro` or `Protobuf`.  The decoders will internally convert the binary input payload into a structured map.  Chosing `Avro` and `Protobuf` requires their schemas to first be defined in [Schema Registry](./schema-registry).  Between multiple transformations in a pipeline, there's no need to decode multiple times.  That is: if transformation `T2` is preceded by transformation `T1`, `T2` _may opt to not use any decoders_, assuming that `T1` already decoded the payload correctly.
     - **Target Format**: the final message payload at the end of the transformation pipeline must be encoded as a binary value.  For that, the same options as in **Source Format** are supported: `none`, `JSON`, `Avro` or `Protobuf`.  Note that only the last transformation in a pipeline must ensure the binary encoding.  Intermediate transformations do not need to do so.
   - **Message Properties Transformation**:
     - **Properties**: defines the destination where the transformed value as a result of an expression will be written to.  Only a few destinations are valid here: `payload`, `topic`, `qos`, `retain` (to set the corresponding flag), `user_property` (for the `User-Property` MQTT property).  When using `user_property`, exactly one key under that field **must** be specified (e.g.: `user_property.my_custom_prop`).  `payload` may be used as-is, thus overwriting the whole contents of the message payload, or a nested key path may be specified, thus treating the payload like a nested JSON object (e.g.: `payload.x.y`).
     - **Target Value**: this defines the value that will be written to the configured property.  It may simply copy values from other fields, such as `qos`, `retain`, `topic`, `payload` and `payload.x.y`, or it can be a [variform expression](../configuration/configuration.md#variform-expressions).
   - **Transformation Failure Operation**:
     - **Action After Failure**: Select the actions to perform if transformation fails:
       - **Drop Message**: Terminate the publish and discard the message, returning a specific reason code for QoS 1 and QoS 2 messages via PUBACK.
       - **Disconnect and Drop Message**: Discard the message and disconnect the publishing client.
       - **Ignore**: Perform no additional actions.
   - **Output Logs**: Select whether to log a message upon transformation failure; default is enabled.
   - **Logs Level**: Set the log output level; the default is `warning`.

4. Click **Create** to complete the settings.

Now you can see an enabled new transformation appears in the list on the Message Transformation page. You can disable it as you need. You can update the transformation settings by clicking **Settings** in the **Actions** column. You can also delete the transformation or move its position by clicking **More**.

You may also test your transformation before it is created by clicking on **Preview**.  When doing so, a new pane shifts into view, where you may fill in the incoming message context for the transformation, such as QoS, payload, if the message retain flag is set, the username and clientid of the publisher, among other fields.  After you fill in the details and click **Execute Transformation**, the provided context will be run against the configured transformation and the final result will be shown.

### Configure Message Transformation in Configuration File

For configuration details, see [Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/).

### Creating Decode / Encode Schemas

For more information on how to create decoder and encoder schemas, please consult the chapter on [Schema Registry](./schema-registry).

### REST API

For detailed information on how to use message transformation through the REST API, see [EMQX Enterprise API](https://docs.emqx.com/en/enterprise/v@EE_MINOR_VERSION@/admin/api-docs.html).

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

Statistics are resettable and also added to Prometheus, accessible via the `/prometheus/message_transformation` path.
