# Sparkplug B

::: tip Note

Schema Registry is an EMQX Enterprise feature. Currently, only the EMQX Enterprise supports the encoding and decoding of the Sparkplug B data format.

:::

[Sparkplug](https://www.eclipse.org/tahu/spec/sparkplug_spec.pdf) is an open-source specification developed by the [Eclipse Foundation's TAHU project](https://www.eclipse.org/tahu/), designed to provide a well-defined payload and state management system for MQTT. The primary aim is to achieve interoperability and consistency within the industrial IoT sector.

Sparkplug encoding scheme version B (Sparkplug B) defines the MQTT namespace for Supervisory Control and Data Acquisition (SCADA) systems, real-time control systems, and devices. It ensures standardized data transmission by encapsulating a structured data format that includes metrics, process variables, and device status information in a concise and easy-to-process format. By using Sparkplug B, organizations can improve their operational efficiency, avoid data silos, and enable seamless communication between devices within an MQTT network.

This page guides you through the implementation of Sparkplug B in EMQX including data format, functions, and practical examples.

## Sparkplug B Data Format 


Sparkplug B utilizes a well-defined payload structure to standardize data communication. At its core, it employs [Protocol Buffers (Protobuf)](https://developers.google.com/protocol-buffers) for structuring Sparkplug messages, resulting in lightweight, efficient, and flexible data interchange.

EMQX offers advanced support for Sparkplug B through the [Schema Registry](./schema-registry.md) feature. With the Schema Registry, you can create custom encoders and decoders for various data formats, including Sparkplug B. By defining the [appropriate Sparkplug B schema](https://github.com/eclipse/tahu/blob/46f25e79f34234e6145d11108660dfd9133ae50d/sparkplug_b/sparkplug_b.proto) in the registry, you can use the `schema_decode` and `schema_encode` functions in EMQX's rule engine to access and manipulate data adhering to the specified format.

Additionally, EMQX offers built-in support for Sparkplug B, eliminating the need for the schema registry for this specific format. The `sparkplug_encode` and `sparkplug_decode` functions are readily available in EMQX, simplifying the encoding and decoding of Sparkplug B messages within the rule engine.

## Sparkplug B Functions

EMQX provides two rule engine SQL functions for encoding and decoding Sparkplug B data: `sparkplug_encode` and `sparkplug_decode`.  The [Practical Examples](#practical-examples) section helps you to understand how to use these functions in different scenarios.

The Sparkplug B encoding and decoding functions can be used to perform a wide variety of tasks due to the flexibility of the rule engine and its `jq` function. To learn more about the rule engine and its `jq` function, refer to the following pages:

* [Create Rules](./rule-get-started.md)
* [Rule Engine SQL Language](./rule-sql-syntax.md)
* [The Rule Engine JQ Fuction](./rule-sql-jq.md)
* [Full Description of the JQ Programming Language](https://stedolan.github.io/jq/manual/)

### sparkplug_decode

The `sparkplug_decode` function is used to decode Sparkplug B messages, for example, if you want forward a message to a specific topic based on the contents of a Sparkplug B encoded message or change the Sparkplug B message in some way. It converts the raw Sparkplug B encoded payload into a more user-friendly format that can be further processed or analyzed.

Example usage:

```sql
select
  sparkplug_decode(payload) as decoded
from t
```

In the example above, `payload` refers to the raw Sparkplug B message that you wish to decode.

The [Sparkplug B Protobuf schema](https://github.com/emqx/emqx/blob/039e27a153422028e3d0e7d517a521a84787d4a8/lib-ee/emqx_ee_schema_registry/priv/sparkplug_b.proto) can provide further insights into the structure of messages.

### sparkplug_encode

The `sparkplug_encode` function is used to encode data into a Sparkplug B message. This is particularly useful when you need to send Sparkplug B messages to MQTT clients or other components of your system.

Example usage:

```sql
select
  sparkplug_encode(json_decode(payload)) as encoded
from t
```

In the example above, `payload` refers to the data that you wish to encode into a Sparkplug B message.

## Practical Examples

This section provides practical examples for handling Sparkplug B messages using the `sparkplug_decode` and `sparkplug_encode` functions. Note that the examples given are just a small subset of what you can do.

Consider scenarios where you have a Sparkplug B encoded message with the following structure:

```json
{
  "timestamp": 1678094561521,
  "seq": 88,
  "metrics": [
    {
      "timestamp": 1678094561525,
      "name": "counter_group1/counter1_1sec",
      "int_value": 424,
      "datatype": 2
    },
    {
      "timestamp": 1678094561525,
      "name": "counter_group1/counter1_5sec",
      "int_value": 84,
      "datatype": 2
    },
    {
      "timestamp": 1678094561525,
      "name": "counter_group1/counter1_10sec",
      "int_value": 42,
      "datatype": 2
    },
    {
      "timestamp": 1678094561525,
      "name": "counter_group1/counter1_run",
      "int_value": 1,
      "datatype": 5
    },
    {
      "timestamp": 1678094561525,
      "name": "counter_group1/counter1_reset",
      "int_value": 0,
      "datatype": 5
    }
  ]
}
```


### Extract Data 

Suppose you get messages from a device on the topic `my/sparkplug/topic` and want to forward just the `counter_group1/counter1_run` metric to another topic called `intresting_counters/counter1_run_updates` as a JSON formatted message. The instructions below demonstrate how to achieve this task by creating a rule in EMQX Dashboard and testing the rule using [MQTTX](https://mqttx.app/) client tool.

#### Create Rule in Dashboard

1. Go to EMQX Dashboard. Click **Integration** -> **Rules** from the left navigation menu. Click **+ Create** to enter the **Create Rule** page.

2. Enter the following SQL statement in **SQL Editor**:

   ```sql
   FOREACH
   jq('
         .metrics[] |
         select(.name == "counter_group1/counter1_run")
      ',
      sparkplug_decode(payload)) AS item
   DO item
   FROM "my/sparkplug/topic"
   ```

   Here, `jq` function is used to iterate over the array of metrics and filter out the one with the name "`counter_group1/counter1_run`".

   ::: tip

   The Sparkplug B specification recommends sending data only when it changes, leading to payloads where only a subset of metrics are present. If there is no item in the array with the specified name, this rule will not output anything.

   :::

2. Click **+ Add Action** on the right side of the page. Select`Republish` from the **Action** drop-down list. Enter `intresting_counters/counter1_run_updates` as the republish topic and enter `${item}` in the **Payload** field for the action. Click **Add**.
3.  Back on the **Create Rule** page, click **Create**. You can see a rule is created in the Rule list.

#### Test the Rule 

You can simulate an MQTT client using the MQTTX client tool to publish the Sparkplug B message to the topic `my/sparkplug/topic`. Then, you can verify that the message is transformed and forwarded to the topic `intresting_counters/counter1_run_updates` as a JSON formatted message:

1. Open MQTTX client desktop and connect to the EMQX broker. For detailed information on working with the MQTTX, refer to [MQTTX Client](../messaging/publish-and-subscribe.md).

2. Create a new subscription and subscribe to the topic `intresting_counters/counter1_run_updates`.

3. In the message-sending area at the lower right corner, enter `my/sparkplug/topic` as the topic. Select `Base64` as the payload type.

4. Copy the following Base64 encoded Sparkplug B message and paste it into the payload field. The message corresponds to the encoded Sparkplug message example given previously. `CPHh67HrMBIqChxjb3VudGVyX2dyb3VwMS9jb3VudGVyMV8xc2VjGPXh67HrMCACUKgDEikKHGNvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxXzVzZWMY9eHrseswIAJQVBIqCh1jb3VudGVyX2dyb3VwMS9jb3VudGVyMV8xMHNlYxj14eux6zAgAlAqEigKG2NvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxX3J1bhj14eux6zAgBVABEioKHWNvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxX3Jlc2V0GPXh67HrMCAFUAAYWA`

5. Click the send button to send the message.

   If everything has worked as expected, you should receive a message in JSON format like the following:

   ```json
   {
       "timestamp":1678094561525,
       "name":"counter_group1/counter1_run",
       "int_value":1,
       "datatype":5
   }
   ```

### Update Data

Consider a scenario where you discover an incorrect metric named `counter_group1/counter1_run` and want to remove it from the Sparkplug B encoded payload before forwarding the message. 

Similar to the demonstration in [Extract Data](#extract-data), you can create the following rule with a republish action in EMQX Dashboard.

```sql
FOREACH
jq('
   # Save payload
   . as $payload |
   # Save name of metric to delete
   "counter_group1/counter1_run" as $to_delete |
   # Filter out metric with name $to_delete
   [ .metrics[] | select(.name != $to_delete) ] as $updated_metrics |
   # Update payload with new metrics
   $payload | .metrics = $updated_metrics
   ',
   sparkplug_decode(payload)) AS item
DO sparkplug_encode(item) AS updated_payload
FROM "my/sparkplug/topic"
```

In this rule, `sparkplug_decode` is used to decode the message and then `jq` is used to filter out the metric with the name `counter_group1/counter1_run`. Then, `sparkplug_encode` in the `DO` clause is used to encode the message again. 

In the republish action, use `${updated_payload}` as the payload because it is the name assigned to the updated Sparkplug B encoded message.

Similarly, you can also use `sparkplug_decode` and `sparkplug_encode` to update the value of a metric. Consider a scenario where you want to update the value of the metric with the name `counter_group1/counter1_run` to 0. You can achieve this by using the following rule:

```sql
FOREACH
jq('
   # Save payload
   . as $payload |
   # Save name of metric to update
   "counter_group1/counter1_run" as $to_update |
   # Update value of metric with name $to_update
   [
     .metrics[] |
     if .name == $to_update
        then .int_value = 0
        else .
     end
   ] as $updated_metrics |
   # Update payload with new metrics
   $payload | .metrics = $updated_metrics
   ',
   sparkplug_decode(payload)) AS item
DO sparkplug_encode(item) AS item
FROM "my/sparkplug/topic"
```

Or consider a scenario where you want to add a new metric with the name `counter_group1/counter1_new` and value 42. You can achieve this by using the following rule:

```sql
FOREACH
jq('
   # Save payload
   . as $payload |
   # Save old metrics
   $payload | .metrics as $old_metrics |
   # New value
   {
     "name": "counter_group1/counter1_new",
     "int_value": 42,
     "datatype": 5
   } as $new_value |
   # Create new metrics array 
   ($old_metrics + [ $new_value ]) as $updated_metrics |
   # Update payload with new metrics
   $payload | .metrics = $updated_metrics
   ',
   sparkplug_decode(payload)) AS item
DO sparkplug_encode(item) AS item 
FROM "my/sparkplug/topic"
```

### Filter Messages 

Consider a scenario where you want to forward only the messages where the value of the metric with the name `counter_group1/counter1_run` is greater than 0. You can achieve this by using the following rule:

```sql
FOREACH
jq('
   # Save payload
   . as $payload |
   # Save name of metric to filter on
   "counter_group1/counter1_run" as $to_filter |
   .metrics[] | select(.name == $to_filter) | .int_value as $value |
   # Filter out messages where value of metric with name $to_filter is 0 or smaller
   if $value > 0 then $payload else empty end
   ',
   sparkplug_decode(payload)) AS item
DO sparkplug_encode(item) AS item 
FROM "my/sparkplug/topic"
```

In the above rule, the `jq` function outputs an empty array if the value of the metric with the name `counter_group1/counter1_run` is 0 or smaller. This means that the message will not be forwarded to any of the actions connected to the rule, if the value is 0 or smaller.

### Split Messages 

Consider a scenario where you want to split a Sparkplug B encoded message into multiple messages, with each metric in the metrics array is republished as a separate Sparkplug B encoded message. This can be accomplished with the following rule:

```sql
FOREACH
jq('
   # Save payload
   . as $payload |
   # Output one message for each metric
   .metrics[] |
        . as $metric |
        # Let the current metric be the only one in the metrics array
        $payload | .metrics = [ $metric ]
   ',
   sparkplug_decode(payload)) AS item
DO sparkplug_encode(item) AS output_payload
FROM "my/sparkplug/topic"
```

In the above rule, the `jq` function outputs an array with multiple items (given that there is more than one item in the metrics array).
All the actions connected to the rule will be triggered for each item in the array.
With the rule above you need to set the payload in the republish action to `${output_payload}` as `output_payload` is the name we assigned to the Sparkplug B encoded message in the `DO` clause.

### Split Messages and Send to Topics Based on Content 

Consider a scenario where you want to split a Sparkplug B encoded message but you also want to send each message to a different topic based on, for example, the metrics name. Suppose that the output topic name should be constructed by concatenating the strings `"my_metrics/"` with the name of the metric contained in the message. You can accomplish this with the following slightly modified code:


```sql
FOREACH
jq('
   # Save payload
   . as $payload |
   # Output one message for each metric
   .metrics[] |
        . as $metric |
        # Let the current metric be the only one in the metrics array
        $payload | .metrics = [ $metric ]
   ',
   sparkplug_decode(payload)) AS item
DO
sparkplug_encode(item) AS output_payload,
first(jq('"my_metrics/" + .metrics[0].name', item)) AS output_topic
FROM "my/sparkplug/topic"
```

To configure the republish action, set the topic name to `${output_topic}` as it is the name assigned in the `DO` clause to the output topic and set the payload to `${output_payload}`.

The `jq` function call is wrapped in the `DO` clause using the `first` function to obtain the first and only output object.
