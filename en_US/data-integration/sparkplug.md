# Sparkplug B
 
[Sparkplug B](https://www.eclipse.org/tahu/) is an open-source specification from the Eclipse Foundation's TAHU project, designed to provide a well-defined payload and state management system for MQTT. The primary aim is to achieve interoperability and consistency within the industrial IoT sector.

Sparkplug B defines the MQTT namespace for SCADA (Supervisory Control and Data Acquisition) systems, real-time control systems, and devices, to ensure standardized data transmission. It encapsulates a structured data format that includes metrics, process variables, and device status information in a concise and easy-to-process way.

By using Sparkplug B, organizations can improve their operational efficiency, avoid data silos, and enable seamless communication between devices in an MQTT network.

To get started with Sparkplug B or to explore more detailed information, [the specification is a good place to start](https://www.eclipse.org/tahu/spec/sparkplug_spec.pdf).

## Sparkplug B Data Format 


Sparkplug B utilizes a well-defined payload structure to standardize data communication. At its core, it employs a [Protobuf (Protocol Buffers)](https://developers.google.com/protocol-buffers) specification for structuring Sparkplug messages, resulting in lightweight, efficient, and flexible data interchange.

EMQX offers an advanced feature called [Schema Registry](./schema-registry.md), allowing users to create custom encoders and decoders for data formats defined with [Avro](https://avro.apache.org) and [Protobuf](https://developers.google.com/protocol-buffers/), including Sparkplug B. After defining the [appropriate Sparkplug B schema](https://github.com/eclipse/tahu/blob/46f25e79f34234e6145d11108660dfd9133ae50d/sparkplug_b/sparkplug_b.proto) in the registry, one can use the `schema_decode` and `schema_encode` functions within EMQX's rule engine to access and manipulate data adhering to the specified format.

However, EMQX provides built-in support for Sparkplug B, circumventing the need to use the schema registry for this specific format. The `sparkplug_encode` and `sparkplug_decode` functions are available out-of-the-box in EMQX, simplifying the process of encoding and decoding Sparkplug B formatted messages within the rule engine. These functions ensure seamless, reliable, and efficient handling of Sparkplug B data, facilitating smooth communication within your MQTT network.

## Sparkplug B Functions

EMQX provides two rule engine SQL functions for encoding and decoding Sparkplug B data, namely `sparkplug_encode` and `sparkplug_decode`. These functions can be useful if one for example want to forward a message to a specific topic based on the content of a Sparkplug B message, or if one wants to change the Sparkplug B message in some way. The "Practical Examples" section in this document will be useful for understanding how to use these functions in different scenarios.
Also, the documentations for [EMQX rule engine rules](./rule-get-started) and the [rule engine SQL based language](./rule-sql-syntax) will be useful if you are not yet familiar with these concepts.

### sparkplug_decode

The `sparkplug_decode` function is used to decode Sparkplug B messages. It converts the raw Sparkplug B encoded payload into a more user-friendly format that can be further processed or analyzed.

Example usage:

```sql
select
  sparkplug_decode(payload) as decoded
from t
```

In the example above, `payload` refers to the raw Sparkplug B message that you wish to decode.

The [Sparkplug B Protobuf schema](https://github.com/emqx/emqx/blob/039e27a153422028e3d0e7d517a521a84787d4a8/lib-ee/emqx_ee_schema_registry/priv/sparkplug_b.proto) can provide further insights on the structure of messages.

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

In this section, we provide practical examples for handling Sparkplug B messages using the `sparkplug_decode` and `sparkplug_encode` functions.

Consider scenarios where you have a Sparkplug B encoded messages with the following structure:

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


### Extracting Data 

Now, consider a scenario where you are getting messages from a device to the topic `my/sparkplug/topic` and want to forward just the `counter_group1/counter1_run` metric to another topic called `intresting_counters/counter1_run_updates` as a JSON formatted message.

To achieve this, you can can create a rule with the following rule engine SQL statement:

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

The Sparkplug B specification recommends sending data only when it changes, leading to payloads where only a subset of metrics are present. If there is no item in the array with the specified name, this rule will not output anything.

To forward the obtained metric to the desired topic, you can can go to Actions in the dashboard UI for the rule and add a new republish action. Type in `intresting_counters/counter1_run_updates` as the republish topic and type in `${item}` in the payload field for the action.

To test this you can use the [MQTT X](https://mqttx.app/) MQTT client to publish the Sparkplug B message to the topic `my/sparkplug/topic` and see that the message is transformed and forwarded to the topic `intresting_counters/counter1_run_updates` as a JSON formatted message:
1. First, make sure that you have configured a rule using the EMQX dashboard as described above and saved it.
2. Open MQTT X and connect a client to the EMQX broker.
3. Now, click on "New Subscription" and subscribe to the topic `intresting_counters/counter1_run_updates`
4. In the bottom right corner (the send message component) in [MQTT X](https://mqttx.app/), type in `my/sparkplug/topic`
5. Also in the send message component, select `Base64` as the payload type.
6. Now, copy the following Base64 encoded Sparkplug B message which correspond to the message example given above and paste it into the data field in the send message component: `CPHh67HrMBIqChxjb3VudGVyX2dyb3VwMS9jb3VudGVyMV8xc2VjGPXh67HrMCACUKgDEikKHGNvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxXzVzZWMY9eHrseswIAJQVBIqCh1jb3VudGVyX2dyb3VwMS9jb3VudGVyMV8xMHNlYxj14eux6zAgAlAqEigKG2NvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxX3J1bhj14eux6zAgBVABEioKHWNvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxX3Jlc2V0GPXh67HrMCAFUAAYWA`
7. Click on the send button in the send message component.
8. If everything has worked as expected you should get back a message containing the following JSON:

```
{
    "timestamp":1678094561525,
    "name":"counter_group1/counter1_run",
    "int_value":1,
    "datatype":5
}
```
   
### Updating Data

Consider another scenario where you have found out that the metric with the name `counter_group1/counter1_run` is incorrect so you would like to delete this metric from the Sparkplug B encoded payload before you forward the message to some topic. You can accomplish this by using the following rule:

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

Here we use `sparkplug_decode` to decode the message and then use `jq` to filter out the metric with the name `counter_group1/counter1_run` and then use `sparkplug_encode` in the `DO` clause to encode the message again. In the republish action, you have to use `${updated_payload}` as the payload as `updated_payload` is the name we assigned to the updated Sparkplug B encoded message.

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

### Filtering Messages 

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

### Splitting Messages 

Now consider a scenario where you want to split a Sparkplug B encoded message into multiple messages so that each metrics in the metrics array is republished in a separate Sparkplug B encoded message. This can be accomplished with the following rule:

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
With the rule above we need to set the payload in the republish action to `${output_payload}` as `output_payload` is the name we assigned to the Sparkplug B encoded message in the `DO` clause.

### Splitting and Sending to Topics Based on Content 

One could also imagine a scenario where one would like to split a Sparkplug B encoded message as described above but one would also like to send each message to a different topic based on, for example, the metrics name. Let us say that the output topic name should be constructed by concatenating the strings `"my_metrics/"` with the name of the metric contained in the message. We could accomplish this with the following slightly modified code:


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

With this rule we need to set the topic name in the republish action to `${output_topic}` as `output_topic` is the name we assigned in the `DO` clause to the output topic, and the payload in the republish action should be set to `${output_payload}`. We wrap the`jq` function call in the `DO` clause with the `first` function as the `jq` function always output an array containing the output objects and in this case we are only interested in the first and only output object.

### Learning More

As you can see from the examples provided above, the Sparkplug B encoding and decoding functions can be used to perform a wide variety of tasks due to the flexibility of the rule engine and its `jq` function.
Obviously, the examples given above are just a small subset of what you can do.
To learn more about the rule engine and its `jq` function, please refer to the following documentation pages:

* [Rule Engine Rules](./rule-get-started)
* [Rule Engine SQL Language](./rule-sql-syntax)
* [The Rule Engine JQ Fuction](./rule-sql-jq)
* [Full Description of the JQ Programming Language](https://stedolan.github.io/jq/manual/)
