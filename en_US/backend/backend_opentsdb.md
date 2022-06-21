# OpenTSDB Backend

::: tip

After EMQX version 3.1, a powerful rule engine is introduced to replace plug-ins. It is recommended that you use it. See [Save data to OpenTSDB](../rule/backend_opentsdb.md) to setup Save data to OpenTSDB in rule engine.

:::

## Configure OpenTSDB Server

Config file: etc/plugins/emqx\_backend\_opentsdb.conf:

```bash
## OpenTSDB Server
backend.opentsdb.pool1.server = 127.0.0.1:4242

## OpenTSDB Pool Size
backend.opentsdb.pool1.pool_size = 8

## Whether to return summary info
backend.opentsdb.pool1.summary = true

## Whether to return detailed info
##
## Value: true | false
backend.opentsdb.pool1.details = false

## Synchronous write or not
##
## Value: true | false
backend.opentsdb.pool1.sync = false

## Synchronous write timeout in milliseconds
##
## Value: Duration
##
## Default: 0
backend.opentsdb.pool1.sync_timeout = 0

## Max batch size
##
## Value: Number >= 0
## Default: 20
backend.opentsdb.pool1.max_batch_size = 20

## Store PUBLISH Messages
backend.opentsdb.hook.message.publish.1 = {"topic": "#", "action": {"function": "on_message_publish"}, "pool": "pool1"}
```

Parameters in hook
rule:

| Option | Description                                                                                                     |
| ------ | --------------------------------------------------------------------------------------------------------------- |
| topic  | Configure which topics need to execute hooks                                                                    |
| action | Configure specific action for hook, `function` is a built-in function provided as Backend for general functions |
| pool   | Pool Name, used to connect multiple OpenTSDB servers                                                            |

Example:

```bash
## Store PUBLISH message whose topic is "sensor/#"
backend.influxdb.hook.message.publish.1 = {"topic": "sensor/#", "action": {"function": "on_message_publish"}, "pool": "pool1"}

## Store PUBLISH message whose topic is "stat/#"
backend.influxdb.hook.message.publish.2 = {"topic": "stat/#", "action": {"function": "on_message_publish"}, "pool": "pool1"}
```

## Description of OpenTSDB Persistence Hooks

| hook            | topic | action               | Description              |
| --------------- | ----- | -------------------- | ------------------------ |
| message.publish | \#    | on\_message\_publish | Store published messages |

Since MQTT Message cannot be written directly to OpenTSDB, OpenTSDB
Backend provides an emqx\_backend\_opentsdb.tmpl template file to
convert MQTT Message to DataPoint that can be written to OpenTSDB.

Template file use Json format:

  - `key` - MQTT Topic, Json String, support wildcard characters
  - `value` - Template, Json Object, used to convert MQTT Message into
    `measurement,tag_key=tag_value,... field_key=field_value,...
    timestamp` and write to InfluxDB。

You can define different templates for different topics or multiple
templates for the same topic, likes:

```bash
{
    <Topic 1>: <Template 1>,
    <Topic 2>: <Template 2>
}
```

The template format is as follows:

```bash
{
    "measurement": <Measurement>,
    "tags": {
        <Tag Key>: <Tag Value>
    },
    "value": <Value>,
    "timestamp": <Timestamp>
}
```

`measurement` and `value` are required options, `tags` and `timestamp`
are optional.

All values (such as `<Measurement>`) can be configured directly in the
template as a fixed value that data types supported depending on the
table you define. More realistically, of course, you can access the data
in the MQTT message through the placeholder we provide.

Currently, we support placeholders as
follows:

| Placeholder | Description                                                                         |
| ----------- | ----------------------------------------------------------------------------------- |
| $id         | MQTT Message UUID, assigned by EMQX                                                |
| $clientid   | Client ID used by the Client                                                        |
| $username   | Username used by the Client                                                         |
| $peerhost   | IP of Client                                                                        |
| $qos        | QoS of MQTT Message                                                                 |
| $topic      | Topic of MQTT Message                                                               |
| $payload    | Payload of MQTT Message, must be valid Json data                                    |
| $<Number\> | It must be used with $payload to retrieve data from Json Array                      |
| $timestamp  | The timestamp EMQX sets when preparing to forward messages, precision: Nanoseconds |

**$payload and $<Number\>:**

You can directly use `$content` to obtain the complete message payload,
you can use `["$payload", <Key>, ...]` to get the data inside the
message payload.

For example `payload` is `{"data": {"temperature": 23.9}}`, you can via
`["$payload", "data", "temperature"]` to get `23.9`.

In the case of array data type in Json, we introduced `$0` and
`$<pos_integer>`, `$0` means to get all elements in the array, and
`$<pos_integer>` means to get the <pos\_integer\>th element in the
array.

A simple example, `["$payload", "$0", "temp"]` will get `[20, 21]` from
`[{"temp": 20}, {"temp": 21}]`, and `["$payload", "$1", "temp"]` will
only get `20`.

It is worth noting that when you use `$0`, we expect the number of data
you get is same. Because we need to convert these arrays into multiple
records and write it into OpenTSDB, and when you have three pieces of
data in one field and two in another, we won't know how to combine the
data for you.

**Example**

data/templates directory provides a sample template
(emqx\_backend\_opentsdb\_example.tmpl, please remove the "\_example"
suffix from the filename when using it formally) for the user's
reference:

```bash
{
    "sample": {
        "measurement": "$topic",
        "tags": {
            "host": ["$payload", "data", "$0", "host"],
            "region": ["$payload", "data", "$0", "region"],
            "qos": "$qos",
            "clientid": "$clientid"
        },
        "value": ["$payload", "data", "$0", "temp"],
        "timestamp": "$timestamp"
    }
}
```

When an MQTT Message whose Topic is "sample" has the following Payload:

```bash
{
    "data": [
        {
            "temp": 1,
            "host": "serverA",
            "region": "hangzhou"
        },
        {
            "temp": 2,
            "host": "serverB",
            "region": "ningbo"
        }
    ]
}
```

Backend converts MQTT messages into the following data and writes it to
OpenTSDB:

```bash
[
    {
        "measurement": "sample",
        "tags": {
            "clientid": "mqttjs_ebcc36079a",
            "host": "serverA",
            "qos": "0",
            "region": "hangzhou",
        },
        "value": "1",
        "timestamp": "1560743513626681000"
    },
    {
        "measurement": "sample",
        "tags": {
            "clientid": "mqttjs_ebcc36079a",
            "host": "serverB",
            "qos": "0",
            "region": "ningbo",
        },
        "value": "2",
        "timestamp": "1560743513626681000"
    }
]
```

## Enable OpenTSDB Backend

```bash
./bin/emqx_ctl plugins load emqx_backend_opentsdb
```
