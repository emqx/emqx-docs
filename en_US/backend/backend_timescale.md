# Timescale Backend

::: tip

After EMQX version 3.1, a powerful rule engine is introduced to replace plug-ins. It is recommended that you use it. See [Save data to Timescale](../rule/backend_timescaledb.md) to setup Save data to Timescale in rule engine.

:::

## Configure Timescale Server

Config file: etc/plugins/emqx\_backend\_timescale.conf:

```bash
## Timescale Server
backend.timescale.pool1.server = 127.0.0.1:5432
## Timescale Pool Size
backend.timescale.pool1.pool_size = 8
## Timescale Username
backend.timescale.pool1.username = postgres
## Timescale Password
backend.timescale.pool1.password = password
## Timescale Database
backend.timescale.pool1.database = tutorial
## Timescale SSL
backend.timescale.pool1.ssl = false

## SSL keyfile.
##
## Value: File
## backend.timescale.pool1.keyfile =

## SSL certfile.
##
## Value: File
## backend.timescale.pool1.certfile =

## SSL cacertfile.
##
## Value: File
## backend.timescale.pool1.cacertfile =

## Store Publish Message
backend.timescale.hook.message.publish.1 = {"topic": "#", "action": {"function": "on_message_publish"}, "pool": "pool1"}
```

Parameters in hook
rule:

| Option | Description                                                                                                     |
| ------ | --------------------------------------------------------------------------------------------------------------- |
| topic  | Configure which topics need to execute hooks                                                                    |
| action | Configure specific action for hook, `function` is a built-in function provided as Backend for general functions |
| pool   | Pool Name, used to connect multiple Timescale servers                                                           |

Example:

```bash
## Store PUBLISH message whose topic is "sensor/#"
backend.influxdb.hook.message.publish.1 = {"topic": "sensor/#", "action": {"function": "on_message_publish"}, "pool": "pool1"}

## Store PUBLISH message whose topic is "stat/#"
backend.influxdb.hook.message.publish.2 = {"topic": "stat/#", "action": {"function": "on_message_publish"}, "pool": "pool1"}
```

## Description of Timescale Persistence Hooks

| hook            | topic | action               | Description              |
| --------------- | ----- | -------------------- | ------------------------ |
| message.publish | \#    | on\_message\_publish | Store published messages |

Timescale Backend provides the template file named
`emqx_backend_timescale.tmpl`, which is used to extract data from MQTT
messages with different topics for writing to Timescale.

Template file use Json format:

  - `key` - MQTT Topic, Json String, support wildcard characters
  - `value` - Template, Json Object, used to convert MQTT Message into
    `measurement,tag_key=tag_value,... field_key=field_value,...
    timestamp` and write to InfluxDB。

You can define different templates for different topics or multiple
templates for the same topic, likes:

```json
{
    <Topic 1>: <Template 1>,
    <Topic 2>: <Template 2> 
}
```

The template format is as follows:

```json
{
    "name": <Name of template>,
    "sql": <SQL INSERT INTO>,
    "param_keys": <Param Keys>
}
```

`name`, `sql` and `param_keys` are required options.

`name` can be any string, just make sure there are no duplicates.

`sql` is SQL INSERT INTO statement for Timescale, like `insert into
sensor_data(time, location, temperature, humidity) values (NOW(), $1,
$2, $3)`.

`param_keys` is a array, its first element corresponds to `$1` appearing
in `sql` and so on.

Any element in an array can be a fixed value, and the data type it
supports depends on the table you define. More realistically, of course,
you can access the data in the MQTT message through the placeholder we
provide.

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
| $<Number\> | It must be used with $paylaod to retrieve data from Json Array                      |
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
records and write it into Timescale, and when you have three pieces of
data in one field and two in another, we won't know how to combine the
data for you.

**Example**

data/templates directory provides a sample template
(emqx\_backend\_timescale\_example.tmpl, please remove the "\_example"
suffix from the filename when using it formally) for the user's
reference:

```json
{
    "sensor_data": {
        "name": "insert_sensor_data",
        "sql": "insert into sensor_data(time, location, temperature, humidity) values (NOW(), $1, $2, $3)",
        "param_keys": [
            ["$payload", "data", "$0", "location"],
            ["$payload", "data", "$0", "temperature"],
            ["$payload", "data", "$0", "humidity"]
        ]
    },
    "sensor_data2/#": {
        "name": "insert_sensor_data2",
        "sql": "insert into sensor_data(time, location, temperature, humidity) values (NOW(), $1, $2, $3)",
        "param_keys": [
            ["$payload", "location"],
            ["$payload", "temperature"],
            ["$payload", "humidity"]
        ]
    },
    "easy_data": {
        "name": "insert_easy_data",
        "sql": "insert into easy_data(time, data) values (NOW(), $1)",
        "param_keys": [
            "$payload"
        ]
    }
}
```

When an MQTT Message whose Topic is "sensor\_data" has the following
Payload:

```json
{
    "data":[
        {
            "location":"bedroom",
            "temperature":21.3,
            "humidity":40.3
        },
        {
            "location":"bathroom",
            "temperature":22.3,
            "humidity":61.8
        },
        {
            "location":"kitchen",
            "temperature":29.5,
            "humidity":58.7
        }
    ]
}
```

`["$payload", "data", "$0", "location"]` will extract Payload from MQTT
Message first.

If the format of Payload is json, backend continue to extract `data`
from Payload.

And the value of `data` is an array, we use `$0` to gets all elements in
the array.

`["$payload", "data", "$0", "location"]` will help us get `["bedroom",
"bathroom", "kitchen"]` finally.

Accordingly if you replace `$0` with `$1`, you get only `["bedroom"]`.

So in this scene, we will get the following SQL
statement:

```json
insert into sensor_data(time, location, temperature, humidity) values (NOW(), 'bedroom', 21.3, 40.3)
insert into sensor_data(time, location, temperature, humidity) values (NOW(), 'bathroom', 22.3, 61.8)
insert into sensor_data(time, location, temperature, humidity) values (NOW(), 'kitchen', 29.5, 58.7)
```

Eventually Timescale Backend executes these SQL statements to write data
to Timescale.
