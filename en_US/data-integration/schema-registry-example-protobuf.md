# Schema Registry + Rule Engine Example - Protobuf

## Decoding Scenario

A device publishes a binary message encoded using Protobuf, which needs to be matched by
the rule engine and then republished to the topic associated with the `name` field. The
format of the topic is `person/${name}`.

For example, let's see how to republish a message with the `name` field equal to "Shawn"
to the topic `person/Shawn`.

### Create Schema

In the [Dashboard](http://127.0.0.1:18083/#/schema/create) interface of EMQX,
create a Protobuf Schema using the following parameters:

1. Name: `protobuf_person`

2. Schema Type: `Protobuf`

3. Schema:

```protobuf
message Person {
  required string name = 1;
  required int32 id = 2;
  optional string email = 3;
}
```

![](./assets/schema_registry/protobuf_create1.png)

### Creating the rule

**Use the Schema you have just created to write the rule SQL statement:**

```sql
SELECT
  schema_decode('protobuf_person', payload, 'Person') as person, payload
FROM
  "t/#"
WHERE
  person.name = 'Shawn'
```

The key point here is `schema_decode('protobuf_person', payload, 'Person')`:

- The `schema_decode` function decodes the contents of the payload field according to the
  Schema `protobuf_person`;
- `as person` stores the decoded value in the variable `person`;
- The last argument `Person` specifies that the message type in the payload is the
  `Person` type defined in the Protobuf schema.

**Then add the action using the following parameters:**

- Action Type: `Republish`
- Destination Topic: `person/${person.name}`
- Message Content Template: `${person}`

This action sends the decoded user to the topic `person/${person.name}` in JSON
format. `${person.name}` is a variable placeholder that will be replaced at runtime with
the value of the `name` field from the decoded message.

### Device side code

Once the rules have been created, it is time to simulate the data for testing.

The following code uses the Python language to fill a User message, encode it as binary
data, then send it to the `t/1` topic. See [full
code](https://gist.github.com/thalesmg/3c5fdbae2843d63c2380886e69d6123c) for details.

```python
def publish_msg(client):
    p = person_pb2.Person()
    p.id = 1
    p.name = "Shawn"
    p.email = "shawn@example.com"
    message = p.SerializeToString()
    topic = "t/1"
    print("publish to topic: t/1, payload:", message)
    client.publish(topic, payload=message, qos=0, retain=False)
```

### Checking rule execution results

1) In the Dashboard's [Websocket](http://127.0.0.1:18083/#/websocket) tools, log in to a
MQTT Client and subscribe to `person/#`.

2) Install the Python dependencies and execute the device-side code:

```shell
$ pip3 install protobuf paho-mqtt
$ protoc --python_out=. person.proto

$ python3 protobuf_mqtt.py
Connected with result code 0
publish to topic: t/1, payload: b'\n\x05Shawn\x10\x01\x1a\x11shawn@example.com'
```

3) Check that a message with the topic `person/Shawn` is received on the Websocket
side:

```
{"name":"Shawn","id":1,"email":"shawn@example.com"}
```

## Encoding Scenario

A device subscribes to a topic `protobuf_out` expecting a binary message encoded using
Protobuf. The Rule Engine is used to encode such message and publish it to the associated
topic.

### Create Schema

Use the same schema as described in the [decoding scenario](#decoding-scenario).

### Creating the rule

**Use the Schema you have just created to write the rule SQL statement:**

```sql
SELECT
  schema_encode('protobuf_person', json_decode(payload), 'Person') as protobuf_person
FROM
  "protobuf_in"
```

The key point here is `schema_encode('protobuf_person', payload)`:

- The `schema_encode` function encodes the contents of the payload field according to the
  Schema `protobuf_person`, and `Person` specifies the message type to be used;
- `as protobuf_person` stores the decoded value in the variable `protobuf_person`;
- `json_decode(payload)` is needed because `payload` is generally a JSON-encoded binary,
  and `schema_encode` requires a Map as its input.

**Then add the action using the following parameters:**

- Action Type: `Republish`
- Destination Topic: `protobuf_out`
- Message Content Template: `${protobuf_person}`

This action sends the Avro encoded user to the topic `protobuf_out`. `${protobuf_person}` is a
variable placeholder that will be replaced at runtime with the value of the result of
`schema_encode` (a binary value).

### Device side code

Once the rules have been created, it is time to simulate the data for testing.

The following code uses the Python language to fill a User message, encode it as binary
data, then send it to the `protobuf_in` topic. See [full
code](https://gist.github.com/thalesmg/c5f03f99f982401d16ef6583e30144fa) for details.

```python
def on_message(client, userdata, msg):
    p = person_pb2.Person()
    p.ParseFromString(msg.payload)
    print(msg.topic+" "+str(p))
```

### Checking rule execution results

1) In the Dashboard's [Websocket](http://127.0.0.1:18083/#/websocket) tools, log in to a
MQTT Client.

2) Publish a message to the `protobuf_out` topic:

```json
{"name":"Shawn","id":1,"email":"shawn@example.com"}
```

3) Install the Python dependencies and execute the device-side code:

```shell
$ pip3 install protobuf paho-mqtt

$ python3 protobuf_mqtt_sub.py
Connected with result code 0
protobuf_out name: "Shawn"
id: 1
email: "shawn@example.com"
```
