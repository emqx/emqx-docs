# Schema Registry + Rule Engine Example - Protobuf

## Scenario

A device publishes a binary message encoded using Protobuf, which needs to be matched by
the rule engine and then republished to the topic associated with the `name` field. The
format of the topic is `person/${name}`.

For example, let's see how to republish a message with the `name` field equal to "Shawn"
to the topic `person/Shawn`.

## Create Schema

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

## Creating the rule

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

## Device side code

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

## Checking rule execution results

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
