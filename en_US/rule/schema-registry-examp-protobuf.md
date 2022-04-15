# Custom codec example - Protobuf

## Rule requirements

The device publishes a binary message encoded using Protobuf, which needs to be matched by the rule engine and then republished to the topic associated with the "name" field. The format of the topic is "person/${name}".

For example, republish a message with the "name" field as "Shawn" to the topic "person/Shawn".

## Create schema

In the [Dashboard](http://127.0.0.1:18083/#/schemas/0?oper=create) interface of EMQX, create a Protobuf Schema using the following parameters:

1. Name: protobuf_person

2. Codec Type: protobuf

3. Schema: The following protobuf schema defines a Person message.

```protobuf
message Person {
  required string name = 1;
  required int32 id = 2;
  optional string email = 3;
}
```

## Creating rules

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

- The `schema_decode` function decodes the contents of the payload field according to the Schema 'protobuf_person';
- `as person` stores the decoded value in the variable "person";
- The last argument `Person` specifies that the message type in the payload is the 'Person' type defined in the protobuf schema.

**Then add the action using the following parameters:**

- Action Type: Message republishing
- Destination Topic: person/${person.name}
- Message Content Template: ${person}

This action sends the decoded "person" to the topic `person/${person.name}` in JSON format. `${person.name}` is a variable placeholder that will be replaced at runtime with the value of the "name" field in the message content.

## Device side code

Once the rules have been created, it is time to simulate the data for testing.

The following code uses the Python language to fill a Person message and encode it as binary data, then sends it to the "t/1" topic. See [full code](https://github.com/terry-xiaoyu/schema-registry-examples/blob/master/protobuf/pb2_mqtt.py) for details.

```python
def publish_msg(client):
    p = person_pb2.Person()
    p.id = 1
    p.name = "Shawn"
    p.email = "liuxy@emqx.io"
    message = p.SerializeToString()
    topic = "t/1"
    print("publish to topic: t/1, payload:", message)
    client.publish(topic, payload=message, qos=0, retain=False)
```

## Checking rule execution results

1)  In the Dashboard's [Websocket](http://127.0.0.1:18083/#/websocket) tools, log in to an MQTT Client and subscribe to "person/#".

2)  Install the python dependency and execute the device-side code:

```shell
$ pip3 install protobuf
$ pip3 install paho-mqtt

$ python3 ./pb2_mqtt.py
Connected with result code 0
publish to topic: t/1, payload: b'\n\x05Shawn\x10\x01\x1a\rliuxy@emqx.io'
t/1 b'\n\x05Shawn\x10\x01\x1a\rliuxy@emqx.io'
```

3) Check that a message with the topic `person/Shawn` is received on the Websocket side:

```bash
{"email":"liuxy@emqx.io","id":1,"name":"Shawn"}
```
