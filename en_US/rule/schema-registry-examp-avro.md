# Custom codec example - Avro

## Rule requirements

The device publishes a binary message encoded using Avro, which needs to be matched by the rule engine and then republished to the topic associated with the "name" field. The format of the topic is "avro_user/${name}".

For example, republish a message with the "name" field as "Shawn" to the topic "avro_user/Shawn".

## Create Schema

In the [Dashboard](http://127.0.0.1:18083/#/schemas/0?oper=create) interface of EMQX, create an Avro Schema using the following parameters:

1. Name: avro_user

2. Codec Type: avro

3. Schema:

```protobuf
{
"type":"record",
"fields":[
    {"name":"name", "type":"string"},
    {"name":"favorite_number", "type":["int", "null"]},
    {"name":"favorite_color", "type":["string", "null"]}
]
}
```

## Creating rules

**Use the Schema you have just created to write the rule SQL statement:**

```sql
SELECT
  schema_decode('avro_user', payload) as avro_user, payload
FROM
  "t/#"
WHERE
  avro_user.name = 'Shawn'
```

The key point here is `schema_decode('avro_user', payload)`:

- The `schema_decode` function decodes the contents of the payload field according to the Schema "avro_user";
- `as avro_user` stores the decoded value in the variable "avro_user";

**Then add the action using the following parameters:**

- Action Type: Message republishing
- Destination Topic: avro_user/${avro_user.name}
- Message Content Template: ${avro_user}

This action sends the decoded "user" to the topic `avro_user/${avro_user.name}` in JSON format. `${avro_user.name}` is a variable placeholder that will be replaced at runtime with the value of the "name" field in the message content.

## Device side code

Once the rules have been created, it is time to simulate the data for testing.

The following code uses the Python language to fill a User message and encode it as binary data, then sends it to the "t/1" topic. See [full code](https://github.com/terry-xiaoyu/schema-registry-examples/blob/master/avro/avro_mqtt.py) for details.

```python
def publish_msg(client):
    datum_w = avro.io.DatumWriter(SCHEMA)
    buf = io.BytesIO()
    encoder = avro.io.BinaryEncoder(buf)
    datum_w.write({"name": "Shawn", "favorite_number": 666, "favorite_color": "red"}, encoder)
    message = buf.getvalue()
    topic = "t/1"
    print("publish to topic: t/1, payload:", message)
    client.publish(topic, payload=message, qos=0, retain=False)
```

## Checking rule execution results

1)  In the Dashboard's [Websocket](http://127.0.0.1:18083/#/websocket) tools, log in to an MQTT Client and subscribe to "avro_user/#".

2)  Install the python dependency and execute the device-side code:

```shell
$ pip3 install protobuf
$ pip3 install paho-mqtt

$ python3 avro_mqtt.py
Connected with result code 0
publish to topic: t/1, payload: b'\nShawn\x00\xb4\n\x00\x06red'
```

3) Check that a message with the topic `avro_user/Shawn` is received on the Websocket side:

```
{"favorite_color":"red","favorite_number":666,"name":"Shawn"}
```
