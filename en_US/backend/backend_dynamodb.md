# DynamoDB Backend

::: tip

After EMQX version 3.1, a powerful rule engine is introduced to replace plug-ins. It is recommended that you use it. See [Save data to DynamoDB](../rule/backend_dynamodb.md) to setup Save data to DynamoDB in rule engine.

:::

## Configure DynamoDB Cluster

Config file: etc/plugins/emqx\_backend\_dynamo.conf

```bash
## DynamoDB Region
backend.dynamo.region = us-west-2

## DynamoDB Server
backend.dynamo.pool1.server = http://localhost:8000

## DynamoDB Pool Size
backend.dynamo.pool1.pool_size = 8

## AWS ACCESS KEY ID
backend.dynamo.pool1.aws_access_key_id = AKIAU5IM2XOC7AQWG7HK

## AWS SECRET ACCESS KEY
backend.dynamo.pool1.aws_secret_access_key = TZt7XoRi+vtCJYQ9YsAinh19jR1rngm/hxZMWR2P

## DynamoDB Backend Hooks
backend.dynamo.hook.client.connected.1    = {"action": {"function": "on_client_connected"}, "pool": "pool1"}
backend.dynamo.hook.session.created.1     = {"action": {"function": "on_subscribe_lookup"}, "pool": "pool1"}
backend.dynamo.hook.client.disconnected.1 = {"action": {"function": "on_client_disconnected"}, "pool": "pool1"}
backend.dynamo.hook.session.subscribed.1  = {"topic": "#", "action": {"function": "on_message_fetch_for_queue"}, "pool": "pool1"}
backend.dynamo.hook.session.subscribed.2  = {"topic": "#", "action": {"function": "on_retain_lookup"}, "pool": "pool1"}
backend.dynamo.hook.session.unsubscribed.1= {"topic": "#", "action": {"function": "on_acked_delete"}, "pool": "pool1"}
backend.dynamo.hook.message.publish.1     = {"topic": "#", "action": {"function": "on_message_publish"}, "pool": "pool1"}
backend.dynamo.hook.message.publish.2     = {"topic": "#", "action": {"function": "on_message_retain"}, "pool": "pool1"}
backend.dynamo.hook.message.publish.3     = {"topic": "#", "action": {"function": "on_retain_delete"}, "pool": "pool1"}
backend.dynamo.hook.message.acked.1       = {"topic": "#", "action": {"function": "on_message_acked_for_queue"}, "pool": "pool1"}

# backend.dynamo.hook.message.publish.4   = {"topic": "#", "action": {"function": "on_message_store"}, "pool": "pool1"}
```

## Description of DynamoDB Persistence Hooks

| hook                | topic | action                         | Description                     |
| ------------------- | ----- | ------------------------------ | ------------------------------- |
| client.connected    |       | on\_client\_connected          | Store client connected state    |
| client.connected    |       | on\_subscribe\_lookup          | Subscribed topics               |
| client.disconnected |       | on\_client\_disconnected       | Store client disconnected state |
| session.subscribed  | \#    | on\_message\_fetch\_for\_queue | Fetch offline messages          |
| session.subscribed  | \#    | on\_retain\_lookup             | Lookup retained messages        |
| message.publish     | \#    | on\_message\_publish           | Store published messages        |
| message.publish     | \#    | on\_message\_retain            | Store retained messages         |
| message.publish     | \#    | on\_retain\_delete             | Delete retained messages        |
| message.acked       | \#    | on\_message\_acked\_for\_queue | Process ACK                     |

## Create DynamoDB DB

```bash
./test/dynamo_test.sh
```

::: tip
DB name is free of choice
:::

## DynamoDB Client Connection Table

*mqtt\_client* stores client connection states:

```bash
{
    "TableName": "mqtt_client",
    "KeySchema": [
        { "AttributeName": "clientid", "KeyType": "HASH" }
    ],
    "AttributeDefinitions": [
        { "AttributeName": "clientid", "AttributeType": "S" }
    ],
    "ProvisionedThroughput": {
        "ReadCapacityUnits": 5,
        "WriteCapacityUnits": 5
    }
}
```

Query the client connection
state:

```bash
aws dynamodb scan --table-name mqtt_client --region us-west-2  --endpoint-url http://localhost:8000

{
    "Items": [
        {
            "offline_at": { "N": "0" },
            "node": { "S": "emqx@127.0.0.1" },
            "clientid": { "S": "mqttjs_384b9c73a9" },
            "connect_state": { "N": "1" },
            "online_at": { "N": "1562224940" }
        }
    ],
    "Count": 1,
    "ScannedCount": 1,
    "ConsumedCapacity": null
}
```

## DynamoDB Subscription Table

*mqtt\_sub* table stores MQTT subscriptions of clients:

```bash
{
    "TableName": "mqtt_sub",
    "KeySchema": [
        { "AttributeName": "clientid", "KeyType": "HASH" },
        { "AttributeName": "topic", "KeyType": "RANGE" }
    ],
    "AttributeDefinitions": [
        { "AttributeName": "clientid", "AttributeType": "S" },
        { "AttributeName": "topic", "AttributeType": "S" }
    ],
    "ProvisionedThroughput": {
        "ReadCapacityUnits": 5,
        "WriteCapacityUnits": 5
    }
}
```

Query topics subscribed by the client named
"test-dynamo":

```bash
aws dynamodb scan --table-name mqtt_sub --region us-west-2  --endpoint-url http://localhost:8000

{
    "Items": [{"qos": { "N": "2" }, "topic": { "S": "test-dynamo-sub" }, "clientid": { "S": "test-dynamo" }},
               {"qos": { "N": "2" }, "topic": { "S": "test-dynamo-sub-1"}, "clientid": { "S": "test-dynamo" }},
               {"qos": { "N": "2" }, "topic": { "S": "test-dynamo-sub-2"}, "clientid": { "S": "test-dynamo" }}],
    "Count": 3,
    "ScannedCount": 3,
    "ConsumedCapacity": null
}
```

## DynamoDB Message Table

*mqtt\_msg* stores MQTT messages:

```bash
{
    "TableName": "mqtt_msg",
    "KeySchema": [
        { "AttributeName": "msgid", "KeyType": "HASH" }
    ],
    "AttributeDefinitions": [
        { "AttributeName": "msgid", "AttributeType": "S" }
    ],
    "ProvisionedThroughput": {
        "ReadCapacityUnits": 5,
        "WriteCapacityUnits": 5
    }
}
```

*mqtt\_topic\_msg\_map* stores the mapping between topics and messages:

```bash
{
    "TableName": "mqtt_topic_msg_map",
    "KeySchema": [
        { "AttributeName": "topic", "KeyType": "HASH" }
    ],
    "AttributeDefinitions": [
        { "AttributeName": "topic", "AttributeType": "S" }
    ],
    "ProvisionedThroughput": {
        "ReadCapacityUnits": 5,
        "WriteCapacityUnits": 5
    }
}
```

Query *mqtt\_msg* and *mqtt\_topic\_msg\_map* after a client publishes a
message to the "test" topic:

Query
*mqtt\_msg*:

```bash
aws dynamodb scan --table-name mqtt_msg --region us-west-2  --endpoint-url http://localhost:8000
```

```bash
>   - {
>       - "Items": [
>           - {
>             "arrived": { "N": "1562308553" }, "qos": { "N": "1" },
>             "sender": { "S": "mqttjs\_231b962d5c" }, "payload": { "S":
>             "{ "msg": "Hello, World\!" }"}, "retain": { "N": "0" },
>             "msgid": { "S":
>             "Mjg4MTk1MDYwNTk0NjYwNzYzMTg4MDk3OTQ2MDU2Nzg1OTD" },
>             "topic": { "S": "test" }
>         }
>     ], "Count": 1, "ScannedCount": 1, "ConsumedCapacity": null
> }
```

Query
*mqtt\_topic\_msg\_map*:

```bash
aws dynamodb scan --table-name mqtt_topic_msg_map --region us-west-2  --endpoint-url http://localhost:8000
```

```bash
>   - {
>       - "Items": \[
>           - {
>             "topic": { "S": "test" }, "MsgId": { "SS": \[
>             "Mjg4MTk1MDYwNTk0NjYwNzYzMTg4MDk3OTQ2MDU2Nzg1OTD" \]}
>         }
>     \], "Count": 1, "ScannedCount": 1, "ConsumedCapacity": null
> }
```

## DynamoDB Retained Message Table

*mqtt\_retain* stores retained messages:

```bash
{
    "TableName": "mqtt_retain",
    "KeySchema": [
        { "AttributeName": "topic", "KeyType": "HASH" }
    ],
    "AttributeDefinitions": [
        { "AttributeName": "topic", "AttributeType": "S" }
    ],
    "ProvisionedThroughput": {
        "ReadCapacityUnits": 5,
        "WriteCapacityUnits": 5
    }
}
```

Query *mqtt\_retain* after a client publishes a message to the "test"
topic:

```bash
{
    "Items": [
        {
            "arrived": { "N": "1562312113" },
            "qos": { "N": "1" },
            "sender": { "S": "mqttjs_d0513acfce" },
            "payload": { "S": "test" },
            "retain": { "N": "1" },
            "msgid": { "S": "Mjg4MTk1NzE3MTY4MjYxMjA5MDExMDg0NTk5ODgzMjAyNTH" },
            "topic": { "S": "testtopic" }
        }
    ],
    "Count": 1,
    "ScannedCount": 1,
    "ConsumedCapacity": null
}
```

## DynamoDB Acknowledgement Table

*mqtt\_acked* stores acknowledgements from the clients:

```bash
{
    "TableName": "mqtt_acked",
    "KeySchema": [
        { "AttributeName": "topic", "KeyType": "HASH" },
        { "AttributeName": "clientid", "KeyType": "RANGE" }
    ],
    "AttributeDefinitions": [
        { "AttributeName": "topic", "AttributeType": "S" },
        { "AttributeName": "clientid", "AttributeType": "S" }
    ],
    "ProvisionedThroughput": {
        "ReadCapacityUnits": 5,
        "WriteCapacityUnits": 5
    }
}
```

Query *mqtt\_acked* after a client publishes a message to the "test"
topic:

```bash
{
    "Items": [
        {
            "topic": { "S": "test" },
            "msgid": { "S": "Mjg4MTk1MDYwNTk0NjYwNzYzMTg4MDk3OTQ2MDU2Nzg1OTD" },
            "clientid": { "S": "mqttjs_861e582a70" }
        }
    ],
    "Count": 1,
    "ScannedCount": 1,
    "ConsumedCapacity": null
}
```

## Enable DynamoDB Backend

```bash
./bin/emqx_ctl plugins load emqx_backend_dynamo
```
