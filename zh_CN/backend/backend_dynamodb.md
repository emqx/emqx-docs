---
enterprise: true
---
# DynamoDB 消息存储

::: tip

EMQ X 3.1 版本后推出强大的规则引擎用于替换插件，建议您前往使用[保存数据到 DynamoDB](../rule/backend_dynamodb.md)规则引擎中创建 保存数据到 DynamoDB

:::

## 配置 DyanmoDB 消息存储

配置文件: etc/plugins/emqx_backend_dynamo.conf

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

*backend*
消息存储规则包括:

| hook                | topic | action                         | 说明         |
| ------------------- | ----- | ------------------------------ | ---------- |
| client.connected    |       | on_client_connected          | 存储客户端在线状态  |
| client.connected    |       | on_subscribe_lookup          | 订阅主题       |
| client.disconnected |       | on_client_disconnected       | 存储客户端离线状态  |
| session.subscribed  | \#    | on_message_fetch_for_queue | 获取一对一离线消息  |
| session.subscribed  | \#    | on_retain_lookup             | 获取retain消息 |
| message.publish     | \#    | on_message_publish           | 存储发布消息     |
| message.publish     | \#    | on_message_retain            | 存储retain消息 |
| message.publish     | \#    | on_retain_delete             | 删除retain消息 |
| message.acked       | \#    | on_message_acked_for_queue | 一对一消息ACK处理 |

## DynamoDB 数据库创建表

```bash
./test/dynamo_test.sh
```

## DynamoDB 用户状态表(Client Table)

*mqtt_client* 表定义(存储设备在线状态):

```json
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

查询设备在线状态:

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

## DynamoDB 用户订阅主题(Subscription Table)

*mqtt_sub* 表定义(存储订阅关系):

```json
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

查询 ClientId 为 "test-dynamo"
的客户端已订阅主题:

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

## DynamoDB 发布消息(Message Table)

*mqtt_msg* 表定义(存储 MQTT 消息):

```json
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

*mqtt_topic_msg_map* 表定义(存储主题和消息的映射关系):

```json
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

某个客户端向主题 test 发布消息后，查询 *mqtt_msg* 表和 *mqtt_topic_msg_map* 表:

查询 mqtt_msg
表:

```bash
aws dynamodb scan --table-name mqtt_msg --region us-west-2  --endpoint-url http://localhost:8000

>   - {
>       - "Items": \[
>           - {
>             "arrived": { "N": "1562308553" }, "qos": { "N": "1" },
>             "sender": { "S": "mqttjs_231b962d5c" }, "payload": { "S":
>             "{ "msg": "Hello, World\!" }"}, "retain": { "N": "0" },
>             "msgid": { "S":
>             "Mjg4MTk1MDYwNTk0NjYwNzYzMTg4MDk3OTQ2MDU2Nzg1OTD" },
>             "topic": { "S": "test" }
>         }
>     \], "Count": 1, "ScannedCount": 1, "ConsumedCapacity": null
> }

```

查询 mqtt_topic_msg_map 表：

```bash
aws dynamodb scan --table-name mqtt_topic_msg_map --region us-west-2  --endpoint-url http://localhost:8000


>   - {
>       - "Items": \[
>           - {
>             "topic": { "S": "test" }, "MsgId": { "SS": \[
>             "Mjg4MTk1MDYwNTk0NjYwNzYzMTg4MDk3OTQ2MDU2Nzg1OTD" \]}
>         }
>     \], "Count": 1, "ScannedCount": 1, "ConsumedCapacity": null
> }
```

## DynamoDB 保留消息(Retain Message Table)

*mqtt_retain* 表定义(存储 retain 消息):

```json
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

某个客户端向主题 test 发布消息后，查询 *mqtt_retain* 表:

```json
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

## DynamoDB 接收消息 ack (Message Acked Table)

*mqtt_acked* 表定义(存储确认的消息):

```json
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

某个客户端向主题 test 发布消息后，查询 *mqtt_acked* 表:

```json
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

启用 DynamoDB 消息存储:

```bash
./bin/emqx_ctl plugins load emqx_backend_dynamo
```
