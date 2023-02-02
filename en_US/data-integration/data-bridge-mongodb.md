# MongoDB

EMQX supports integration with MongoDB so you can save client messages and events to MongoDB.

:::tip Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridge](./data-bridges.md)

:::

## Feature list

- [Connection pool](./data-bridges.md#连接池)
- [Async mode](./data-bridges.md#异步请求模式)
- [Batch mode](./data-bridges.md#批量模式)
- [Buffer mode](./data-bridges.md#缓存队列)

## Quick starts

### Install MongoDB

Install MongoDB via Docker, and then run the docker image. 

```
#  To start the MongoDB docker image and set the password as public
docker run -d --name mongodb -p 27017:27017 mongo

# Access the container
docker exec -it mongodb bash

# Locate the MongoDB server in the container
mongo

# Create a user
use admin
db.createUser({ user: "admin", pwd: "public", roles: [ { role: "root", db: "admin" } ] })

# Create database emqx_data
use emqx_data

# create collection emqx_messages
db.createCollection('emqx_messages')
```

### Connect to MongoDB

1. Go to EMQX Dashboard, click **Data Integration** -> **Data Bridge**.
2. Click **Create** on the top right corner of the page.
3. In the **Create Data Bridge** page, click to select **MongoDB**, and then click **Next**.
4. Input a name for the data bridge. Note: It should be a combination of upper/lower case letters and numbers.
5. Set **MongoDB Mode**  and **Srv Record** as your business needs, for example, **single** and the default deselected status. 
6. Configure the MongoDB connection information. Input **emqx_data** as the **Database Name**, **127.0.0.1:27017** as the **Server Host**, **admin** as the **Username**, **public** as the **Password**, and **emqx_messages** as **Collection to be used**. For the other fields, you can keep the default setting. 
7. Configure the **Payload template** to save `clientid`, `topic`, `qos`,  `timestamp`, and `payload` to MongoDB. This template will be executed via the MongoDB insert command, and the sample code is as follows:

```
{
  "clientid": "${clientid}",
  "topic": "${topic}",
  "qos": ${qos},
  "timestamp": ${timestamp},
  "payload": ${payload}
}
```

:::tip Notes when configuring the Payload template:

- All **keys** need to be wrapped in double quotes `"`;
- Auto-derivation of the data type of "value" is not supported:
  - Characters need to be wrapped with `"`, otherwise, an error will be reported;
  - Values do not need to be wrapped, otherwise, they will be recognized as characters;
  - For timestamp, date, and time types, if no special treatment is performed, they will be recognized as numeric or character types. To store them as date or time, use the `mongo_date` function in the rule SQL to process the fields. For details, see [Time and date functions](https://github.com/emqx/emqx-docs/blob/1991eaf3eb0596726e3b397f7758212742bf7a3e/zh_CN/data-integration/rule-sql-builtin-functions.md#Time and date functions). 
-  Nested objects are allowed, when value is a JSON object:
    - It is not allowed to use `"` to nest the value in the template, otherwise, it will cause an execution error;
    - Objects will be nested and stored according to their own structure;
    - To store objects as JSON characters, use the `json_encode` function in rule SQL for the conversion, and the corresponding **value** in the template is still not allowed to be wrapped with `"`. :::

1. Advanced settings (optional):  Choose whether to use sync or async query mode as needed. For details, see [Configuration parameters](#Configuration).
2. Then click **Create** to finish the creation of the data bridge. A confirmation dialog will appear and ask if you like to create a rule using this data bridge, you can click **Create Rule** or **Data Integration** -> **Rules **on EMQX dashboard to configure rules.

#### Create rules

1. Click **Create** on the top right corner of the page.
2. Input `my_rule` as the rule ID, and set the rules in the **SQL Editor**. Here we want to save the MQTT messages under topic `t/#`  to PostgreSQL, we can use the SQL syntax below. Note: If you are testing with your SQL, please ensure you have included all required fields in the `SELECT` part. 

```
SELECT
  *
FROM
  "t/#"
```

You can use the SQL below to save `timestamp` as data type and the `payload`  in JSON as JSON strings:

```
SELECT
  *,
  mongo_date(timestamp) as timestamp,
  json_decode(payload) as payload
FROM
  "t/#"
```

3. Click the **Add Action** button, select **Forwarding with Data Bridge** from the dropdown list and then select the data bridge we just created under **Data bridge**. Then, click the **Add** button. 

4. Click the **Create** button to finish the setup. 

Now you can go to **Data Integration** -> **Flows** to view the topology. Messages under topic `t/#` are first processed by rule  `my_rule`  and then saved in MongoDB. 

### Test

Use MQTTX  to send a message to topic  `t/1`:

```
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello MongoDB" }'
```

Check the running status of the two data bridges, there should be one new **Matched** and one new **Sent Successfully** message.

Check whether the message is written into collection `emqx_messages`:

```
> db.emqx_messages.find().pretty()
{
    "_id" : ObjectId("63db7059df489d01ed000009"),
    "clientid" : "emqx_c",
    "payload" : {
      "msg" : "hello MongoDB"
    },
    "qos" : 0,
    "timestamp" : NumberLong("1675325529070"),
    "topic" : "t/1"
}
```

Under the second rule SQL, the returned information should be: 

```
> db.emqx_messages.find().pretty()
{
    "_id" : ObjectId("63db7535df489d01ed000013"),
    "clientid" : "emqx_c",
    "payload" : "{ \"msg\": \"hello MongoDB\" }",
    "qos" : 0,
    "timestamp" : ISODate("2023-02-02T08:33:36.715Z"),
    "topic" : "t/1"
}
```
