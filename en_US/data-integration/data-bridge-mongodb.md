# Ingest MQTT Data into MongoDB

{% emqxce %}
:::tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}

EMQX supports integration with MongoDB so you can save MQTT messages and client events to MongoDB.

<!--As the beginning paragraph, you should provide a brief introduction to the external database or data processing services, and also describe what the data integration can do, such as streaming data, real-time analysis, series data storage, etc. Optionally, you can add a high-level picture, indicating if the data integration is single-directional or bi-directional, producing data or writing data into the database.-->

## How It Works

<!-- In this section, you explain how the data integration works together with the rule engine by introducing the architecture, emphasizing the simplicity and no coding required….You can also combine the working principle with a real-life use case. Some of the typical scenarios include connected vehicles, IIoT, power and engergy, and etc. You can describe where the data will be used evantually and what value they can bring to the business under the specific scenario. Use sequential steps to describe how data flows from devices to the data integration, and then to the data storage…-->

## Features and Benefits



## Befor You Start

This section describe the preparations you need to complete before you start to create the MongoDB data bridges in EMQX Dashboard.

### Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridge](./data-bridges.md)

### Install MongoDB Server

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

```

### Create a Database

You can use the following command to create a database in MongDB:

```
# Create database emqx_data
use emqx_data

# create collection emqx_messages
db.createCollection('emqx_messages')
```

## Create Rule and MongoDB Data Bridge

1. Go to EMQX Dashboard, click **Integration** -> **Rules**.

2. Click **Create** on the top right corner of the page.

3. Input `my_rule` as the rule ID, and set the rules in the **SQL Editor**. Here we want to save the MQTT messages under topic `t/#`  to MongoDB, we can use the SQL syntax below. 

   Note: If you want to specify your own SQL syntax, make sure that you have included all fields required by the data bridge in the `SELECT` part.

```
SELECT
  *
FROM
  "t/#"
```

You can use the SQL syntax below to save `timestamp` as data type and the `payload`  in JSON as JSON strings:

```
SELECT
  *,
  mongo_date(timestamp) as timestamp,
  json_encode(payload) as payload
FROM
  "t/#"
```

Note: If you are a beginner user, you can click **SQL Examples** and **Enable Test** to learn and test the SQL rule. 

4. Click the + **Add Action** button to define an action that will be triggered by the rule. Select **Forwarding with Data Bridge** from the dropdown list. With this action, EMQX sends the data processed by the rule to MongoDB.
5. Click the **+** icon next to the **Data bridge** drop-down box to create a data bridge.
6. Select **MongoDB** from the **Type of Data Bridge** drop-down list. Fill in the required fields (marked with an asterisk).
7. Enter a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.
8. Set **MongoDB Mode**  and **Srv Record** as your business needs, for example, **single** and the default deselected status.
9. Configure the MongoDB connection information. Input `emqx_data` as the **Database Name**, `127.0.0.1:27017` as the **Server Host**, `admin` as the **Username**, `public` as the **Password**, and `emqx_messages` as **Collection to be used**. For the other fields, you can keep the default setting. 
10. Configure the **Payload template** to save `clientid`, `topic`, `qos`,  `timestamp`, and `payload` to MongoDB. This template will be executed via the MongoDB insert command, and the sample code is as follows:

```json
{
  "clientid": "${clientid}",
  "topic": "${topic}",
  "qos": ${qos},
  "timestamp": ${timestamp},
  "payload": ${payload}
}
```

::: tip Notes when configuring the Payload template:

- All `keys` need to be wrapped in double quotes `"`;
- Auto-derivation of the data type of "value" is not supported:
  - Characters need to be wrapped with `"`, otherwise, an error will be reported;
  - Values do not need to be wrapped, otherwise, they will be recognized as characters;
  - For timestamp, date, and time types, if no special treatment is performed, they will be recognized as numeric or character types. To store them as date or time, use the `mongo_date` function in the rule SQL to process the fields. For details, see [Time and date functions](./rule-sql-builtin-functions.md#time-and-date-functions). 

- Nested objects are allowed, when value is a JSON object:
  - It is not allowed to use `"` to nest the value in the template, otherwise, it will cause an execution error;
  - Objects will be nested and stored according to their own structure;

- To store objects as JSON characters, use the `json_encode` function in rule SQL for the conversion, and the corresponding **value** in the template is still not allowed to be wrapped with `"`. 

:::

11. Advanced settings (optional): See [Advanced Configurations](#advanced-configurations).

12. Before clicking **Create**, you can click **Test Connectivity** to test that the bridge can connect to the MongoDB server.

13. Click the **Create** button to finish the setup.

Now the MongoDB data bridge should appear in the data bridge list (**Integration** -> **Data Bridge**) with **Resource Status** as **Connected**.

## Test MongoDB Data Bridge and Rule

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

## Advanced Configurations

<!--Provide users with some advanced operations so that they can optimize the configurations to be more suitable to their business needs, including parameters under speical scenarios, such as high throughput, lower lentancy, large payload size, also some advanced optional parameters that affect operation, such as buffered queue, sync or async query mode, and batch mode.-->

<!--In the descriptions, we need to provide detailed information about what this option is used for, how to use this option for optimization, what’s the default value and optional values, and etc.-->

This section describes some advanced configurations options that can optimize the performance of your data bridge and customize the operation based on your specific using scenarios. When creating the data bridge, unfold **Advanced Settings** and you can configure the following settings according to your business needs.

| **Fields**               | **Descriptions** | Recommend Value? |
| ------------------------ | ---------------- | ---------------- |
| Write Mode               |                  |                  |
| Max Overflow Workers     |                  |                  |
| Overflow TTL             |                  |                  |
| Overflow Check Period    |                  |                  |
| Local Threshold          |                  |                  |
| Connect Timeout          |                  |                  |
| Socket Timeout           |                  |                  |
| Server Selection Timeout |                  |                  |
| Wait Queue Timeout       |                  |                  |
| Heartbeat Period         |                  |                  |
| Minimum Heartbeat Period |                  |                  |
| Connection Pool Size     |                  |                  |
| Start Timeout            |                  |                  |
| Buffer Pool Size         |                  |                  |
| Request TTL              |                  |                  |
| Health Check Interval    |                  |                  |
| Max Buffer Queue Size    |                  |                  |
| Query Mode               |                  |                  |
| Inflight Window          |                  |                  |

## More Information

EMQX provides bunches of learning resources on the data integration with MongoDB. Check out the following links to learn more:

**Blogs:**

- 

**Videos:**

- 
