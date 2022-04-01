# Rule Engine

EMQX Broker Rule Engine (Hereinafter referred to as rule engine) is used to configure EMQX Broker message flow and device event processing and response rules. The rule engine not only provides a clear and flexible "configuration-style" business integration solution, simplifies the business development process, improves ease of use for user, and reduces the coupling between the business system and EMQX Broker, but also provides a better infrastructure for the private function customization of EMQX broker.

![image-20190506171815028](../assets/image-20190506171815028.jpg)

EMQX Broker will trigger the rule engine when **publishing message or triggering  event**, and the rules that meet the triggering conditions will execute their own SQL statements to filter and process the context information of messages and events.

::: tip
Applicable version:**EMQX Broker v3.1.0+**

Compatibility Tip: EMQX Broker v4.0 makes major adjustments to the SQL syntax of the rule engine. For v3.x upgrade users, please refer to  [Migration Guide](./rule-engine.md#migration-guide) for compatibility.
:::

## Publish message

The rule engine can store the message processing results of a specific topic to the database with the response action, send it to the HTTP server, forward it to the message queue of Kafka or RabbitMQ, and republish to a new topic or even another Broker cluster. Each rule can be configured with multiple response actions.

Select the message published to the t/# topic and filter out all fields:

```sql
SELECT * FROM "t/#"
```

Select the message posted to the t/a topic, and filter out the "x" field from the message content in JSON format:

```sql
SELECT payload.x as x FROM "t/a"
```

## Event trigger

The rule engine uses a virtual topic beginning with **$events/** to process EMQX Broker built-in events. The built-in events provide finer message control and client action processing capabilities, which can be used in the business of QoS 1 QoS 2 messages arrival recording, device online and offline recording.

Select the client connection event, filter the device whose Username is `emqx` and obtain the connection information:

```sql
SELECT clientid, connected_at FROM "$events/client_connected" WHERE username = 'emqx'
```

For rule engine data, SQL statement format and [event topic](./rule-engine_field.md#event-topic-available-for-from-clause) list, please refer to [SQL manual](./rule-engine_grammar_and_examples.md#rule-engine-sql-statement) for detailed tutorials.

## Rule engine composition

The rule describes the three configurations of **where data comes from,  how to filter and process data, and where processed results go to**, which means an available rule contains three elements:

- Triggered event: The rule is triggered by an event. When triggered, the event inputs the context information (data source) of the event into the rule, and the event type is specified through the FROM clause of SQL;
- Processing rules (SQL): Use SELECT clause and WHERE clause and built-in processing functions to filter and process data from context information;
- Response action: If there is a processing result output, the rule will perform the corresponding action, such as persisting to the database, republishing the processed message, forwarding the message to the message queue, etc. A rule can configure multiple response actions.

The following figure is a simple rule, which is used to process the data at the time of **message publishing**, filter out the `msg` field,  messages `topic`, ` qos` of all topic messages, and send them to the Web Server and /uplink topics:

![image-20190604103907875](../assets/image-20190604103907875.png)

EMQX Broker's rule engine can be used to flexibly process messages and events. By using the rule engine, it can easily achieve such function as converting the message into a specified format, and then stored in a database table, or sent to the message queue.

The concepts related to the EMQX Broker rule engine include: rules, actions, resources, and resource-types.

The relationship between rules, actions and resources:
```
Rule: {
    SQL statement
    Action list: [
        {
            action 1,
            Action parameters,
            Bind resources: {
                Resource configuration
            }
        },
        {
            action 2,
            Action parameters,
            Bind resources:  {
                Resource configuration
            }
        }
    ]
}
```
- Rule: Rule consists of SQL statements and action list. The action list contains one or more actions and their parameters.
- SQL statements are used to filter or transform data in messages.
- The action is the task performed after the SQL statement is matched, which defines an operation for data.
  Actions can be bound to resources or unbound. For example, the "inspect" action does not require binding resources, which simply prints the data content and action parameters. The "data_to_webserver" action needs to bind a web_hook type resource, and a URL is configured in this resource.
- Resource: A resource is an object instantiated through a resource type as a template, and saves the configuration related to the resource (such as database connection address and port, user name and password, etc.).
- Resource Type: Resource type is a static definition of a resource and describes the configuration items required for this type of resource.

::: tip
Actions and resource types are provided by emqx or plugin code and cannot be created dynamically through API and CLI.
:::

## SQL statement 
### SQL syntax
**FROM, SELECT, and WHERE clauses:**

The basic format of the SQL statement of the rule engine is:
```sql
SELECT <fields> FROM <topic> [WHERE <any>]
```
- The `FROM` clause mounts rules to a topic
- The `SELECT` clause is used to select fields in the output
- The `WHERE` clause is used to filter messages based on conditions

**FOREACH, DO and INCASE clauses:**

If you want to perform some operations and actions for each element of an array data, you need to use the `FOREACH-DO-INCASE` syntax. The basic format is:

```sql
FOREACH <Field name> [DO <Condition>] [INCASE <Condition>] FROM <Topic> [WHERE <Condition>]
```

- The `FOREACH` clause is used to select the field that needs to perform foreach operation. Note that the selected field must be an array type
- The `DO` clause is used to transform each element in the array selected by FOREACH and select the field of interest
- The `INCASE` clause is used to apply conditional filtering to a field selected by DO

The DO and INCASE clauses are optional. DO is equivalent to the SELECT clause for objects in the current loop, while INCASE is equivalent to the WHERE statement for objects in the current loop.

## Examples of typical application scenarios for rule engine

- Action listening: In the development of intelligent door lock for smart home, the function of the door lock will be abnormal because of offline resulting by the network or power failure, man-made damage and other reasons. Through using rule engine configuration to monitor offline events, it can push the fault information to the application service and realize the ability of first time fault detection in the access layer.
- Data filtering: Truck fleet management of vehicle network. Vehicle sensors collect and report a large amount of operational data. The application platform only focuses on data with a vehicle speed greater than 40 km/h. In this scenario, the rule engine can be used to conditionally filter messages to the service, and data that satisfies the condition can be written to the business message queue .
- Message routing: In the intelligent billing application, the terminal device distinguishes the service type by different topics. The message of billing service can be connected to the billing message queue by configuring the rule engine, and the non-billing information can be connected to other message queues to realize the routing configuration of business messages.
- Message encoding and decoding: In the application scenarios such as public protocol/proprietary TCP protocol access and industrial control, the encoding and decoding of binary/special format message body can be done through the local processing function of the rule engine (which can be customized and developed on EMQX). Relevant messages can also be routed through the rule engine to external computing resources such as function computing for processing (processing logic can be developed by users), and the messages can be converted into JSON format that is easy for business processing, which simplifies the difficulty of project integration and improves the ability of rapid development and delivery of applications.

The topic of the event message starts with `"$events/"`, such as `"$events/client_connected",` `"$events/session_subscribed"`.
If you want emqx to publish the event message, you can configure it in the `emqx_rule_engine.conf` file.

For all supported events and available fields, please see [Event topics available for FROM clause](#event-topic-available-for-from-clause).

### SQL statement example: 
**Basic syntax examples**

-  Extract all fields from the messages with a topic of "t/a": 
    ```sql
    SELECT * FROM "t/a"
    ```
-  Extract all fields from the messages with a topic of "t/a" or "t/b": 
    ```sql
    SELECT * FROM "t/a","t/b"
    ```
-  Extract all fields from the message with a topic that can match 't/#'. 
    ```sql
    SELECT * FROM "t/#"
    ```
-  Extract the qos, username, and clientid fields from the message with a topic that can match 't/#' :
    ```sql
    SELECT qos, username, clientid FROM "t/#"
    ```
-  Extract the username field from any topic message with the filter criteria of username = 'Steven':
    ```sql
    SELECT username FROM "#" WHERE username='Steven'
    ```
- Extract the x field from the payload of message with any topic and create the alias x for use in the WHERE clause. The WHERE clause is restricted as x = 1. Note that the payload must be in JSON format. Example: This SQL statement can match the payload `{"x": 1}`, but can not match to the payload `{"x": 2}`:
    ```sql
  SELECT payload FROM "#" WHERE payload.x = 1
  ```
- Similar to the SQL statement above, but nested extract the data in the payload, this SQL statement can match the payload{"x": {"y": 1}}`:
    ```sql
    SELECT payload FROM "#" WHERE payload.x.y = 1
    ```
-  Find the connection where clientid = 'c1', extract its source IP address and port number:
    ```sql
    SELECT peername as ip_port FROM "$events/client_connected" WHERE clientid = 'c1'
    ```
- Filter all clientids that subscribe to the 't/#' topic and have a subscription level of QoS1 :
    ```sql
    SELECT clientid FROM "$events/session_subscribed" WHERE topic = 't/#' and qos = 1
    ```
- Filter all clientids that subscribe to the 't/#' topic and subscription level is QoS1. Note that the strict equality operator '=~' is used here, so it does not match subscription requests with the topic 't' or 't/+/a' :
    ```sql
    SELECT clientid FROM "$events/session_subscribed" WHERE topic =~ 't/#' and qos = 1
    ```

::: tip
- Topic after the FROM clause need to be enclosed in double quotes `""`.
- The WHERE clause is followed by the filter condition. If a string is used, it needs to be enclosed in single quotes `'' `.
- If there are multiple topics in the FROM clause, they need to be separated by commas `","`. For example,
    ```sql
    SELECT * FROM "t/1", "t/2".
    ```
- You can use the `"." `Symbol to nest select payloads
- If possible, don't create alias for payload, as this would cause performance degradations.
  i.e. Do not use `SELECT payload as p`
:::

#### Examples of FOREACH-DO-INCASE

Suppose there is a message with ClientID of `c_steve` and topic of ` t/1`. The message body is in JSON format, and the sensors field is an array containing multiple Objects:

```json
{
    "date": "2020-04-24",
    "sensors": [
        {"name": "a", "idx":0},
        {"name": "b", "idx":1},
        {"name": "c", "idx":2}
    ]
}
```

**Example 1: It is required that each object in sensors is re-published as a data input to the topic of `sensors/${idx}` with the content of `${name}`. That means the final rule engine will issue 3 messages:** 

1) Topic: sensors/0
   Content: a
2) Topic: sensors/1
   Content: b
3) Topic: sensors/2
   Content: c

To complete this rule, we need to configure the following actions:

- Action type: message republish
- Target topic: sensors/$ {idx}
- Target QoS: 0
- Message content template: $ {name}

And the following SQL statement:

```sql
FOREACH
    payload.sensors
FROM "t/#"
```

**Example analysis: **

In this SQL, the FOREACH clause specifies the array sensors that need to be traversed, then the selection result is:

```json
[
  {
    "name": "a",
    "idx": 0
  },
  {
    "name": "b",
    "idx": 1
  },
  {
    "name": "c",
    "idx": 2
  }
]
```

The FOREACH statement will perform a "message republish" action for each object in the result array, so the republish action will be performed 3 times.

**Example 2: It is required that each object in sensors with ids value greater than or equal to 1 is re-published as a data input to the topic of `sensors/${idx}` with the content of  `clientid=${clientid},name=${name},date=${date}`. That means the final rule engine will issue 2 messages:** 

1) Topic: sensors/1
   Content: clientid=c_steve,name=b,date=2020-04-24
2) Topic: sensors/2
   Content: clientid=c_steve,name=c,date=2020-04-24

To complete this rule, we need to configure the following actions:

- Action type: message republish
- Target topic: sensors/$ {idx}
- Target QoS: 0
- Message content template: clientid=${clientid},name=${name},date=${date}

And the following SQL statement:

```sql
FOREACH
    payload.sensors
DO
    clientid,
    item.name as name,
    item.idx as idx
INCASE
    item.idx >= 1
FROM "t/#"
```

**Example analysis: **

In this SQL, the FOREACH clause specifies the array `sensors` that need to be traversed; the DO clause selects the fields required for each operation, and we select the outer clientid field here, and the two fields of `name` and `idx` of the current sensor object. Note that  item  represents the object of this loop in the sensors array. The INCASE clause is a filtering condition for the fields in the DO statement, only if idx> = 1 meets the condition. So the selection result of SQL is:

```json
[
  {
    "name": "b",
    "idx": 1,
    "clientid": "c_emqx"
  },
  {
    "name": "c",
    "idx": 2,
    "clientid": "c_emqx"
  }
]
```

The FOREACH statement will perform a "message republish" action for each object in the result array, so the republish action will be performed twice.

In DO and INCASE statements, you can use `item` to access the object of the current loop, or you can customize a variable name by using the `as` syntax in FOREACH. So the SQL statement in this example can be written as:

- Action listening: In the development of intelligent door lock for smart home, the function of the door lock will be abnormal because of offline resulting by the network or power failure, man-made damage and other reasons. Through using rule engine configuration to monitor offline events, it can push the fault information to the application service and realize the ability of first time fault detection in the access layer.
- Data filtering: Truck fleet management of vehicle network. Vehicle sensors collect and report a large amount of operational data. The application platform only focuses on data with a vehicle speed greater than 40 km/h. In this scenario, the rule engine can be used to conditionally filter messages to the service, and data that satisfies the condition can be written to the business message queue .
- Message routing: In the intelligent billing application, the terminal device distinguishes the service type by different topics. The message of billing service can be connected to the billing message queue by configuring the rule engine, and the non-billing information can be connected to other message queues to realize the routing configuration of business messages.
- Message encoding and decoding: In the application scenarios such as public protocol/proprietary TCP protocol access and industrial control, the encoding and decoding of binary/special format message body can be done through the local processing function of the rule engine (which can be customized and developed on EMQX). Relevant messages can also be routed through the rule engine to external computing resources such as function computing for processing (processing logic can be developed by users), and the messages can be converted into JSON format that is easy for business processing, which simplifies the difficulty of project integration and improves the ability of rapid development and delivery of applications.

### Test SQL statements in Dashboard
The SQL statement test function is provided in the Dashboard interface, and the SQL test results are shown through the given SQL statement and event parameters.

1.  On the rule creating interface, enter **rule SQL** and enable the **SQL test** switch:

    ![image](../assets/sql-test-1@2x.png)

2. Modify the field of the simulated event, or use the default configuration, and click the **Test** button:

   ![image](../assets/sql-test-2@2x.png)

3. The result of SQL processing will be displayed in the **Test Output** text box:

   ![image](../assets/sql-test-3@2x.png)

## Migration Guide

In version 4.0, the SQL syntax of the rule engine is easier to use. In version 3. X, the event name needs to be specified after the **FROM** clause. After 4.0 version, we introduce the concept of **event topic** . By default, the **message publish** event no longer needs to be specified. 

```sql
## 3.x
## Event name needs to be specified for processing
SELECT * FROM "message.publish" WHERE topic =~ 't/#'

## 4.0 and later
## The message.publish event is processed by default, and MQTT topics are filtered directly after FROM
## The above SQL is equivalent to:
SELECT * FROM 't/#'

## Other events are filtered by event topics
SELECT * FROM "$events/message_acked" where topic =~ 't/#'
SELECT * FROM "$events/client_connected"
```

::: tip
The old version of SQL syntax conversion function is provided in Dashboard to complete SQL upgrade and migration. 
:::

[Rule engine statement and examples](rule-engine_grammar_and_examples.md)
