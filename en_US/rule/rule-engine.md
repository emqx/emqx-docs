# Rule Engine

EMQX Rule Engine is used to configure EMQX message flow and device event processing and response rules. The rule engine not only provides a clear and flexible "configuration-style" business integration solution, simplifies the business development process, improves ease of use for users, and reduces the coupling between the business system and EMQX, but also provides a better infrastructure for the private function customization of EMQX.

<img src="../assets/image-20190506171815028.jpg" alt="image-20190506171815028" style="zoom:50%;" />

Rule engine will be triggered when **publishing messages or by events**, and the rules that meet the triggering conditions will execute their own SQL statements to filter and process the context information of messages and events.

:::tip
Applicable version: **EMQX v3.1.0+**

Compatibility Tip: EMQX v4.0 makes major adjustments to the SQL syntax of the rule engine. For v3.x upgrade users, please refer to  [Migration Guide](./rule-engine.md#migration-guide) for compatibility guidance.
:::

## Publish Messages

The rule engine can store the message processing results of a specific topic in the database with the response action, send it to the HTTP server, forward it to the message queue of Kafka or RabbitMQ, and republish it to a new topic or even another Broker cluster. Each rule can be configured with multiple response actions.

Select the message published to the t/# topic and filter out all fields:

```sql
SELECT * FROM "t/#"
```

Select the message posted to the t/a topic, and filter out the "x" field from the message content in JSON format:

```sql
SELECT payload.x as x FROM "t/a"
```

## Event

The rule engine uses a virtual topic beginning with **$events/** to process EMQX built-in events. The built-in events provide finer message control and client action processing capabilities, which can be used in the business of QoS 1 and QoS 2 messages arrival recording, and device online and offline recording.

Select the client connection event, filter the device whose Username is `emqx` and obtain the connection information:

```sql
SELECT clientid, connected_at FROM "$events/client_connected" WHERE username = 'emqx'
```

For rule engine data, SQL statement format and [event topic](./rule-engine_field.md#event-topic-available-for-from-clause) list, please refer to [SQL manual](./rule-engine_grammar_and_examples.md#rule-engine-sql-statement) for detailed tutorials.

## How the Rule Engine Works

The rule describes the three configurations of **where data comes from, how to filter and process data, and where processed results go to**, which means an available rule contains three elements:

- Triggered event: The rule is triggered by an event. When triggered, the event inputs the context information (data source) of the event into the rule, and the event type is specified through the FROM clause of SQL;
- Processing rules (SQL): Use SELECT clause and WHERE clause and built-in processing functions to filter and process data from context information;
- Response action: If there is a processing result output, the rule will perform the corresponding action, such as persisting to the database, republishing the processed message, forwarding the message to the message queue, etc. A rule can configure multiple response actions.

EMQX rule engine can be used to flexibly process messages and events. By using the rule engine, it can easily achieve such functions as converting the message into a specified format, and then stored in a database table, or sent to the message queue.

The relationship between rules, actions, and resources:
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
- Rule: The rule consists of SQL statements and an action list. The action list contains one or more actions and their parameters.
- SQL statements are used to filter or transform data in messages.
- The action is the task performed after the SQL statement is matched, which defines an operation for data.
  Actions can be bound to resources or unbound. For example, the "inspect" action does not require binding resources, which simply prints the data content and action parameters. The "data_to_webserver" action needs to bind a web_hook type resource, and a URL is configured in this resource.
- Resource: A resource is an object instantiated through a resource type as a template, and saves the configuration related to the resource (such as database connection address and port, user name and password, etc.).
- Resource Type: Resource type is a static definition of a resource and describes the configuration items required for this type of resource.

:::tip
Actions and resource types are provided by EMQX or plugin code and cannot be created dynamically through API and CLI.
:::

### Test SQL Statements in Dashboard
The SQL statement test function is provided in the Dashboard interface, and the SQL test results are shown through the given SQL statement and event parameters.

1.  On the rule creating interface, enter **rule SQL** and enable the **SQL test** switch:

    <img src="./assets/SQL-test.png" alt="SQL-test" style="zoom:50%;" />

2. Modify the field of the simulated event, or use the default configuration, and click the **Test** button:

3. The result of SQL processing will be displayed in the **Test Output** text box:


## Migration Guide

In version 4.0, the SQL syntax of the rule engine is easier to use. In version 3. X, the event name needs to be specified after the **FROM** clause. After 4.0 version, we introduce the concept of **event topic**. By default, the **message publish** event no longer needs to be specified. 

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

:::tip
The old version of SQL syntax conversion function is provided in Dashboard to complete SQL upgrade and migration. 
:::

[Rule engine statement and examples](./rule-engine_grammar_and_examples.md)
