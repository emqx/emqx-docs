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

## Examples of typical application scenarios for rule engine

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

[Rule engine statement and examples](./rule-engine_grammar_and_examples.md)
