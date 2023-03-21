# Rule Engine

The Rules Engine is EMQX's built-in data processing component, which works with [data-bridges](./data-bridges.md) and can be used to help extract, filter, enrich, transform and store IoT data to accelerate application integration and business innovation. The rules are partly specified with a domain-specific SQL-like language. For simplicity we call this language SQL from here on.

![image](./assets/rules/data-integration-arch.png)

## Composition of Rules

The rules describe **data source**, **data transformation**, and **processing result destination**:

- **data source**: the data source of a rule can be a message or event, or an external data system. Rules specify the source of data with the FROM clause in the SQL for the rule. The WHERE clause of the SQL is used to add additional constraints on which messages will be processed by the rule.

- **data transformation**: describes the transformation of an input message. The SELECT-part of the SQL is used to extract and transform data from the input message. The SELECT part can make use of built-in functions to do advanced transformations and add data such as time stamp to the output message.

- **processing result destination**: a rule can define one or more actions to process the SQL execution results. If the SQL execution passes (that is, does not produce an error), the rules will perform corresponding actions in sequence, such as storing the processing results in the database or republishing them to another MQTT topic.

![sql_process](./assets/sql_process.png)

### Introduction to the Rule-engine Language

The data **data source** and **data transformation** part of a rule is specified by a domain specific language. As most programs in this language looks similar to an SQL statement, we call this language SQL. We call a program written in this language an SQL statement. An SQL statement is used to specify the data source of rules, and the transformation of the input message. An example of an SQL statement is given below:

```SQL
SELECT
    payload.data as d
FROM
    "t/#"
WHERE
    clientid = "foo"
```

The above SQL statement specifies the following for the rule:

- **data source**: the messages with topic `t/#` where the client ID is `foo`.
- **data transformation**: The rule engine assumes that the payload is structured data (such as JSON, avro, and protobuf). The `payload.data` syntax is used to extract the `data` field from the payload. The `as` keyword is used to assign the extracted value to a new field name `d`. So in the above example the output data will have the following structure `{d: "value of the payload's data field"}`.

::: tip
The dot (".") syntax requires that the data is a structured format that EMQX supports. If the payload is formatted in some other way, SQL functions (such as the jq function) must be used for data type conversion.
:::

For more details about the SQL syntax and usages, see [SQL syntax](./rule-sql-syntax.md).

### Actions

Actions are components used to process the output of the rule's SQL statement and determine the final destination of the data.

Currently rules supports the following types of actions:

* **Republish** - that republish the message to a new topic.
* **Console Output** - that prints the output of the rule to the console. This is mainly intended for debugging purposes.
- **Forwarding with Data Bridges** - A data bridge is a channel to an external service (typically a database, web service or message queue). Rules can directly use the ID of data bridge as the action. Please see the [documentation for data bridges](./data-bridges.md) for more details about how to uses bridges.


## The Republish Action


The republish action is used to publish a new MQTT message. For example, this can be useful when one wants to send an error message back to a device. 

::: tip
The republish action does not prevent the delivery of the original message. For example, if a "a/1" message triggers a "republish" action through a rule and sends a new message "a/2", then the "a/1" message will still be delivered to the clients subscribed to the topic.
:::

In the republish action, you can customize the payload, topic, QoS and other parameters of the message,
and fields in the rule output can be referenced as values of these parameters in the form of `${field name}`.

## The Console Action

The console output action is used to print the result message of a rule to the console or log file

* If the emqx is started with `emqx console`, the results will be printed to the terminal where `emqx console` was invoked.
* If the emqx is started with `emqx start`, the results will be printed to a log file (`erlang log.*`) under the log dir of EMQX.

### Output Format

The first line of the output will contain the `[rule action]` header followed by the rule ID.
The rest of the output is divided into two parts:

- The `Action Data` section is the output result of the rule. The fields contained in `Action Data` can be referenced in the form of `${field name}` in the action parameters.

- The `Envs` is the environment variable information available for the action. The environment variable information includes all available fields of the data source and other internal information related to the execution of this action.

Example of the outputs:

```bash
[rule action] rule_id1
    Action Data: #{key1 => val1}
    Envs: #{key1 => val1, key2 => val2}
```

::: tip
The console output action should only be used for debugging. If it is used in the production environment, it may cause performance problems.
:::

## Typical Use Cases of Rules

### Action Monitoring

Rules can be used to monitor the status of clients. For example, if a client representing a door in a smart home is disconnected, a rule can be used to send a notification to dashboard for the smart home.

### Data Filtering

In the truck fleet management of the Internet of vehicles, vehicle sensors collect and report a large amount of operation data.
The application platform only focuses on the data when the vehicle speed is greater than 40 km/h.
In this scenario, rules can be used to filter messages conditionally and only write data that meets the conditions to a message queue.

### Message Routing

Rules can be used to route messages to different topics based on the content of
the message. For example, a rule can be used to route messages containing a
temperature greater than 100 degrees to a topic for high temperature alarms. 

### Message Encoding and Decoding

Rules can be used to encode and decode messages. For example, a rule can be used to decode a message containing a binary format into a JSON format, and then forward the JSON format message to the application platform for further processing. The rule engine SQL language comes with many built-in functions that can be used for encoding and decoding (the jq function is especially powerful). If the built-in functionality is not enough for the encoding or decoding task, one can also add new built-in functions that are implemented in Erlang.


