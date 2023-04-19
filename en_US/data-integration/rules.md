# EMQX Rules

EMQX equips our uses with the Rules feature for data processing, which works with [data bridges](./data-bridges.md) and can be used to help extract, filter, enrich, transform, and store IoT data to accelerate application integration and business innovation. 

<img src="./assets/rule-engine.png" alt="SQL-based IoT Rule Engine" style="zoom:30%;" />

EMQX rules are particularly useful when you need to transform or reroute incoming messages, for example, you can create rules that filter out irrelevant data, do transformations, and trigger alerts or notifications based on specific events or conditions.

This chapter provides an in-depth exploration of the EMQX Rule and its capabilities. 

## How the Rule Engine Works

Rules describe how to retrieve data from a **data source**, perform **data transformations**, and what actions should be applied to the results.

<img src="./assets/sql_process.png" alt="sql_process" style="zoom:50%;" />

- **Data Source**: The data source of a rule can be a message, event, or an external data system. You can use the `FROM` clause in the rule's SQL to specify the data source, and then use the `WHERE` clause to add additional constraints on which messages will be processed by the rule. 

  For details about different types of data sources supported and the fields that can be refered in the `WHERE` clause, see [Data Sources and Fields](./rule-sql-events-and-fields.md).

- **Data Transformation**: Data transformations describe how to transform an input message. You can use the `SELECT` part of the SQL to extract and transform data from the input message. You can use the embedded SQL sample statements to implement advanced transformations, for example, to add a time stamp to the output message. 

  For a detailed explanation about the syntax and the built-in SQL functions, see [Rule Syntax](./rule-sql-syntax.md) and [Built-in SQL Functions](./rule-sql-builtin-functions.md).

- **Actions**: After the input is processed as per the rules specified, you can continue to define one or more actions to process the SQL execution results. The Rule Engine will perform corresponding actions in sequence, such as storing the processing results in a database or republishing them to another MQTT topic.

  For a step-by-step guide on how to create a rule with EMQX dashboard, see [Create Rules](./rule-get-started).

## Typical Use Cases

### Data Filtering

In certain scenarios, only specific types of data are relevant, and processing all incoming data may not be necessary. 

For example, in truck fleet management, vehicle sensors collect and report a large amount of operation data. However, the application platform may only be interested in data when the vehicle speed is greater than 40 km/h. 

In this case, rules can be used to filter messages conditionally and only write data that meets the conditions to a message queue. This can help optimize system performance and reduce storage requirements.

### Message Routing

Rules can also be used to route messages to different topics based on the content of the message. This allows for more fine-grained control over message distribution. 

For example, a rule can be used to route messages containing a temperature greater than 100 degrees to a topic for high-temperature alarms. This can help ensure that messages are delivered to the appropriate destination for further processing.

### Message Encoding and Decoding

For cases where the message format needs to be changed, rules can be used to encode and decode messages as required.

For example, a rule can be used to decode a message containing binary data into a JSON format. 

If the built-in functionality is not enough for a specific encoding or decoding task, new built-in functions can be added that are implemented in Erlang. This provides great flexibility in handling message formats.

### Action Monitoring

Rules can be used to monitor the status of clients connected to the broker. 

For example, if a client representing a door in a smart home is disconnected, a rule can be used to send a notification to a dashboard for the smart home. This helps ensure that important events are promptly acted upon.
