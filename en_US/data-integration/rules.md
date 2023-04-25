# EMQX Rules

EMQX provides users with the Rules feature for data processing, which works in conjunction with [data bridges](https://chat.openai.com/data-bridges.md) to extract, filter, enrich, transform, and store IoT data. This accelerates application integration and drives business innovation.

<img src="./assets/rule-engine.png" alt="SQL-based IoT Rule Engine" style="zoom:30%;" />

EMQX rules are especially useful for transforming or rerouting incoming messages. For instance, you can create rules that filter out irrelevant data, perform transformations, and trigger alerts or notifications based on specific events or conditions.

This chapter provides an in-depth exploration of the EMQX Rules and their capabilities. 

## How the Rule Engine Works

Rules specify how to retrieve data from a **data source**, perform **data transformations**, and the **actions** that should be applied to the results.

<img src="./assets/sql_process.png" alt="sql_process" style="zoom:50%;" />

- **Data Source**: The data source of a rule can be a message, event, or external data system. The `FROM` clause in the rule's SQL specifies the data source, while the `WHERE` clause adds additional constraints on which messages the rule processes.

  For more information on the various types of supported data sources and fields that can be referenced in the `WHERE` clause, see [Data Sources and Fields](https://chat.openai.com/rule-sql-events-and-fields.md).

- **Data Transformation**: Data transformations describe the process of transforming an input message. The `SELECT` part of the SQL extracts and transforms data from the input message. Embedded SQL sample statements can be used to implement advanced transformations, such as adding a timestamp to the output message.

  For a detailed explanation of the syntax and built-in SQL functions, see [Rule Syntax](https://chat.openai.com/rule-sql-syntax.md) and [Built-in SQL Functions](https://chat.openai.com/rule-sql-builtin-functions.md).

- **Actions**: After the input is processed according to the specified rules, one or more actions can be defined to process the SQL execution results. The Rule Engine will sequentially perform corresponding actions, such as storing the processing results in a database or republishing them to another MQTT topic.

  For a step-by-step guide on creating a rule with the EMQX dashboard, see [Create Rules](https://chat.openai.com/rule-get-started).

## Key Benefits

EMQX's rule feature offers users the following benefits:

**Simplified data processing**

The SQL-like syntax and stream processing capabilities of the rule engine streamline filtering, transforming, and distributing data without the need for custom code or additional tools.

**Real-time insights and actions**

By triggering actions based on specific conditions, the Rule Engine empowers users to gain real-time insights and take appropriate actions.

**Reduced development time and effort**

The Rule Engine eases IoT application development by providing an extensive range of built-in capabilities, minimizing the need for custom code and maintenance efforts.

**Scalability and reliability**

Designed to handle high throughput and numerous connected devices, the Rule Engine allows users to scale their IoT solutions without compromising performance or reliability.
