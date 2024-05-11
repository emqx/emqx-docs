## Message Validation

EMQX includes built-in message validation capabilities that support multiple schema formats such as JSON Schema, Protobuf, and Avro, as well as built-in SQL statement validation. This ensures that only messages conforming to predefined data formats are published to subscribers from specified topics. This page describes the message validation features and how to use and configure them.

## Feature Introduction

### Why Validate Data

Clients may publish non-standard messages to the Broker, which could lead to exceptions in subscribers and data systems or pose security risks. By validating data formats early, these non-compliant messages can be identified and blocked, ensuring system stability and reliability.

- **Data Integrity**: Validates the structure and format of MQTT messages to ensure data consistency and correctness.
- **Data Quality**: Enforces data quality by checking for missing or invalid fields, data types, and formats, ensuring data consistency and quality.
- **Unified Data Model**: Ensures that the entire team and project use a unified data model, reducing inconsistencies and errors.
- **Reuse and Sharing**: Allows team members to reuse and share schemas, improving collaboration efficiency and reducing repetitive work and errors.
- **Security**: Prevents malicious or incorrectly formatted messages from being processed, reducing the risk of security vulnerabilities.
- **Interoperability**: Ensures messages conform to standardized formats, facilitating communication between different devices and systems.
- **Debugging**: Easily identify and debug invalid or incorrectly formatted messages.

### Workflow

When a message is published, it is validated against predefined rules. If validation succeeds, the process continues; otherwise, user-configured actions are executed, such as message discarding or disconnection.

1. When a message is published, first, the publish permissions are checked. After passing the permission check, validation rules are matched based on the publish topic from a user-configured list of validators. A validator can be set for multiple topics or topic filters.

2. Once a validation rule is matched, the message is validated against the preset Schema or SQL.

   - Supports multiple types of Schema: JSON Schema, Protobuf, and Avro.
   - Supports SQL statements that comply with EMQX rule engine syntax.
   - A single policy can add multiple Schemas or SQLs and specify their relationships:
     - **All Pass**: Validation is considered successful only if all validations pass.
     - **Any Pass**: Validation stops and is considered successful if any validation passes.

3. Once validated successfully, the message continues to the next process, such as triggering the rule engine or dispatching to subscribers.

4. If validation fails, the following user-configured actions can be executed:

   - **Discard Message**: Terminate the publish and discard the message, returning a specific reason code (131 - Implementation Specific Error) for QoS 1 and QoS 2 messages via PUBACK.
   - **Disconnect and Discard Message**: Discard the message and disconnect the publishing client.
   - **Ignore**: No additional actions are taken.

   Regardless of the configured action, a log entry can be generated upon validation failure; users can configure the log's output level, which defaults to `warning`. Additionally, a validation failure can trigger a rule engine event `$events/message_cluster_validation_failed`, allowing users to catch this event for custom handling, such as publishing the erroneous message to another topic or sending it to Kafka for analysis.

### Example Validation Schema

<TODO: Provide a step-by-step example of using a JSON Schema>

Requirements for the JSON Schema:

- The JSON object must include a property named `temp`.
- The `temp` property must be an integer.
- The `temp` property must be at least 101.

```json
{
  "$schema": "http://json-schema.org/draft-06/schema#",
  "type": "object",
  "properties": {
    "temp": {
      "type": "integer",
      "minimum": 101
    }
  },
  "required": ["temp"]
}
```

Test method:

```bash
# Fails
mqttx pub -t t/1 -m '{"temp": 100}'

# Passes
mqttx pub -t t/1 -m '{"temp": 102}'
```

### APIs

- GET /message_validations: To list all the message validations
- GET /message_validations?topic=t/#&schema_name=jsonsch1&schema_type=json: Fetch validations based on filter
- PUT /message_validations: To update a validation
- POST /message_validations: To create a new validation

### Metrics and Statistics

| **Metric**   | **Description**                                              |
| ------------ | ------------------------------------------------------------ |
| Total        | Total number of triggers since the system started            |
| Successes    |                                                              |
| Failures     |                                                              |
| Success Rate | Success / Total * 100% (calculated on the front end)         |
| Speed        | Current verification speed, speed in the last 5 minutes, and historical maximum speed |

Metrics are resettable and are also added to Prometheus, accessible via the `/prometheus/message_validation` path.

## Configure Data Validation in Dashboard

1. Click on **Integrations** -> **Data Validation** in the left navigation of the Dashboard.
2. Click **Create** at the top right of the **Data Validation** page.
3. On the Create Data Validation page, configure the following information:
   - **Name**: Enter the name of the data validation rule.
   - **Message Source Topic**: Set the topics whose messages need to be validated. Multiple topics or topic filters can be set.
   - **Note** (optional): Enter any notes.
   - **Validation Method**:
     - **Validation Strategy**: Specify the relationship between multiple validation strategies. Default is set to **Any Pass**.
       - **All Pass**: Considered passing only when all validation methods pass.
       - **Any Pass**: Stops further validation and is considered passing if any validation method passes.
     - **Validation List**: In the **Validation List**, select **Type**, click **Add**, and in the pop-up **Create Schema** dialog, enter the name and schema. Click **Create**.
   - **Validation Failure Operation**: Select the actions to perform if validation fails:
     - **Discard Message**: Terminate the publish and discard the message, returning a specific reason code for QoS 1 and QoS 2 messages via PUBACK.
     - **Disconnect and Discard Message**: Discard the message and disconnect the publishing client.
     - **Ignore**: Perform no additional actions.
   - **Output Logs**: Select whether to log a message upon validation failure; default is enabled.
   - **Logs Level**: Set the log output level; default is `warning`.

## Configure Data Validation in Configuration File