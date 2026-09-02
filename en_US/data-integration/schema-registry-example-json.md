# Schema Registry Example - JSON Schema

This page demonstrates how to register a draft 2020-12 JSON Schema in Schema Registry and use the `schema_check` function in a rule to validate MQTT message payloads. The example republishes only payloads that conform to the schema.

## Supported JSON Schema Drafts

Starting from EMQX 6.0.4, Schema Registry supports the following JSON Schema drafts:

- draft-03
- draft-04
- draft-06
- draft 2019-09
- draft 2020-12

EMQX selects the JSON Schema version based on the value of the `$schema` field. If `$schema` is omitted, EMQX uses draft-06.

Support for draft 2019-09 and draft 2020-12 has the following limitations:

- Draft 2019-09 does not support `$recursiveRef`.
- Draft 2020-12 does not support `$dynamicRef`.
- References to remote schemas are not supported for these two drafts.

If a schema uses an unsupported keyword, validation returns an error instead of silently ignoring the keyword.

## Create a JSON Schema

Create a schema that accepts an array containing exactly two integers:

1. In the EMQX Dashboard, click **Smart Data Hub** -> **Schema Registry** in the left navigation menu.
2. On the **Internal** tab, click **Create**.
3. Configure the following fields:

   - **Name**: Enter `json_array`.
   - **Type**: Select **JSON Schema**.
   - **Schema**: Enter the following draft 2020-12 schema:

     ```json
     {
       "$schema": "https://json-schema.org/draft/2020-12/schema",
       "type": "array",
       "prefixItems": [
         { "type": "integer" },
         { "$ref": "#/prefixItems/0" }
       ],
       "minItems": 2,
       "maxItems": 2
     }
     ```

4. Click **Create**.

The `prefixItems` array defines the schema for each position. The local `$ref` makes the second item use the same integer schema as the first item. `minItems` and `maxItems` require the payload to contain exactly two items.

## Create a Rule

Create a rule that republishes messages only when the payload conforms to `json_array`:

1. In the Dashboard, click **Integration** -> **Rules** in the left navigation menu.
2. On the **Rules** page, click **Create**.
3. Enter `validate_json_array` in the **Name** field.
4. Enter the following statement in the **SQL Editor**:

   ```sql
   SELECT *
   FROM "t/json"
   WHERE schema_check('json_array', payload)
   ```

   The `schema_check` function returns `true` when the payload conforms to `json_array`. Otherwise, it returns `false`, and the rule does not execute its action.

5. Click **Add Action**, and select **Republish**.
6. Enter `validated/json` in the **Topic** field and `${payload}` in the **Payload** field.
7. Click **Create**.

## Test the Rule

Use MQTTX CLI to verify the rule:

1. Subscribe to the republish topic:

   ```bash
   mqttx sub -t validated/json
   ```

2. In another terminal, publish a payload that conforms to the schema:

   ```bash
   mqttx pub -t t/json -m '[1, 2]'
   ```

   The subscriber receives `[1, 2]` from the `validated/json` topic.

3. Publish a payload whose second item is not an integer:

   ```bash
   mqttx pub -t t/json -m '[1, "two"]'
   ```

   The rule does not execute the republish action, and the subscriber receives no message.

Using `schema_check` in a rule filters rule execution but does not reject the original MQTT message. To reject or discard nonconforming messages, use [Schema Validation](./schema-validation.md).
