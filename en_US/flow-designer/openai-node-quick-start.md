# Quick Start: Create a Flow Using OpenAI Node

This section demonstrates how to quickly create and test an LLM-based Flow in the Flow Designer through a practical use case.

This demonstration shows you how to build a workflow that receives sensor data from MQTT topics and uses an LLM (e.g., OpenAI GPT) to interpret the data and summarize its meaning in natural language. The resulting summary is republished to a new topic, `ai/summary`, for downstream consumption.

## Scenario Description

Assume a device reports temperature and humidity readings to the MQTT topic `sensors/temp_humid`. Each message includes raw sensor data in JSON format. The EMQX Flow will perform the following steps:

- **Data Processing**: Extract the device ID and sensor values.
- **LLM-Based Processing**: Use an OpenAI model to summarize the sensor reading.
- **Message Republish**: Publish the AI-generated summary to a new topic, `ai/summary`.

**Sample message:**

```json
{
  "device_id": "device123",
  "temperature": 38.2,
  "humidity": 75,
  "timestamp": 1717568000000
}
```

**Expected output (AI-generated):**

```
Device device123 reported a temperature of 38.2°C and 75% humidity.
```

## Create the Flow

::: tip Prerequisite

Make sure you have a valid OpenAI API Key.

:::

1. Click the **Create Flow** button on the **Flows** page.

2. Add a **Messages** node.

   - Drag a **Messages** node from the Source panel.
   - Set the topic to `sensors/temp_humid`.
   - Click **Save**.

3. Add a **Data Processing** node.

   - Drag a **Data Processing** node from the **Processing** section.
   - Add the following mappings:
     - `payload.device_id` to alias `device_id`
     - `payload.temperature` to alias `temperature`
     - `payload.humidity` to alias `humidity`
   - Click **Save**.

4. Add an **OpenAI** node.

   - Drag an **OpenAI** node from the Processing section and connect it to the Data Processing node.
   - Configure the node:
     - **Input**: Use the entire `payload` or combine fields like `${device_id}, ${temperature}, ${humidity}`.
     - **System Message**: Enter *"Generate a short summary of the device’s sensor readings in human-readable format."*
     - **Model**: Select `gpt-4o`.
     - **API Key**: Enter your OpenAI API key.
     - **Base URL**: Leave empty.
     - **Output Result Alias**: Enter `summary`.
   - Click **Save**.

5. Add a **Republish** node.

   - Drag a **Republish** node from the Sink section and connect it to the OpenAI node.
   - Set the topic to `ai/summary`.
   - Set the payload to `${summary}`.
   - Click **Save**.

6. Connect all the nodes and click **Save** in the upper-right corner to save the Flow.

   ![openai_node_flow](./assets/openai_node_flow.png)

   Flows and form rules are interoperable. You can also view the SQL and related rule configurations on the Rule page.

   ![openai_node_rule_page](./assets/openai_node_rule_page.png)

## Test the Flow

1. Connect an MQTT client to EMQX.

   To quickly test the flow. You can use the **Diagnostic Tools** -> **Websocket Client** on the Dashboard to simulate an MQTT client. Or, you can also use the [MQTTX](https://mqttx.app/) tool or a real MQTT client:

   - Connect to your EMQX server.
   - Subscribe to the topic `ai/summary`.

2. Start Testing.

   - In the Flow Designer, double-click any node to open the Edit panel.

   - Click **Edit**, then click **Start Test** to open the test panel at the bottom.

   - Click **Input Simulated Data** and publish this message to topic `sensors/temp_humid` by clicking **Submit Test**:

     ```json
     {
       "device_id": "device123",
       "temperature": 38.2,
       "humidity": 75
     }
     ```

3. Review results.

   - You can see the successful excecution result of the flow.

     ![openai_node_test_result](./assets/openai_node_test_result.png)

   - Return to the **WebSocket Client** page and you You should receive an AI-generated summary like:

     > “The sensor readings from device "device123" indicate that the current temperature is 38.2°C and the humidity level is 75%.”

   - If the test results are unsuccessful, error messages will be displayed accordingly.
