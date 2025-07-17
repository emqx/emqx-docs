# Quick Start: Create a Flow Using Gemini Node

This section demonstrates how to quickly create and test an LLM-based Flow in the Flow Designer through a practical use case using the Gemini Node. 

This example demonstrates how to build a Flow that integrates with the Gemini LLM to process MQTT device messages containing a free-text `prompt` while preserving the `clientid` for routing. A single Data Processing node pulls out both `payload.prompt` and `clientid`. The Gemini node generates a reply based on that prompt, and the Republish node sends the AI’s reply to the per-client topic `device/${clientid}/reply`, ensuring each device receives its own customized advice.

## Scenario Description

In a smart agriculture deployment, each greenhouse is equipped with soil sensors that periodically publish JSON messages to the topic `devices/<greenhouse_id>`. Each message’s `prompt` field contains key environmental readings in plain text. The Flow will:

- **Data Processing**: Extract the sensor readings from the `prompt` field and expose the `clientid` (i.e., `greenhouse_id`) for downstream use.
- **LLM-Based Processing**: Send the readings to Gemini to generate an actionable irrigation recommendation.
- **Message Republish**: Publish the AI-generated advice back to the per-greenhouse control topic `device/<greenhouse_id>/reply`.

**Sample incoming message (to `devices/gh_1`):**

```json
{
  "prompt": "Soil moisture is 18%. Air temperature is 28°C. Humidity is 65%."
}
```

**Expected republished output (to `device/gh_1/reply`):**

```
Irrigate Zone 1 with 15 liters of water for 20 minutes.
```

## Create the Flow

::: tip Prerequisite

Make sure you have a valid Gemini API Key.

:::

1. Click the **Create Flow** button on the **Flows** page.

2. Add a **Messages** node.

   - Drag a **Messages** node from the Source panel.
   - Set the topic to `devices/+`.
   - Click **Save**.

3. Add a **Data Processing** node.

   - Drag a **Data Processing** node from the **Processing** section.
   - Fill in the form with the following configurations. This setting exposes `clientid` for later use to ensure that it is accessible in downstream nodes (e.g., for `${clientid}` in republish topics).
     
     - **Field**: `clientid`
     - **Transform**: Leave empty
     - **Alias**: `clientid`
   - Click **Save**.
   
4. Add a **Gemini** node.

   - Drag a **Gemini** node from the Processing section and connect it to the Data Processing node.

   - Configure the node:

     - **Input**: Enter `payload.prompt`.

     - **System Message**: Enter the following message:

       ```
       You are an expert agricultural AI assistant.  
       Based on soil moisture, air temperature, and humidity readings provided in the user prompt, generate a concise irrigation recommendation specifying the zone, amount of water (in liters), and duration.  
       Only return a single sentence with the recommendation—no extra commentary.
       ```
       
     - **Model**: Here you can keep the default model `gemini-2.0-flash`.

     - **API Key**: Enter your Gemini API key.

     - **Base URL**: Leave empty to use Gemini’s default endpoint.

     - **Output Result Alias**: Enter `ai_reply`.

   - Click **Save**.

5. Add a **Republish** node.

   - Drag a **Republish** node from the Sink section and connect it to the Gemini node.
   - Set the topic to `device/${clientid}/reply`.
   - Set the payload to `${ai_reply}`.
   - Click **Save**.

6. Connect all the nodes and click **Save** in the upper-right corner to save the Flow.

   ![openai_node_flow](./assets/gemini_node_flow.png)

   Flows and form rules are interoperable. You can also view the SQL and related rule configurations on the Rule page.

   ![openai_node_rule_page](./assets/gemini_node_rule_page.png)

## Test the Flow

1. Connect an MQTT client to EMQX.

   To quickly test the flow, you can use the **Diagnostic Tools** -> **WebSocket Client** on the Dashboard to simulate an MQTT client. Alternatively, you can also use the [MQTTX](https://mqttx.app/) tool or a real MQTT client:

   - Connect to your EMQX server.
   - Subscribe to the topic, for example `device/gh_1/reply`.

2. Start Testing.

   - In the Flow Designer, click any node to open the Edit panel.

   - Click **Edit**, then click **Start Test** to open the test panel at the bottom.

   - Click **Input Simulated Data** and publish the following message to topic `device/gh_1` by clicking **Submit Test**:

     ```json
     {
       "prompt": "Soil moisture is 18%. Air temperature is 28°C. Humidity is 65%."
     }
     ```
   
3. Review results.

   - You can see the successful execution result of the flow.

     ![openai_node_test_result](./assets/gemini_node_test_result.png)

   - Return to the **WebSocket Client** page and you should receive an AI-generated summary like:

     > “Irrigate Zone 1 with 15 liters of water for 20 minutes.”

   - If the test results are unsuccessful, error messages will be displayed accordingly.

   - To view the running statistics and metrics of the **Gemini** node, exit the editting page, click the node to open the Edit panel and click the **Overview** tab.

     ![openai_node_statistics](./assets/gemini_node_statistics.png)



