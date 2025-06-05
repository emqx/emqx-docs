# LLM-Based MQTT Data Processing

Introduction of this page...

## Feature Overview

What is the feature?

### How It Works?

Working principles

### What Can It Do?

Use cases. What can users achieve using this procedding node

## How to Configure LLM-Based Processing Nodes

### OpenAI

Instructions for configure the node:

- **Input**：
- **System Message**：
- **Model**：
- **API Key**：
- **Base URL**：
- **Output Result Alias**：

### Anthropic

Instructions for configure the node:

- **Input**：
- **System Message**：
- **Model**：
- **Max Tokens**:
- **Anthropic Version**:
- **API Key**：
- **Base URL**：
- **Output Result Alias**：

## Quick Start

Provide two different simple demos.

### Use Case1: Create a Flow Using OpenAI Node

This section demonstrates how to quickly create and test an LLM-based Flow in the Flow Designer through a practical use case.

This demonstration shows you how to build a workflow that receives sensor data from MQTT topics and uses an LLM (e.g., OpenAI GPT) to interpret the data and summarize its meaning in natural language. The resulting summary is republished to a new topic, `ai/summary`, for downstream consumption.

### Scenario Description

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

```css
Device device123 reported a temperature of 38.2°C and 75% humidity.
```

### Create the Flow

1. **Create a New Flow**

   Click the **Create Flow** button on the **Flows** page.

2. **Add a Messages Node**

   - Drag a **Messages** node from the Source panel.
   - Set the topic to `sensors/temp_humid`.
   - Click **Save**.

3. **Add a Data Processing Node**

   - Drag a **Data Processing** node from the **Processing** section.
   - Add the following mappings:
     - `payload.device_id` → alias `device_id`
     - `payload.temperature` → alias `temperature`
     - `payload.humidity` → alias `humidity`
   - Click **Save**.

4. **Add an OpenAI Node**

   - Drag an **OpenAI** node from the Processing section and connect it to the Data Processing node.
   - Configure the node:
     - **System Message**:
        *"Generate a short summary of the device’s sensor readings in human-readable format."*
     - **Model**: `gpt-4o`
     - **API Key**: Your OpenAI API key
     - **Input**: Use the entire `payload` or combine fields like `${device_id}, ${temperature}, ${humidity}`
     - **Output Result Alias**: `summary`
   - Click **Save**.

5. **Add a Republish Node**

   - Drag a **Republish** node from the Sink section and connect it to the OpenAI node.
   - Set the topic to `ai/summary`.
   - Set the payload to `${summary}`.
   - Click **Save**.

6. **Save the Flow**

   - Click **Save** in the upper-right corner to save and activate the Flow.

### Test the Flow

1. **Start Testing**

   - In the Flow Designer, click any node to open the configuration panel.
   - Click **Edit Flow**, then click **Start Test** to open the test panel at the bottom.

2. **Send Test Messages**

   Use a real MQTT client such as **MQTTX Web** or **MQTTX CLI**:

   - Connect to your EMQX server.
   - Subscribe to the topic `ai/summary`.

   **Example 1**: Publish this message to topic `sensors/temp_humid`:

   ```json
   {
     "device_id": "device123",
     "temperature": 38.2,
     "humidity": 75
   }
   ```

   You should receive an AI-generated summary like:

   > “Device device123 reported a temperature of 38.2°C and 75% humidity.”

   **Example 2**: Try with different sensor values to see variations in the LLM response.

3. **Review Results**

   - Return to the Flow Designer test panel to view live test logs.
   - If the flow fails, check the OpenAI node for errors such as incorrect API key or timeout.

### Use Case1: Create a Flow Using Anthropic Node

Refer to the [Quick Start](./introduction#quick-start) part in the introduction.md.