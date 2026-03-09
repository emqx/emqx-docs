# Ingest MQTT Data into HTTP Server

The HTTP server data integration provides a quick way to integrate EMQX with external HTTP services. It supports flexible configuration of request methods and request data formats, offers secure communication through HTTPS, and provides authentication mechanisms. It can efficiently and flexibly transmit client messages and event data in real-time, enabling scenarios such as IoT device state notifications, alert notifications, and data integration.

This page provides a detailed overview of the features and capabilities of the data integration with HTTP server and offers practical guidance on setting up an HTTP server data integration.

:::tip 

For users who need to integrate with HTTP services but do not require data processing using rules, we recommend using [Webhook](./webhook.md) as it is simpler and easier to use. 

:::

<video
  src="./assets/http_server_integration.mp4"
  autoplay
  controls
  muted
  playsinline
  style="width: 100%; border-radius: 8px;"></video>

## How It Works

HTTP server data integration is an out-of-the-box feature in EMQX, allowing integration with external HTTP services through simple configuration. With the HTTP service, users can write code in their preferred programming language and framework to implement custom, flexible, and complex data processing logic.

<img src="./assets/emqx-integration-http.jpg" alt="emqx-integration-http" style="zoom:67%;" />

EMQX forwards device events and messages to an HTTP server through the rule engine and Sink. The workflow is as follows:

1. **Devices Connect to EMQX**: When IoT devices successfully connect, an online event is triggered, containing device ID, source IP address, and other attributes.
2. **Devices Publish Messages**: Devices publish telemetry and status data through specific topics, triggering the rule engine.
3. **Rule Engine Processes Messages**: The rule engine matches messages based on topic filters and processes them using configured rules, such as filtering fields, transforming data formats, or enriching messages with additional context.
4. **Bridging to HTTP Server**: The rule triggers the action of forwarding the processed messages and events to the HTTP server. Request headers, request bodies, and URLs can be dynamically constructed from the rule output.

After events and message data are sent to the HTTP server, you can perform flexible processing, such as:

- Updating device status or recording device events in device management systems.
- Writing message data to a database for storage.
- Triggering alert or notification systems based on abnormal data detected by SQL rules.

## Features and Benefits

Using EMQX's HTTP server integration can bring the following advantages to your business:

- **Extend Data Delivery to More Downstream Systems**: The HTTP service enables seamless integration of MQTT data with various external systems, such as analytics platforms and cloud services, facilitating data distribution across multiple systems.
- **Real-time Responses and Business Process Triggering**: Through the HTTP service, external systems can receive MQTT data in real-time and trigger business processes, ensuring quick responses. For example, receiving alert data and triggering business workflows.
- **Custom Data Processing**: External systems can perform secondary processing on received data as per their needs, allowing for more complex business logic that is not limited by EMQX's capabilities.
- **Loosely Coupled Integration**: The HTTP service uses a simple HTTP interface, providing a loosely coupled approach to system integration.

In summary, the HTTP service offers real-time, flexible, and customizable data integration capabilities for your business, catering to flexible and diverse application development needs.

## Before You Start

This section describes the preparations you need to complete before you start to create the HTTP server data integration, including setting up a simple HTTP server.

### Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)

- Knowledge about [Data Integration](./data-bridges.md)


### Set up a Simple HTTP Server

1. Use Python to build a simple HTTP service. This HTTP service will receive the `POST /` requests and will return `200 OK` after printing the requested content:

```bash
from flask import Flask, json, request

api = Flask(__name__)

@api.route('/', methods=['POST'])
def print_messages():
  reply= {"result": "ok", "message": "success"}
  print("got post request: ", request.get_data())
  return json.dumps(reply), 200

if __name__ == '__main__':
  api.run()
```

2. Save the above code as `http_server.py` file. Then start the server by running:

```shell
pip install flask

python3 http_server.py
```

## Create a Connector

This section demonstrates how to configure an HTTP server Connector that is used to connect the Sink to the HTTP server.

1. From the Dashboard left menu, click **Integration** -> **Connector**.
2. Click **Create** in the upper-right corner of the page.
3. Select **HTTP Server** as the connector type and click **Next**.
4. Enter a name for the Connector. The name should be a combination of upper/lowercase letters or numbers, for example: `httpserver`. 
5. Set **URL** to the address of your HTTP server. For example: `http://localhost:5000`.
6. For the remaining settings, keep the default values.
7. Advanced settings (optional):  For details, see [Features of Sink](./data-bridges.md#features-of-sink).
8. Before clicking **Create**, you can click **Test Connectivity** to verify that the Connector can connect to the HTTP server.
9. Click **Create** to complete the Connector configuration.

After the Connector is created successfully, a dialog appears asking whether you want to create a rule using this connector.

- Click **Create Rule** to go directly to the rule creation page and continue configuring the integration.
- Alternatively, click **Back To Connector List** to return to the Connector list and create a rule later from **Integration** -> **Rules**.

In this example, click **Create Rule** to continue.

## Create a Rule with HTTP Server Sink

This section demonstrates how to create a rule and configure an HTTP Server Sink to send MQTT messages to the HTTP server.

After clicking **Create Rule**, you are automatically directed to the **Create Rule** page, where the **Action pane** (for configuring the HTTP Server Sink) is automatically displayed with the connector ready to be used.

1. The **Type of Action** and **Action** are auto-filled with `HTTP Server` and `Create Action` to create a new Sink.

2. Enter the name and description for the Sink. The **Connector** is auto-filled with the connector you created earlier: `httpserver`.

3. Configure the HTTP request:

   - **URL Path**: `/`
   - **Method**: `POST`

   The final request URL will be constructed by combining the Connector URL and this path.

4. Configure the **Request Body** to send MQTT message data to the HTTP server:

   ```json
   {
     "topic": "${topic}",
     "payload": ${payload},
     "clientid": "${clientid}",
     "qos": ${qos},
     "timestamp": ${timestamp}
   }
   ```

   The variables in the template are populated using fields selected by the rule SQL.

5. **Fallback Actions (Optional)**: If you want to improve reliability in case of message delivery failure, you can define one or more fallback actions. These actions will be triggered if the primary Sink fails to process a message. See [Fallback Actions](./data-bridges.md#fallback-actions) for more details.

6. Before clicking **Create**, you can click **Test Connectivity** to verify that the Sink can connect to the HTTP server.

7. Click **Create** to complete the Sink configuration. The new Sink appears under the **Action Outputs** section of the rule on the **Create Rule** page.

8. Enter the **Rule ID**, which can either be randomly generated by the system or defined by yourself (optional), for example: `my_rule`.

9. In the **SQL Editor**, enter the following SQL statement:

   ```bash
   SELECT
     *
   FROM
     "t/#"
   ```

   This rule matches all MQTT messages published to topics under `t/#`.

   ::: tip

   If you want to specify your own SQL syntax, make sure that you have included all fields required by the Sink in the `SELECT` part.

   :::

10. After verifying the rule configuration, click **Save** to generate the rule. 

After the rule is created, messages published to topics under `t/#` will be processed by the rule and forwarded to the configured HTTP server.

You can also go to **Integration** -> **Flow Designer** to view the data flow topology of the rule and the HTTP Server Sink.

## Test the Rule

1. Use MQTTX  to send a message to topic  `t/1`  to trigger an online/offline event.

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello HTTP Server" }'
   ```

2. Go to the **Rule** page in the Dashboard and click the rule name to view its statistics. The metrics should show one new incoming message and one new outgoing message, indicating that the message has been successfully processed and forwarded by the HTTP Server Sink.

3. Verify that the HTTP server has received the request.

   If the Python HTTP server is running, the terminal should display output similar to the following:

   ```text
   python3 http_server.py
    * Serving Flask app 'http_server'
    * Environment: production
      WARNING: This is a development server. Do not use it in a production deployment.
      Use a production WSGI server instead.
    * Debug mode: off
    * Running on http://127.0.0.1:5000 (Press CTRL+C to quit)
   
   got post request:  b'{"topic":"t/1","payload":{"msg":"hello HTTP Server"},"clientid":"emqx_c","qos":0,"timestamp":1700000000000}'
   ```

   The printed content shows that EMQX has forwarded the MQTT message to the HTTP server in JSON format. The fields in the request body correspond to the variables configured in the Sink request body template.
