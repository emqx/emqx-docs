# Schema Registry Example - External HTTP Server

This page demonstrates how the Schema Registry and Rule Engine support message encoding and decoding using an external HTTP server with custom logic.

In some scenarios, you might need to apply custom encoding or decoding logic that EMQX does not support natively. EMQX allows you to delegate this processing to an external HTTP service by invoking it through `schema_encode` and `schema_decode` functions within a rule.

## External HTTP API Specification

To implement a custom External HTTP API that integrates with EMQX's `schema_encode` and `schema_decode` functions, your External HTTP server must provide a single `POST` endpoint that handles the encoding or decoding requests from EMQX.  

### Request Format

The request body is a JSON object with the following fields:

- `payload`: Base64-encoded string value passed to the `schema_encode` or `schema_decode` function in Rule Engine.
- `type`: Either the `encode` or the `decode` string, depending on which function is evaluated, `schema_encode` or `schema_decode`.
- `schema_name`: A string identifying the name of this External HTTP schema configured in EMQX.
- `opts`: An arbitrary string that can be configured in EMQX to provide further options, which is passed unaltered to the HTTP server.

### Response Format

- The server must respond with HTTP status code `200`.
- The response body must contain a base64-encoded string representing the result. Note that this base64 value must not be further JSON-encoded when replying to EMQX.

## Example Use Case

Suppose a device publishes a binary message, and you want to encode or decode the payload using a custom XOR operation. This section demonstrates how to integrate custom encoding and decoding logic into EMQX by building a simple external HTTP service.

### Build an External HTTP Service

The following example demonstrates how to create and run a simple HTTP server using Python and Flask. The server receives Base64-encoded data and applies an XOR operation to the decoded payload.

<details>
<summary><strong>Code for sample External HTTP Server</strong></summary>

Ensure [Flask](https://flask.palletsprojects.com/en/stable/) is installed:

```sh
pip install Flask==3.1.0
```

Sample code:

```python
from flask import Flask, request
import base64

app = Flask(__name__)

@app.route("/serde", methods=['POST'])
def serde():
    # The input payload is base64 encoded
    body = request.get_json(force=True)
    print("incoming request:", body)
    payload64 = body.get("payload")
    payload = base64.b64decode(payload64)
    secret = 122
    response = bytes(b ^ secret for b in payload)
    # The response must also be base64 encoded
    response64 = base64.b64encode(response)
    return response64
```

To run your server:

```sh
# This assumes your server is in the same directory in a file named `myapp.py`
flask --app myapp --debug run -h 0.0.0.0 -p 9500
```

</details>

### Create External HTTP Schema in EMQX

1. Go to the Dashboard, and select **Smart Data Hub** -> **Schema Registry** from the left navigation menu.

2. In the **Internal** tab page, click **Create**.

3. Create an External HTTP server schema using the following parameters:
   - **Name**: `myhttp`

   - **Type**: `External HTTP`

   - **URL**: The full URI where your server is running.  For example: `http://server:9500/serde`.

4. Click **Create**.

### Create a Rule to Apply Schema

Use the EMQX rule engine to create a rule that applies your schema for message encoding and decoding.

1. In the Dashboard, select **Integration** -> **Rules** from the navigation menu.

2. On the **Rules** page, click **Create** at the top right corner.

3. Use the schema you have just created to write the rule SQL statement:

   ```sql
   SELECT
     schema_encode('myhttp', payload) as encoded,
     schema_decode('myhttp', encoded) as decoded
   FROM
     "t/external_http"
   ```

   Both `schema_encode('myhttp', payload)` and `schema_decode('myhttp', encoded)` will call the configured External HTTP server to encode/decode the given payload.

4. Click **Add Action**.  Select `Republish` from the drop-down list of the **Action** field.

5. In the **Topic** field, type `external_http/out` as the destination topic.

6. In the **Payload** field, type message content template: `${.}`. 

7. Click **Add** to add the action to the rule.

   This action sends the decoded message to the topic `external_http/out` in JSON format. `${.}` is a variable placeholder that will be replaced at runtime with the value of the whole output of the rule.

8. Click **Save** to complete the rule creation.

### Check Rule Execution Results

1. In the Dashboard, select **Diagnose** -> **WebSocket Client**.
2. Fill in the connection information for the current EMQX instance.
   - If you run EMQX locally, you can use the default value.
   - If you have changed EMQX's default configuration. For example, the configuration change on authentication can require you to type in a username and password.
3. Click **Connect** to connect to the EMQX instance as an MQTT client.
4. In the **Subscription** area, type `external_http/out` in the **Topic** field and click **Subscribe**.

5. In the **Publish** area, type `t/external_http` in the **Topic** field, write any payload you wish, and click **Publish**.

6. Check that a message with the topic `external_http/out` is received on the Websocket side.  For example, if your payload was `hello`:

   ```json
   {"encoded":"\u0012\u001F\u0016\u0016\u0015","decoded":"hello"}
   ```
