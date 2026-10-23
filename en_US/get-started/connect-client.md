# Connect a Client

Connect MQTTX CLI to your local EMQX broker and verify that the connection succeeds. A successful connection does not verify topic permissions or message delivery. To test message delivery, continue with [Publish and Subscribe](publish-subscribe.md).

## Prerequisites

- Complete [Quick Start](quick-start.md) to start the local broker and install MQTTX CLI. Keep the broker running.
- Stop any earlier test subscribers with Ctrl+C before starting this connection test.

This example uses the disposable Quick Start broker, which accepts local MQTT connections without credentials. Complete the basic exercises before adding authentication later in the learning path. For a secured deployment, use its required credentials and TLS settings instead of these local settings.

## Connection Settings

Use these settings for the local Quick Start broker:

| Setting | Value | CLI Option |
| --- | --- | --- |
| Broker address | `127.0.0.1` | `-h 127.0.0.1` |
| Host port | `1883` | `-p 1883` |
| Transport | MQTT over TCP, without TLS | `-l mqtt` |
| MQTT version | MQTT 5.0 | `-V 5.0` |
| Client ID | `docs-connect` | `-i docs-connect` |
| Username and password | Not required for this local broker | Omit credential options. |

If you changed the MQTT host port in Quick Start, replace `1883` with that port. Run the command on your computer; inside a container, `127.0.0.1` refers to that container, not your computer.

For transport compatibility, Client ID selection, authentication, TLS, and Keep Alive concepts, see [MQTT Connection Settings](../develop/mqtt/basics.md#mqtt-connection-settings).

## Connect and Verify

1. Run the following command on your computer:

   ```bash
   mqttx conn -h 127.0.0.1 -p 1883 -l mqtt -V 5.0 -i docs-connect
   ```

2. Wait until MQTTX reports **Connected**. If the connection fails, use the [connection checks](../develop/clients/test-clients.md#if-the-message-does-not-arrive).
3. [Optional] Open the EMQX Dashboard and go to **Monitoring** -> **Clients**. While the CLI client is connected, find Client ID `docs-connect` to confirm that it reached the intended broker.
4. Press Ctrl+C to disconnect the client before continuing with separate publisher and subscriber clients.

## Continue Learning

Next, [publish and subscribe](publish-subscribe.md) with two clients and check the received message.

For a graphical client, see [MQTTX Desktop](../develop/clients/test-clients.md#test-with-mqttx-desktop). For browser-based testing, see [MQTTX Web](../develop/clients/test-clients.md#test-with-mqttx-web), including its WebSocket listener and browser requirements.

To connect your application with a client library, [continue in Develop](../develop/connect-emqx/introduction.md).
