# Publish and Subscribe

Build on the first message exchange in [Quick Start](quick-start.md) by using a new topic, distinct Client IDs, QoS 1, and a JSON payload. This task explains the command options and shows how to change the payload and verify the result. Use the local broker connection verified in [Connect a Client](connect-client.md).

## Prerequisites

- Complete [Connect a Client](connect-client.md) and keep the local Quick Start broker running. Press Ctrl+C to disconnect the `docs-connect` client if it is still running.
- Have two terminal windows available and use the MQTTX CLI installed for Quick Start. Run the commands on your computer, not inside the broker container.

Use the same broker address and MQTT host port as in Connect a Client. The commands below use `127.0.0.1:1883`; if you changed the MQTT host port, replace `1883` in both commands. This local broker accepts connections without credentials. Complete these basic exercises before enabling authentication and topic permissions.

## Subscribe to a Topic

Both clients use MQTT 5.0 (`-V 5.0`) over TCP (`-l mqtt`). The `-t` option specifies the topic, and `-i` gives each client a distinct Client ID so they do not disconnect one another.

1. In Terminal 1, subscribe to `docs/test`:

   ```bash
   mqttx sub -h 127.0.0.1 -p 1883 -l mqtt -V 5.0 -i docs-reader -t docs/test -q 1 -v
   ```

   The `-q 1` option requests a subscription QoS of 1. The `-v` option includes the topic in the received-message output.

2. Wait until MQTTX reports that the client is connected and the subscription succeeds. Keep Terminal 1 running before publishing.

## Publish a Message

1. In Terminal 2, publish a message to the same topic:

   ```bash
   mqttx pub -h 127.0.0.1 -p 1883 -l mqtt -V 5.0 -i docs-writer -t docs/test -q 1 -m '{"message":"hello-001"}'
   ```

   The `-q 1` option publishes at QoS 1, and `-m` specifies the payload.

2. Return to Terminal 1 to verify receipt. A successful connection or a completed publish command alone does not confirm that the subscriber received the message.

## Verify Message Delivery

1. Confirm that Terminal 1 displays topic `docs/test` and this payload:

   ```json
   {"message":"hello-001"}
   ```

2. To repeat the test, publish a different value, such as `{"message":"hello-002"}`, from Terminal 2 and check for that value in Terminal 1. Keep the subscriber running throughout the test.
3. After verification, press Ctrl+C in Terminal 1 to stop the subscriber. Keep the broker running for the next task. When you finish the learning path, follow [Quick Start: Clean Up](quick-start.md#clean-up).

These commands do not request session retention or retained publishing, so they do not deliberately leave an offline session or retained value.

## If the Message Does Not Arrive

| What You See | What to Check |
| --- | --- |
| Connection fails | Use the same host and MQTT port as in Connect a Client. Check that the broker is running; see the [Quick Start checks](quick-start.md#if-the-message-does-not-arrive). |
| One client disconnects when the other connects | Use different Client IDs: `docs-reader` for the subscriber and `docs-writer` for the publisher. Restart the subscriber and wait for subscription confirmation before publishing again. |
| Subscription fails | Read the reported subscription error. If you enabled topic permissions after Quick Start, check that the subscriber can subscribe to `docs/test`; see [authorization](../guides/access-control/authz/authz.md). |
| Connected, but no message | Keep Terminal 1 running, confirm the subscription succeeds, and publish again. Both commands must use the same host, port, and exact topic `docs/test`. Check for the payload you just sent. |

For other failures, see the [client testing checks](../develop/clients/test-clients.md#if-the-message-does-not-arrive).

## Continue Learning

Next, [use topics and wildcards](topics-and-wildcards.md) to receive several related topics with one subscription.

For graphical or browser-based testing, see [MQTTX Desktop](../develop/clients/test-clients.md#test-with-mqttx-desktop) or [MQTTX Web](../develop/clients/test-clients.md#test-with-mqttx-web). For testing through the Dashboard, see [Dashboard WebSocket Client](../develop/clients/test-clients.md#test-with-the-dashboard-websocket-client).
