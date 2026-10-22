# Test Publishing and Subscribing

This fixture preserves selected procedure and troubleshooting sections from the supplied Enterprise documentation rebuild. It is not a complete migration of that page.

## Prerequisites

Use a running isolated broker at `127.0.0.1:1883`, MQTTX CLI 1.13.0, MQTT 5, and permissions for `docs/test`. Run the commands on your computer; inside a container, `127.0.0.1` refers to that container. Use different Client IDs for the publisher and subscriber.

## MQTTX CLI

In terminal A, subscribe and wait until MQTTX reports the connection and subscription:

```bash
mqttx sub -h 127.0.0.1 -p 1883 -V 5.0 -i docs-reader \
  -t docs/test -q 1 -v
```

Keep that command running. In terminal B, publish a unique payload:

```bash
mqttx pub -h 127.0.0.1 -p 1883 -V 5.0 -i docs-writer \
  -t docs/test -q 1 -m '{"message":"hello-001"}'
```

Terminal A should show topic `docs/test` and payload `{"message":"hello-001"}`. A successful connection alone is not the expected result. If the publisher finishes but the subscriber shows nothing, follow the checks below.

For navigation context, see the [Reference workspace](../../reference/index.md).

Stop terminal A with Ctrl+C after verification. These examples do not request session retention or retained publishing, so they do not deliberately leave an offline session or retained value.

## If the Message Does Not Arrive

| Observation | Check Next |
| --- | --- |
| Connection fails | Host, listener status, port, transport, TLS trust, credentials, and MQTT version. |
| A client disconnects when the other connects | Duplicate Client IDs. Give them distinct IDs. |
| Connection succeeds but subscription fails | Subscribe authorization and the SUBACK reason code. |
| Subscription succeeds but publishing fails | Publish authorization, topic validity, packet size, and the PUBACK reason code. |
| Both succeed but there is no received message | Subscribe before publishing; compare exact topic case and spelling; inspect expiry and filtering. |
| Only the browser client fails | WebSocket port/path, `ws` versus `wss`, certificate trust, browser policy, and network reachability. |

A public test broker is suitable only for disposable, non-sensitive examples. Use your own broker to verify authentication, authorization, retention, and operational settings.


## Related Tasks

Return to the [introductory task](../../get-started/publish-subscribe.md) or [Reference](../../reference/index.md).
