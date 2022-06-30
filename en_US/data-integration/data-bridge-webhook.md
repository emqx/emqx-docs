# Webhook

Webhook is the channel through which emqx sends messages to HTTP services.
Through webhooks, users can send to the remote HTTP service from a local topic,
or from the output of a rule.

## Example: Setup Webhook using Config Files

Add following configs to the end of the `emqx.conf` file:

```js
bridges.webhook.my_webhook {
    enable = true
    direction = egress
    url = "http://localhost:9901/${clientid}"
    local_topic = "a/#"
    method = post
    body = "${payload}"
    headers {
        "content-type": "application/json"
    }
}
```

This webhook forwards messages sent to this node which match `a/#` to `http://localhost:9901/${clientid}`.
Where `${clientid}` is a placeholder variable representing the sender's client ID.
For example, if the client ID is `steve`, the message will be sent to `http://localhost:9901/steve`.

Placeholders can be used in the following parameters: `method`, `body`, `headers` and `url`.

Note that placeholders can only be used in the path part of the `url` parameter, but cannot be used in the `scheme://host:port` part.

For all the available placeholder fields, see [event types and fields in rule SQL](./rule-sql-events-and-fields.md#mqtt-message).

## Example: Create Webhooks from the Dashboard

See [Quick Start with Data Bridge](./data-bridges.md#introduction-to-data-bridges)

::: tip
For now the dashboard doesn't support creating a standalone webhook without rules.
Please create standalone webhooks using config files.
:::
