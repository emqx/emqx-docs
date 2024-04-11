# Rate Limiter Configuration

Limiter is a new feature introduced in EMQX 5.0, it is a mechanism to restrict the number of messages that a client or topic can publish or subscribe to in a specified time. For more information on the Limiter and how it works, see [Rate Limit](../rate-limit/rate-limit.md). 

For the moment, you can restrict the message rates from the following perspectives:

| **Type**        | Dashboard UI            | **Description**                           | **Recovery Behavior**           |
| --------------- | ----------------------- | ----------------------------------------- | ------------------------------- |
| `bytes_rate`    | Data Publish Rate       | Incoming message size in bytes per second per client | Pause receiving client messages |
| `messages_rate`  | Messages Publish Rate   | Incoming messages per second  per client | Pause receiving client messages |
| `max_conn_rate` | Maximum Connection Rate | Connections per second per listener | Pause receiving new connections |

For example, to set a limiter for the default TCP listener, you can use the configuration below:

```bash
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_conn_rate = "1000/s"
  messages_rate = "1000/s"
  bytes_rate = "1MB/s"
}
```

{% emqxce %}

:::tip

EMQX has offered more configuration items to serve customized needs better. For details, see [Configuration Manual](https://www.emqx.io/docs/en/v@CE_VERSION@/hocon/).

:::

{% endemqxce %}

{% emqxee %}

:::tip

EMQX has offered more configuration items to serve customized needs better. For details, see [Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/).

:::

{% endemqxee %}
