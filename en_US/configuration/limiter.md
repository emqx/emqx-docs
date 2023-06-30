# Rate Limiter

Limiter is a new feature introduced in EMQX 5.0, it is a mechanism to restrict the number of messages that a client or topic can publish or subscribe to in a specified time period. For more information on the Limiter and how it works, see [Rate limit](../rate-limit/rate-limit.md). 

For the moment, you can restrict the message rates from the following perspectives:

| **Type**        | Dashboard UI            | **Description**                           | **Recovery Behavior**           |
| --------------- | ----------------------- | ----------------------------------------- | ------------------------------- |
| `bytes_rate`    | Data Publish Rate per Client       | Incoming message size in bytes per second per client | Pause receiving client messages |
| `messages_rate`  | Messages Publish Rate per Client   | Incoming messages per second  per client | Pause receiving client messages |
| `max_conn_rate` | Maximum Connection Rate per Listener | Connections per second per listener | Pause receiving new connections |

For example, to set a Limiter for the default TCP listener, you can work with the code below:

```bash
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_conn_rate = "1000/s"
  messages_rate = "1000/s"
  bytes_rate = "1MB/s"
}
```

:::tip

EMQX has offered more configuration items to better serve customized needs, you can continue to read [Configuration Manual](./configuration-manual.html).

:::
