# Rate Limiter

Limiter is a new feature introduced in EMQX 5.0, it is a mechanism to restrict the number of messages that a client or topic can publish or subscribe to in a specified time period. For more information on the Limiter and how it works, see [Rate limit](../rate-limit/rate-limit.md). 

For the moment, you can restrict the message rates from the following perspectives:

| **Type**     | Dashboard UI | **Description**                           | **Recovery Behavior**           |
| ------------ | ------------ | ----------------------------------------- | ------------------------------- |
| `bytes_in`   | Bytes In     | Incoming message size in bytes per second | Pause receiving client messages |
| `message_in` | Message In   | Incoming messages per second              | Pause receiving client messages |
| `connection` | Connection   | Connections per second                    | Pause receiving new connections |

The limiter can be set both on EMQX or the client side, as shown below: 

```bash
limiter {
  bytes_in.rate  =  infinity
  message_in.rate  =  infinity
  connection.rate  =  infinity
  
  client.bytes_in.rate = infinity
  client.message_in.rate = infinity
  client.connection.rate = infinity
}
```

Limiter can work on the node level or the listener level, for example, to set a Limiter for the default TCP listener, you can work with the code below:

```bash
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_connections = 1024000
  limiter.client.message_in {
  rate = "100/s"
  }
}
```

{% emqxce %}

:::tip

EMQX has offered more configuration items to serve customized needs better. For details, see [Configuration Manual](https://www.emqx.io/docs/en/v${CE_VERSION}/hocon/).

:::

{% endemqxce %}

{% emqxee %}

:::tip

EMQX has offered more configuration items to serve customized needs better. For details, see [Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/).

:::

{% endemqxee %}