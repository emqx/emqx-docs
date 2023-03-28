# Limiter

Limiter is a new feature introduced in EMQX 5.0, it is a mechanism to restrict the number of messages that a client or topic can publish or subscribe to in a specified time period. 

For the moment, you can restrict the message rates from the following perspectives:

| **Type**     | **Description**                           | **Recovery Behavior**           |
| ------------ | ----------------------------------------- | ------------------------------- |
| `bytes_in`   | Incoming message size in bytes per second | Pause receiving client messages |
| `message_in` | Incoming messages per second              | Pause receiving client messages |
| `connection` | Connections per second                    | Pause receiving new connections |

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

Limiter can work on the node level or the listener level. 

### Configure Limiter on Node Level

For example, to restrict the message received per second to 100 pieces, you can add the following configuration to `emqx.conf`:

```bash
1limiter.message_in.rate = "100/s"
```

### Confiture Limiter on Listener Level

For example, you want to configure a TCP listener on port `1883`, with a maximum 1,024,000 of concurrent connections allowed by the listener. And for this listener, if you want to restrict the message received per second to 100 pieces, you can configure it as follows:

```bash
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_connections = 1024000
  limiter.client.message_in {
  rate = "100/s"
  }
}
```

For more information on the Limiter and how it works, see [Rate limit](../rate-limit/rate-limit.md).