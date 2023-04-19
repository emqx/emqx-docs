# Rate limit

EMQX can specify the limit on access speed and message speed, this is a backpressure scheme that avoids system overload from the entrance and guarantees system stability and predictable throughput.

## Limiter Types

EMQX limiter currently supports the following types:

| Type              | Description                                             | Post-Overload Behavior            |
| :---------------- | :------------------------------------------------------ | :-------------------------------- |
| bytes_rate        | Incoming message size in bytes per second               | Pause receiving client messages   |
| messages_rate     | Incoming messages per second                            | Pause receiving client messages   |
| max_conn_rate     | Connections per second                                  | Pause for new connections         |

## Useage
                                      
### Setup Limiter for a Listener

It's easy to set limiters for listeners by adding the type directly to the listener configuration.
 
```bash
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_connections = 1024000
  ## Set the limit of connection rate for this listener to 1000 per second
  max_conn_rate = "1000/s"
  ## Set the limit of inbound message numbers per second for each client connected to this listener to 1000
  messages_rate = "1000/s"
  ## Set the limit of inbound message size per second for each client connected to this listener to 1000M
  bytes_rate = "1000MB/s"
}
```

### Setup Limiter for the Node

It's also possible and easy to set a limit for the whole node, which needs the below section into the `emqx.conf`.

```bash
limiter {
  ## Set the limit of connection rate for the current node
  max_conn_rate = "1000/s"
  ## Set the limit of inbound message numbers per second for the current node
  messages_rate = "1000/s"
  ## Set the limit of inbound message size per second for the current node
  bytes_rate = "1000MB/s"
}
```

## Rate Unit

### Time Unit

The supported time unit in the rate value could be:

- **s** :: Second
- **m** :: Minute
- **h** :: Hour
- **d** :: Day

The time unit also can be an interval value, like `1000/10s` means setting the limit to 1000 per every 10 seconds.

### Size Unit

The supported size unit in the rate value could be:

- **KB** :: Kilobyte
- **MB** :: Megabyte
- **GB** :: Gigabyte

