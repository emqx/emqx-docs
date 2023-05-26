# Rate limit

EMQX allows for specifying limits on connection speed and messaging speed, using a backpressure scheme that avoids system overload at the entry point and guarantees system stability with predictable throughput.

## Limiter Types

EMQX uses the following types of limiters to specify the rate limits:

| Type          | Description                               | Post-Overload Behavior          |
| :------------ | :---------------------------------------- | :------------------------------ |
| bytes_rate    | Incoming message size in bytes per second | Pause receiving client messages |
| messages_rate | Incoming messages per second              | Pause receiving client messages |
| max_conn_rate | Connections per second                    | Pause receiving new connections |

## Configure Limiter on Listener

Limiter can work on the listener level. For example, to set a Limiter for the default TCP listener, you can work with the code below:

```bash
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  ## Set the limit of connection rate for this listener to 1000 per second
  max_conn_rate = "1000/s"
  ## Set the limit of incoming message numbers per second for each client connected to this listener to 1000
  messages_rate = "1000/s"
  ## Set the limit of incoming message size per second for each client connected to this listener to 1000M
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

