# Rate Limit

EMQX allows for specifying limits on connection speed and messaging speed, using a backpressure scheme that avoids system overload at the entry point and guarantees system stability with predictable throughput.

## Limiter Types

EMQX uses the following types of limiters to specify the rate limits:

| Type          | Description                                                  | Post-Overload Behavior          |
| :------------ | :----------------------------------------------------------- | :------------------------------ |
| bytes_rate    | The size of messages in bytes published per second by a single client | Pause receiving client messages |
| messages_rate | The number of messages published per second by a single client | Pause receiving client messages |
| max_conn_rate | The number of connections per second for the current listener | Pause receiving new connections |

Limiters can operate at the listener level. You can set rate limits for each listener on the **Management** -> **Listeners** page in the Dashboard.

They can also be set through the configuration file. For example, to set limiters for the default TCP listener, configure it in the emqx.conf file as follows:

```bash
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_conn_rate = "1000/s"
  messages_rate = "1000/s"
  bytes_rate = "1MB/s"
}
```

This configuration implies:

- The maximum rate of connection establishment on the listener is 1000 per second
- The maximum publishing rate of messages is 1000 per second per client
- The maximum publishing rate of data is 1MB per second per client

## Rate Unit

### Time Unit

The supported time unit in the rate value could be:

- **s** : Second
- **m** : Minute
- **h** : Hour
- **d** : Day

The time unit also can be an interval value, like `1000/10s` means setting the limit to 1000 per every 10 seconds.

### Size Unit

The supported size unit in the rate value could be:

- **KB** : Kilobyte
- **MB** : Megabyte
- **GB** : Gigabyte

