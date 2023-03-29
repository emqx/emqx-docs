# Flapping



Flapping in EMQX refers to a situation where an MQTT client rapidly connects and disconnects from the broker in a short period of time. The flapping mechanism is to detect flapping clients and to disconnect the client from a connection. 

:::tip

Most configuration items listed here also can be configured with Dashboard, and their Dashboard UI fields are also introduced in this page. Once you configured these items with the Dashboard, your settings will override the same configuration items in `emqx.conf`.

:::

`flapping_detect` is to enable the flapping connection detection feature, then you can continue to customize the settings with the following configuration items:

- `max_count` is to set the maximum number of connection attempts allowed from a client within a specified time window (defined by window_time)
- `window_time` is to set the time window in which the maximum number of connection attempts from a client are counted.
- `ban_time` is to set the duration for which a client is banned from connecting to the EMQX after after it has been detected as flapping. 

For example, if you want to mark clients that make 15 connection attempts within a 1-minute window as flapping clients, and then ban the detected clients from connecting to EMQX for 5 minutes, you can work with the code below:

```bash
flapping_detect {
  max_count  =  15
  window_time  =  1m
  ban_time  =  5m
}
```

