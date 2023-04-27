# Flapping

Flapping in EMQX refers to a situation where an MQTT client rapidly connects and disconnects from the broker in a short period of time. The flapping mechanism is to detect flapping clients and to disconnect the client from a connection. 

For example, if you want to mark clients that make 15 connection attempts within a 1-minute window as flapping clients, and then ban the detected clients from connecting to EMQX for 5 minutes, you can work with the code below:

```bash
flapping_detect {
  max_count  =  15
  window_time  =  1m
  ban_time  =  5m
}
```

Where, <!--did not add the dashboard UI as it seems that most configurations listed here cannot be configured with the UI-->

- `max_count` is to set the maximum number of connection attempts allowed from a client within a specified time window (defined by window_time)
- `window_time` is to set the time window in which the maximum number of connection attempts from a client are counted.
- `ban_time` is to set the duration for which a client is banned from connecting to the EMQX after it has been detected as flapping. 

:::tip

To customize the settings via Dashboard, click **Access Control** -> **Blacklist** on the left navigation menu of the Dashboard. Once you configured these items with Dashboard, your settings will override the same configuration items in `emqx.conf`.

EMQX has offered more configuration items to better serve customized needs, you can continue to read [Configuration Manual](./configuration-manual.md).

:::