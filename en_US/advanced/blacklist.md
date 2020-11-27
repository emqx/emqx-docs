# Blacklist

EMQ X Broker provides users with a blacklist function. Users can add specified clients to the blacklist through the relevant HTTP API to deny access of the client. In addition to the client identifier, it also supports direct ban of user names and even IP addresses.

For specific usage of the HTTP API, see  [HTTP API - blacklist](http-api.md#endpoint-banned) for reference.

::: tip
The blacklist is only applicable to a small number of client bans. If there are a large number of clients requiring authentication management, please use the [authentication](./auth.md)  function.。
:::

Based on the blacklist function, EMQ X Broker supports automatic banning of clients that are frequently logged in for a short period of time, and rejects these clients for a period of time to prevent such clients from occupying server resources so that  the normal use of other clients is affected.

It should be noted that the automatic ban function only bans the client identifier, not the user name and IP address. That is to say, the machine can continue to log in as long as the client identifier is changed.

This function is disabled by default, and users can set the `enable_flapping_detect` configuration item to `on` in the `emqx.conf` configuration file to enable this function.

```bash
zone.external.enable_flapping_detect = off
```

The user can adjust the trigger threshold and the ban time for this function. The corresponding configuration items are as follows:

```bash
flapping_detect_policy = 30, 1m, 5m
```

The value of this configuration item is separated by `,`, which in turn indicate the number of times that the client is offline, the detection time range, and the ban time. Therefore, the above default configuration means that if the client goes offline 30 times in 1 minute, the client's 'identifier will be banned for 5 minutes. Of course, you can also use other time units such as seconds and hours. For this part, please refer to [Configuration Instructions](../getting-started/config.md#).