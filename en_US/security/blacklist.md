# Blacklisting/Banning ClientID, user name or IP

EMQX Broker provides users with a blacklisting/banning functionality.

Administrator can add specified clients to the blacklist from the management HTTP API
to deny access of the client.

In addition to the client identifier, it also supports direct ban of user names or source IP addresses.

For specific usage of the HTTP API, see  the `/banned` API document.

::: tip
The blacklist is only applicable to a small number of client bans.
If there are a large number of clients requiring authentication management,
please use the [authentication](./auth.md)  function.。
:::

Based on the blacklist function, EMQX supports automatic banning of clients that are frequently
logged in for a short period of time, and rejects these clients for a period of time
to prevent such clients from consuming server resources which in turn may affect other clients.

It should be noted that the automatic ban only bans the client identifier,
not the user name and IP address.
That is to say, a malicious client may still able to attack if they change client identifier for each attempt.

This feature is disabled by default, set `enable_flapping_detect` configuration item to `on` in `emqx.conf` to enable it.

```bash
zone.external.enable_flapping_detect = off
```

The user can adjust the trigger threshold and the ban time with below configs

```bash
flapping_detect_policy = "30, 1m, 5m"
```

The value of this configuration item is separated by `,`,
which repectively indicates the number of times that the client is offline,
the detection time range, and the ban time.

In the above example, the config means that if a client goes offline 30 times in 1 minute,
then this client identifie is banned for 5 minutes.
