# Telemetry

::: tip
Telemetry was added to EMQ X in version 4.2-rc.1.
:::

EMQ collects information about the usage of EMQ X through telemetry. This function is designed to provide us with comprehensive information about users and communities, as well as an understanding of how EMQ X is used. Sharing these metrics with us can help us better understand how you use our products and can continuously help us improve our products.

These statistics do not contain individual data, such as server model, hardware number, IP address, and will never be shared with anyone else.

### Telemetry data example

Telemetry reports data to by encrypting HTTP requests https://telemetry.emqx.io/api/telemetry. If you have any questions, please click [Issues](http://github.com/emqx/emqx/issues) to contact us:

```json
{
  "license": {
    "edition": "community"
  },
  "nodes_uuid": [],
  "active_plugins": [
    "emqx_telemetry",
    "emqx_retainer",
    "emqx_recon",
    "emqx_management",
    "emqx_dashboard"
  ],
  "active_modules": [
    "emqx_mod_presence",
    "emqx_mod_acl_internal"
  ],
  "num_clients": 0,
  "messages_received": 0,
  "messages_sent": 0,
  "emqx_version": "4.2-rc.1",
  "os_name": "Ubuntu",
  "os_version": "16.04.6 LTS (Xenial Xerus)",
  "otp_version": "22",
  "up_time": 7081,
  "uuid": "D6138FCE-E455-11EA-A854-FD8F5F98125F"
}
```

**Telemetry is enabled by default**. Any people may feel uncomfortable collecting such data. You can disable it in the simplest way before startup and during operation.

### Disable before start EMQ X

Edit `data/loaded_plugins` file, remove `{emqx_telemetry, true}.` line. The telemetry plugin will never loaded automatically.

### Disable in running

Stop the emqx-telemetry plugins：`./bin/emqx_ctl plugins unload emqx_telemetry`

Stop the emqx-telemetry plugin in the dashboard：Open http://localhost:18083 enter the dashboard, enter the plugin page, find `emqx-telemetry`, and click stop button.

### Enable again

Start by enabling the plugin.
