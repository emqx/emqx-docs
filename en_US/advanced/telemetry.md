# Telemetry

::: tip
Telemetry was added to EMQX in version 4.2-rc.1.
:::

EMQ collects information about the usage of EMQX through telemetry. This function is designed to provide us with comprehensive information about users and communities, as well as an understanding of how EMQX is used. Sharing these metrics with us can help us better understand how you use our products and can continuously help us improve our products.

These statistics do not contain individual data, such as server model, hardware number, IP address, and will never be shared with anyone else.

## Telemetry data example

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

## Disable before start EMQX

Edit `data/loaded_plugins` file, and remove the line of emqx_telemetry. (Note that there is a "." character at the end of the line）, which means that the telemetry plug-in will never start.

## Disable in running

Stop the emqx_telemetry plugin in the command line: ./bin/emqx_ctl plugins unload emqx_telemetry

Stop emqx_telemetry plugin in the Dashboard: Open http://localhost:18083 to enter Dashboard, enter the plugin page, find emqx_telemetry and click Stop button.