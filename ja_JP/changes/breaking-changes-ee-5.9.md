# Incompatible Changes in EMQX 5.9

## 5.9.0

- [#14865](https://github.com/emqx/emqx/pull/14865) Dropped old LDAP authentication config layout (deprecated since v5.4).
  Move `password_attribute` and `is_superuser_attribute` under the `method` block:
    ```hcl
    method {
      type = hash
      password_attribute = "userPassword"
      is_superuser_attribute = "isSuperuser"
    }
    ```

- [#14765](https://github.com/emqx/emqx/pull/14765) Added extra validation for using Named Instances in SQL Server Connector.  Previously, we could not infer when the user furnished an explicit port for SQL Server, and always added the default port if not explicitly defined.

  For Named Instances, we need to explicitly define a port to connect to when connecting with the ODBC driver. And the driver happily connects to whatever instance is running on that port, completely ignoring the given Instance Name, if any.

  Now, we impose that the port is to be explicitly defined when an instance name is given, and we also attempt to infer differences between desired and connected instance names during health checks.

- [#14773](https://github.com/emqx/emqx/pull/14773) Rate limiting configuration options have been changed.
  - This change is incompatible with versions prior to 5.1.0
  - This change is also incompatible with manually modified limiter configurations that use structures from versions prior to 5.1.0
  - The undocumented endpoint `/configs/limiter` has been removed
  
- [#14703](https://github.com/emqx/emqx/pull/14703) Changed the maximum allowed value for `force_shutdown.max_heap_size` to `128GB`.

- [#14957](https://github.com/emqx/emqx/pull/14957) The way plugin configurations are updated has changed. The system now respects the result of the `on_config_changed` callback when updating a plugin's configuration. This change only affects new configuration updates made through the Dashboard. The result of the `on_config_changed` callback is still ignored for configurations that have already been stored in the cluster.

  Additionally, plugin apps are now loaded during plugin installation to ensure the `on_config_changed` callback is called even for stopped plugins.

