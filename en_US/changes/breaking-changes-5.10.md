# Incompatible Changes in EMQX 5.10

## 5.10.1

- [#15752](https://github.com/emqx/emqx/pull/15752) Listener connection rate limits (`max_conn_rate` and `max_conn_burst`) are now enforced per listener rather than per acceptor, restoring the pre-5.9.0 behavior. As a result, configurations from versions 5.9.0, 5.9.1, and 5.10.0 are incompatible: specified rates must be scaled up by the number of acceptors configured for respective listeners.

- [#15753](https://github.com/emqx/emqx/pull/15753) Listener connection rate limits (`max_conn_rate` and `max_conn_burst`) are now enforced per listener rather than per acceptor, restoring the pre-5.9.0 behavior. As a result, configurations from versions 5.9.0 and 5.9.1 are incompatible: specified rates must be scaled up by the number of acceptors configured for respective listeners.

## 5.10.0

- [#15289](https://github.com/emqx/emqx/pull/15289) Added a new `resource_opts.health_check_timeout` configuration to all Connectors, Actions, and Sources, with a default value of 60 seconds.  If a health check takes more than this to return a response, the Connector/Action/Source will be deemed `disconnected`.

  Note: since the default is 60 seconds, this means that if a Connector/Action/Source previously could take more than that to return a healthy response, now it'll be deemed disconnected in such situations.

- [#15286](https://github.com/emqx/emqx/pull/15286) Configuration option `broker.routing.storage_schema` is now deprecated and ignored. Legacy `v1` routing storage schema is no longer supported, and EMQX will refuse to start in a cluster running older versions that still use it. For instructions on upgrading a cluster that uses `v1` routing schema, see [Rolling Upgrade Considerations for EMQX 5.10 or Later](../deploy/rolling-upgrades.md#rolling-upgrade-considerations-for-emqx-5.10-or-later). 

- [#15239](https://github.com/emqx/emqx/pull/15239) The type for the `multi_tenancy.default_max_sessions` is now either `infinity` or a positive integer.  Previously, `0` would be accepted.

- [#15156](https://github.com/emqx/emqx/pull/15156) Schema validation was added to `dashboard.sso.oidc.issuer` field.  Now, this value is checked to be a valid URL.

