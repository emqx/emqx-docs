# Incompatible Changes in EMQX 5.10

## e5.10.0

## e5.10.0

- [#15289](https://github.com/emqx/emqx/pull/15289) Added a new `resource_opts.health_check_timeout` configuration to all Connectors, Actions and Sources, with default value of 60 seconds.  If a health check takes more than this to return a response, the Connector/Action/Source will be deemed `disconnected`.

  Note: since the default is 60 seconds, this means that if a Connector/Action/Source previously could take more than that to return a healthy response, now it'll be deemed disconnected in such situations.

- [#15286](https://github.com/emqx/emqx/pull/15286) Configuration option `broker.routing.storage_schema` is now deprecated and ignored. Legacy `v1` routing storage schema is no longer supported, and EMQX will refuse to start in a cluster running running older versions that still use it.

- [#15239](https://github.com/emqx/emqx/pull/15239) The type for the `multi_tenancy.default_max_sessions` is now either `infinity` or a positive integer.  Previously, `0` would be accepted.

- [#15156](https://github.com/emqx/emqx/pull/15156) Schema validation was added to `dashboard.sso.oidc.issuer` field.  Now, this value is checked to be a valid URL.


