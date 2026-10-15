# Incompatible Changes in EMQX 6.0

## 6.0.4

- [#18515](https://github.com/emqx/emqx/pull/18515) Azure Blob Storage Action's `blob` template field now has the same schema validation as Aggregated S3's `key`, which verifies the allowed bindings are followed.

- [#18528](https://github.com/emqx/emqx/pull/18528) The OpenTelemetry integration exporter endpoint must now be a valid URL with a scheme and explicit port. Supported schemes are `http` and `https`. The port must be explicitly set.

- [#18974](https://github.com/emqx/emqx/pull/18974) Added `mqtt.max_connect_user_properties`, which limits the number of MQTT v5 User Property pairs accepted separately in CONNECT properties and Will properties. The default is 100; set it to `infinity` to disable the limit.

## 6.0.3

- [#17157](https://github.com/emqx/emqx/pull/17157) Added a Rule Engine configuration, `rule_engine.limit_selects_in_namespace`, which defaults to `true`. When enabled, rules that belong to a namespace are triggered only by messages and client-related events from clients in that same namespace.

## 6.0.1

- [#16061](https://github.com/emqx/emqx/pull/16061) Fixed an issue where RocketMQ actions ignored the configured payload template and sent the entire rule output instead.

  If you relied on the previous (incorrect) behavior, you may need to update your payload templates to ensure messages are formatted as expected.
