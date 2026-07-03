# Incompatible Changes in EMQX 6.0

## 6.0.3

- [#17157](https://github.com/emqx/emqx/pull/17157) Introduced a new Rule Engine configuration, `rule_engine.limit_selects_in_namespace`, which defaults to `true`. When enabled, rules are triggered only by messages published by clients in the same namespace as the rule.

## 6.0.1

- [#16061](https://github.com/emqx/emqx/pull/16061) Fixed an issue where RocketMQ actions ignored the configured payload template and sent the entire rule output instead.

  If you relied on the previous (incorrect) behavior, you may need to update your payload templates to ensure messages are formatted as expected.
