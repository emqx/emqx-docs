# EMQX 6.0 中的不兼容变更

## 6.0.4

- [#18515](https://github.com/emqx/emqx/pull/18515) Azure Blob Storage 动作的 `blob` 模板字段现在使用与 Aggregated S3 动作的 `key` 字段相同的 Schema 验证，以确保仅使用允许的绑定。

- [#18528](https://github.com/emqx/emqx/pull/18528) OpenTelemetry 集成的导出器端点现在必须是包含协议方案和明确指定端口的有效 URL。支持的协议方案为 `http` 和 `https`。

- [#18974](https://github.com/emqx/emqx/pull/18974) 新增配置项 `mqtt.max_connect_user_properties`，分别限制 CONNECT 属性和 Will 属性中允许的 MQTT v5 User Property 对数量。默认值为 100；设置为 `infinity` 可禁用此限制。

## 6.0.3

- [#17157](https://github.com/emqx/emqx/pull/17157) 新增规则引擎配置项 `rule_engine.limit_selects_in_namespace`，默认值为 `true`。启用后，属于某个命名空间的规则只会被同一命名空间内客户端产生的消息和客户端相关事件触发。

## 6.0.1

- [#16061](https://github.com/emqx/emqx/pull/16061) 修复了一个问题：RocketMQ 动作忽略了配置的 payload 模板，错误地发送了整个规则的输出结果。

  如果您依赖了此前（错误）行为，可能需要更新 payload 模板，以确保消息格式符合预期。
