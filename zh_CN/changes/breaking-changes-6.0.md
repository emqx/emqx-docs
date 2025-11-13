# EMQX 6.0 中的不兼容变更

## 6.0.1

- [#16061](https://github.com/emqx/emqx/pull/16061) 修复了一个问题：RocketMQ 动作忽略了配置的 payload 模板，错误地发送了整个规则的输出结果。

  如果您依赖了此前（错误）行为，可能需要更新 payload 模板，以确保消息格式符合预期。