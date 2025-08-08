# EMQX 6.0 不兼容变更

## e6.0.0

- [#15613](https://github.com/emqx/emqx/pull/15613) 停止发布 Debian 10 软件包。

- [#15635](https://github.com/emqx/emqx/pull/15635) 我们不再支持在 RocketMQ 动作的 `parameters.strategy` 字段中设置键模板（从而隐式指定键分发策略）。相反，用户应该设置 `parameters.strategy = key_dispatch` 并在 `parameters.key` 中指定模板。