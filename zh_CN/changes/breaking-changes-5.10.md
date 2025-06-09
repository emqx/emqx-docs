# EMQX 5.10 中的不兼容变更

## 5.10.0

- [#15289](https://github.com/emqx/emqx/pull/15289) 为所有连接器、动作和数据源新增配置项 `resource_opts.health_check_timeout`，默认值为 60 秒。
  如果健康检查在该时间内未返回结果，则该连接器、动作或数据源将被视为“已断开”。

  注意：由于默认值为 60 秒，如果之前某些连接器/动作/数据源的健康响应时间超过 60 秒，在当前版本中将被视为断开。

- [#15286](https://github.com/emqx/emqx/pull/15286) 配置项 `broker.routing.storage_schema` 现已废弃且不再生效。旧版 `v1` 路由表已不再支持，EMQX 将拒绝在使用该路由表的旧版本集群中启动。有关如何升级使用 `v1` 路由表的集群，参考 [EMQX 5.10 或更高版本的滚动升级注意事项](../deploy/rolling-upgrades.md#emqx-5.10-或更高版本的滚动升级注意事项)。

- [#15239](https://github.com/emqx/emqx/pull/15239) 配置项 `multi_tenancy.default_max_sessions` 的类型现为 `infinity` 或正整数。 此前允许配置为 `0`，现已不再支持。

- [#15156](https://github.com/emqx/emqx/pull/15156) 为配置字段 `dashboard.sso.oidc.issuer` 添加了 Schema 校验，现在该字段必须为合法的 URL。

