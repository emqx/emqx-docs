# EMQX 5.9 中的不兼容变更

## 5.9.2

- [#15753](https://github.com/emqx/emqx/pull/15753) 监听器连接速率限制（`max_conn_rate` 和 `max_conn_burst`）现在按监听器维度生效，而不再是按 acceptor （连接接收进程）生效，恢复了 5.9.0 之前的行为。

  这导致 5.9.0 和 5.9.1 版本的相关配置与当前版本不兼容。为了保持相同的限流效果，指定的速率必须按相应监听器配置的 acceptor 数量进行放大。

- [#16062](https://github.com/emqx/emqx/pull/16062) 修复了一个问题：RocketMQ 动作忽略了配置的 payload 模板，错误地发送了整个规则的输出结果。

  如果您依赖了此前（错误）行为，可能需要更新 payload 模板，以确保消息格式符合预期。

- [#16284](https://github.com/emqx/emqx/pull/16284) 停止发布 macOS 13 和 CentOS 7 安装包。

## 5.9.1

- [#15156](https://github.com/emqx/emqx/pull/15156) 为 `dashboard.sso.oidc.issuer` 字段新增了严格的 schema 校验。该字段现在必须为合法的 URL。此前，即使配置无效，API 也可能错误地接受，但会导致 EMQX 无法重启，甚至触发崩溃（`erl_crash.dump`）。

## 5.9.0

- [#14865](https://github.com/emqx/emqx/pull/14865) 删除了旧的 LDAP 认证配置布局（自 v5.4 起已废弃）。将 `password_attribute` 和 `is_superuser_attribute` 移动到 `method` 块下：

    ```hcl
  method {
    type = hash
    password_attribute = "userPassword"
    is_superuser_attribute = "isSuperuser"
  }
  ```

- [#14765](https://github.com/emqx/emqx/pull/14765)

- 为 SQL Server 连接器中的命名实例使用添加了额外的验证。之前，当用户提供了 SQL Server 的显式端口时，我们无法推断，且如果没有显式定义端口，始终添加默认端口。

  对于命名实例，我们需要在使用 ODBC 驱动连接时显式定义端口。并且如果指定了实例名称，驱动程序会忽略给定的实例名称，直接连接到该端口上运行的任何实例。

  现在，当提供实例名称时，我们强制要求显式定义端口，并且在健康检查过程中，我们还尝试推断所需实例名称与已连接实例名称之间的差异。

- [#14773](https://github.com/emqx/emqx/pull/14773) 速率限制相关配置项已发生变更。

  - 此更改与 5.1.0 之前的版本不兼容。
  - 此更改也与使用旧版本（5.1.0 之前）配置结构手动修改过的 limiter 配置不兼容。
  - 已移除未公开的配置接口 `/configs/limiter`。

- [#14703](https://github.com/emqx/emqx/pull/14703) `force_shutdown.max_heap_size` 的最大允许值已更改为 `128GB`。

- [#14957](https://github.com/emqx/emqx/pull/14957) 插件配置更新的方式已发生变化。系统现在会在更新插件配置时考虑 `on_config_changed` 回调的返回结果。此更改仅影响通过 Dashboard 进行的新配置更新。对于已经存储在集群中的配置，`on_config_changed` 回调的结果仍然会被忽略。

  此外，现在插件安装过程中会加载插件应用，以确保即使是已停止的插件也能调用 `on_config_changed` 回调。
