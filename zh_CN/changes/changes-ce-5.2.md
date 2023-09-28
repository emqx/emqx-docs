# 版本发布

## v5.2.0

### 增强

- [#11469](https://github.com/emqx/emqx/pull/11469) 支持在 Redis 认证中指定用户名。
- [#11487](https://github.com/emqx/emqx/pull/11487) 将 bcrypt 的工作因子 (work factor) 限制在5-10的范围内，因为较高的值会消耗太多 CPU 资源。Bcrypt 库已更新以允许并行哈希计算。
- [#11496](https://github.com/emqx/emqx/pull/11496) 默认情况下禁用了 Erlang VM Prometheus 导出器，以提高性能和安全性。
- [#11497](https://github.com/emqx/emqx/pull/11497) 通过增加有关消息、过载保护、授权和身份验证的新指标，并改善 OpenTelemetry 的命名一致性，增强了 MQTT 服务器的指标收集和导出功能。
- [#11490](https://github.com/emqx/emqx/pull/11490) 在各种身份验证后端中添加了对未定义密码的快速错误处理。这提高了身份验证过程的一致性和用户友好性。
- [#11532](https://github.com/emqx/emqx/pull/11532) 改进了解析无效数据包时的错误消息，以提供更清晰的错误提示。
- [#11568](https://github.com/emqx/emqx/pull/11568) 在消息重发布规则动作中，支持设置 MQTT 5.0 发布属性与用户属性。

### 修复

- [#11466](https://github.com/emqx/emqx/pull/11466) 修复了将 `ssl_options.ciphers` 配置选项设置为空字符串（""）时导致崩溃的问题。

- [#11480](https://github.com/emqx/emqx/pull/11480) 改进了在规则引擎中 SQL 函数对接收到错误参数时的错误处理和测试。

- [#11493](https://github.com/emqx/emqx/pull/11493) 修复了REST API 示例文档中关于 `/api/v5/publish` 错误请求响应的描述。之前的文档示例指出错误请求的响应可以在响应体中返回一个列表，但实际情况并非如此。

- [#11506](https://github.com/emqx/emqx/pull/11506) 此前尝试下载不存在的跟踪日志文件时，会下载一个空的文件。在实施此修复后，尝试使用 GET 请求 `/api/v5/trace/clientempty/download` 下载不存在的跟踪日志文件时，服务器现在将返回 404 状态码以及以下 JSON 消息：`{"code":"NOT_FOUND","message":"Trace is empty"}`。

- [#11520](https://github.com/emqx/emqx/pull/11520) 修复了一个问题，该问题导致在发送带有非零 `ack_flag` 的 CONNACK 数据包时，`packets_connack_sent` 指标没有增加。

- [#11522](https://github.com/emqx/emqx/pull/11522) 在规则引擎的编解码功能中，改进了当 schema 名称超出允许的长度时出现的错误消息。

- [#11523](https://github.com/emqx/emqx/pull/11523) 修正了在为 `/configs` API 指定无效证书/密钥时的出现的误导性提示。

- [#11531](https://github.com/emqx/emqx/pull/11531) 修复了针对某个特定的客户端 ID，授权缓存清理 CLI 无法正常工作的问题。

- [#11534](https://github.com/emqx/emqx/pull/11534) 修复了在桥接状态不健康时增加数据桥接统计的问题。现在，发送到不健康状态下的桥接的消息将被计算为丢弃的消息。

- [#11540](https://github.com/emqx/emqx/pull/11540) 改进了针对尝试创建一个具有无效名称的数据桥接的 HTTP 响应。

- [#11548](https://github.com/emqx/emqx/pull/11548) 修复了插件顺序无法在整个集群中更新的问题。

- [#11564](https://github.com/emqx/emqx/pull/11564) 修复了集群分区自动恢复功能。实施了对分裂成多个分区的集群的自动恢复。

- [#11568](https://github.com/emqx/emqx/pull/11568) 修复了一个未明确定义的内置规则动作配置，以避免该配置被理解为自定义用户函数。

- [#11586](https://github.com/emqx/emqx/pull/11586) 修复了当从当前版本滚动升级到 `v5.2.0` 时会导致在会话接管过程中系统奔溃的问题。

  

