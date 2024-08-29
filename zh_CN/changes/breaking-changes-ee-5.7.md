# EMQX 5.7 中的不兼容变更

## e5.7.2

- [#13327](https://github.com/emqx/emqx/pull/13327) 对 Kafka、Confluent 和 Azure Event Hubs 集成中问题的修复导致磁盘缓冲区目录结构被更改。新结构使用动作名称代替主题名称。升级到此版本将使旧缓冲区文件无效，需要手动清理旧目录。
- [#13332](https://github.com/emqx/emqx/pull/13332) 对于配置错误的 Amazon S3 集成，错误信息现在更加详细易懂。具有无效对象键模板的 Amazon S3 Sink 配置将不再起作用。在此更改之前，此类配置被视为有效，但集成实际上无法正常工作。
- [#13420](https://github.com/emqx/emqx/pull/13420) 增加了对 Schema 验证配置的校验，以防止为 Schema 验证配置空主题过滤器列表。任何此类配置都必须定义至少一个主题过滤器才能有效。不过，此类配置可能非常罕见，因为具有空主题的 Schema 验证本质上等同于没有验证。

## e5.7.0

- [#12947](https://github.com/emqx/emqx/pull/12947) 对于 JWT 认证，支持新的 `disconnect_after_expire` 选项。启用时，客户端将在 JWT token 过期后断开连接。此选项默认启用，因此默认行为已更改。

  以前，带有实际 JWT 的客户端可以连接到服务器并在 JWT token 过期后保持连接。现在，客户端将在 JWT token 过期后断开连接。要保留以前的行为，请将 `disconnect_after_expire` 设置为 `false`。

- [#12957](https://github.com/emqx/emqx/pull/12957) 停止为 macOS 12 构建软件包。

- [#12895](https://github.com/emqx/emqx/pull/12895) 为 DynamoDB 连接器和动作添加了一些之前遗漏但必要的关键配置。尽管在此修复之前旧配置并不正确，但现在已不再适用。对于 DynamoDB 连接器，现在需要新增的关键字是 `region`。在 DynamoDB 动作中，现在支持 `hash_key` 和 `range_key`，其中 `hash_key` 是必须的。
