# EMQX 5.7 中的不兼容变更

{% emqxce %}

## v5.7.0


{% endemqxce %}

{% emqxee %}

## e5.7.0

- [#12947](https://github.com/emqx/emqx/pull/12947) 对于 JWT 认证，支持新的 `disconnect_after_expire` 选项。启用时，客户端将在 JWT token 过期后断开连接。此选项默认启用，因此默认行为已更改。以前，带有实际 JWT 的客户端可以连接到服务器并在 JWT token 过期后保持连接。现在，客户端将在 JWT token 过期后断开连接。要保留以前的行为，请将 `disconnect_after_expire` 设置为 `false`。
- [#12895](https://github.com/emqx/emqx/pull/12895) 为 DynamoDB 连接器和动作添加了一些之前遗漏但必要的关键配置。尽管在此修复之前旧配置并不正确，但现在已不再适用。对于 DynamoDB 连接器，现在需要新增的关键字是 `region`。在 DynamoDB 动作中，现在支持 `hash_key` 和 `range_key`，其中 `hash_key` 是必须的。
- [#12957](https://github.com/emqx/emqx/pull/12957) 停止为 macOS 12 构建软件包。

{% endemqxee %}
