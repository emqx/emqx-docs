# EMQX 5.7 中的不兼容变更

## v5.7.0

- [#12947](https://github.com/emqx/emqx/pull/12947) 对于 JWT 认证，支持新的 `disconnect_after_expire` 选项。启用时，客户端将在 JWT token 过期后断开连接。此选项默认启用，因此默认行为已更改。

  以前，带有实际 JWT 的客户端可以连接到服务器并在 JWT token 过期后保持连接。现在，客户端将在 JWT token 过期后断开连接。要保留以前的行为，请将 `disconnect_after_expire` 设置为 `false`。

- [#12957](https://github.com/emqx/emqx/pull/12957) 停止为 macOS 12 构建软件包。
