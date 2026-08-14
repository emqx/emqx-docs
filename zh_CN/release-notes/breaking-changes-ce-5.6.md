# EMQX 5.6 中的不兼容变更

## v5.6.0

- [#12576](https://github.com/emqx/emqx/pull/12576) 从 5.6 版本开始，“配置手册”文档将不再包括 `bridges` 配置根。

  现在，`bridge` 要么是出口数据集成的 `action` + `connector`，要么是入口数据集成的 `source` + `connector`。请注意，`bridges` 配置（在 `cluster.hocon` 中）和 REST API 路径 `api/v5/bridges` 仍然有效，但被视为已弃用。

- [#12634](https://github.com/emqx/emqx/pull/12634) HOCON 配置文件中的三引号字符串值不再支持转义序列。

  详细信息可以在[此拉取请求](https://github.com/emqx/hocon/pull/290)中找到。这里是对 EMQX 用户影响的总结：

  - EMQX 5.6 是第一个在 `cluster.hocon` 中生成三引号字符串的版本，意味着对于生成的配置，不存在兼容性问题。
  - 对于用户手工编写的配置（如 `emqx.conf`）需要进行彻查，检查是否使用了转义序列（如 `\n`、`\r`、`\t` 和 `\\`），如果是，这样的字符串应该改为常规引号（一对 `"`）而不是三引号。
