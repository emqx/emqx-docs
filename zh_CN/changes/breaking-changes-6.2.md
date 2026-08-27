# EMQX 6.2 中的不兼容变更

## 6.2.1

- [#17157](https://github.com/emqx/emqx/pull/17157) 引入新的规则引擎配置项 `rule_engine.limit_selects_in_namespace`，默认值为 `true`。启用后，规则仅对与规则本身同属同一命名空间的客户端发布的消息触发。

## 6.2.0

- [#16589](https://github.com/emqx/emqx/pull/16589) 规则引擎运行时中的 jq 语言已从 1.6.1 版本升级至 1.8.1 版本，引入了若干细微的不兼容变更。这些变更不太可能影响到您的部署，但出于完整性考虑，在此一并说明。

  - **将空字符串作为 jq 程序时，现在视为错误**：请改用 `"."`。（[jq#2790](https://github.com/jqlang/jq/pull/2790)）

  - **字符串函数现在使用码位索引**：`indices/1`、`index/1` 和 `rindex/1` 函数现在使用码位索引而非字节索引；如需获取字节索引，请使用 `utf8bytelength/0`。（[jq#3065](https://github.com/jqlang/jq/pull/3065)）

  - **`tonumber/0` 拒绝包含前导或尾随空白的数字**：请在调用 `tonumber/0` 前先使用 `trim/0` 去除首尾空白。（[jq#3055](https://github.com/jqlang/jq/pull/3055)、[jq#3195](https://github.com/jqlang/jq/pull/3195)）

  - **`last(empty)` 行为变更**：`last(empty)` 现在不产生任何输出值，与 `first(empty)` 保持一致。（[jq#3179](https://github.com/jqlang/jq/pull/3179)）

  - **`limit/2` 在计数为负数时报错**，不再静默接受负数参数。（[jq#3181](https://github.com/jqlang/jq/pull/3181)）

  - **支持 Tcl 风格的多行注释**：这可能会对现有代码的解析产生细微影响。（[jq#2989](https://github.com/jqlang/jq/pull/2989)）

  - **十进制数转换方式变更**：十进制数现在转换为 binary64（double）而非 decimal64，使 jq 的行为更符合 JSON 规范，并与其他语言保持一致。（[jq#2949](https://github.com/jqlang/jq/pull/2949)）

  - **`nth/2` 在索引越界时返回空值**，而不再抛出错误。（[jq#2674](https://github.com/jqlang/jq/pull/2674)）

  - **字符串乘以 0 或小于 1 的数**：现在返回空字符串，而非原始字符串。（[jq#2142](https://github.com/jqlang/jq/pull/2142)）