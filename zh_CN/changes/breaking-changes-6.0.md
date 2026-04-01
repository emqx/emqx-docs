# EMQX 6.0 中的不兼容变更

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

## 6.1.0

- [#16368](https://github.com/emqx/emqx/pull/16368) 内部正则表达式引擎已升级为 PCRE2，带来了更高的匹配性能以及更严格的语法校验。

  如果你在规则引擎 SQL 中使用了 `regex_match`、`regex_replace` 或 `regex_extract` 函数，一些依赖于宽松或未定义行为的现有正则表达式，可能无法再成功编译或按预期进行匹配。

  **需要注意的关键变化包括：**

  - **更严格的转义规则**：此前被忽略的无效或不必要的转义序列，现在会被视为错误。
    - **不再支持**：`[\w-\.]`，在字符类中对 `.` 进行转义是不必要的，现已不被接受；只有元字符才需要转义。
    - **不再支持**：缺少有效十六进制数字的 `\x`（例如 `\xGG`），现在会导致编译错误，而不再被解释为字面量 `x`。
  - **更严格的分组名称校验**：正则表达式中不再允许使用重复或为空的命名捕获分组。

  **需要采取的行动：** 请检查并验证所有使用正则表达式的规则引擎 SQL 定义。对于复杂的正则模式，建议使用符合 PCRE2 规范的测试工具进行验证（大多数在线正则测试工具均支持 PCRE2），或在升级前于预发布 / 测试环境中进行充分测试。

## 6.0.1

- [#16061](https://github.com/emqx/emqx/pull/16061) 修复了一个问题：RocketMQ 动作忽略了配置的 payload 模板，错误地发送了整个规则的输出结果。

  如果您依赖了此前（错误）行为，可能需要更新 payload 模板，以确保消息格式符合预期。