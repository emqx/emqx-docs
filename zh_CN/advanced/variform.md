# Variform 表达式

Variform 是一种轻量级、富有表现力的语言,旨在进行字符串操作和运行时求值。
它不是一种全功能的编程语言，而是一种专门的工具，可以嵌入配置中，用来态执行字符串操作。

## 语法概览

以下面的表达式作为示例：

```
function_call(clientid, another_function_call(username))
```
此表达式结合或操作 `clientid` 和 `username` 以生成新的字符串值。

Variform 支持以下字面量：

- 整数：例如，`42`。
- 浮点数：例如，`3.14`。
- 字符串：单引号 `'` 或双引号 `"` 之间的 ASCII 字符。
- 数组：元素位于 `[` 和 `]` 之间，以逗号 `,` 分隔。
- 变量：引用预定义的值，例如 `clientid`。
- 函数：预定义的函数，例如 `concat([...])`。

Variform 不支持以下功能：

- 算术运算
- 条件语句
- 循环
- 用户定义的变量
- 用户定义的函数
- 异常处理和错误恢复
- Boolean literals. Booleans may be produced intermediately as return values from a built-in functions such as `num_gt` (which stands for 'is number greater'),
  but cannot be wirtten as a literal. The condition functions (`iif` and `coalesce`) take empty string for `false` otherwise `true`.
- Escape sequence in string literals. Call the `unescape` function to unescape special characters.


以下是一个嵌入配置文件中的示例。

```bash
mqtt {
    client_attrs_init = [
        {
            # 提取客户端 ID 在第一个 `-` 字符前面的前缀
            expression = "nth(1,tokens(clientid, '-'))"
            # 然后把提取的字符串赋值给 client_attrs.group 这个字段
            set_as_attr = group
        }
    ]
}

```

::: tip
When unescape function is required in the expression, it's a good idea to use triple quote (`"""`) strings in HOCON config
so there is no need to perform double escaping.

For example

```
## For multi-line client ID, take the first line.
expression = """nth(1, tokens(clientid, unescape('\n')))"""
```
:::


## 预定义函数

EMQX 包含一系列丰富的字符串、数组、随机和散列函数，类似于规则引擎字符串函数中可用的那些。这些函数可以用来操作和格式化提取的数据。例如，`lower()`、`upper()` 和 `concat()` 可以帮助调整提取字符串的格式，而 `hash()` 和 `hash_to_range()` 可以基于数据创建散列或范围输出。

以下是可以在表达式中使用的函数：

- **字符串函数**：
  - [字符串操作函数](../data-integration/rule-sql-builtin-functions.md#string-operation-functions)
  - 还添加了一个新函数 any_to_string/1，用于将任何中间非字符串值转换为字符串。
- **数组函数**：[nth/2](../data-integration/rule-sql-builtin-functions.md#nth-n-integer-array-array-any)
- **随机函数**：rand_str, rand_int
- **无模式编码/解码函数**：
  - [bin2hexstr/1](../data-integration/rule-sql-builtin-functions.md#bin2hexstr-data-binary-string)
  - [hexstr2bin/1](../data-integration/rule-sql-builtin-functions.md#hexstr2bin-data-string-binary)
  - [base64_decode/1](../data-integration/rule-sql-builtin-functions.md#base64-decode-data-string-bytes-string)
  - [base64_encode/1](../data-integration/rule-sql-builtin-functions.md#base64-encode-data-string-bytes-string)
  - int2hexstr/1
- **散列函数**：
  - hash(算法, 数据)，其中算法可以是以下之一：md4 | md5, sha (或 sha1) | sha224 | sha256 | sha384 | sha512 | sha3_224 | sha3_256 | sha3_384 | sha3_512 | shake128 | shake256 | blake2b | blake2s
  - hash_to_range(输入, 最小值, 最大值)：使用 sha256 散列输入数据，并将散列映射到最小值和最大值之间的整数（包括最小值和最大值）。
  - map_to_rage(输入, 最小值, 最大值)：将输入映射到最小值和最大值之间的整数（包括最小值和最大值）。

## Conditions

Variform expression so far has no comprehensive control flows. The `iif` function is a conditional expression used
to evaluate a condition and return one of two values depending on the result of the condition. This function is
inspired by similar constructs in other programming languages, adapted here for use in a programming expression
that lacks loops and variable bindings.

```js
iif(Condition, ThenExpression, ElseExpression)
```

### Parameters

- **Condition** (Boolean or String): Specifies the condition to be evaluated.
  - If a Boolean (`true` or `false`), it directly evaluates to `true` or `false`.
    - If a String, it evaluates to `false` if the string is empty, and `true` otherwise.
    - **ThenExpression**: The expression or value that is returned if `Condition` evaluates to `true`.
    - **ElseExpression**: The expression or value that is returned if `Condition` evaluates to `false`.

### Returns

- The result of `ThenExpression` if `Condition` is `true`.
- The result of `ElseExpression` if `Condition` is `false`.

## Error Handling

As the default behavior of scripting environments like Bash, Variform expression is designed to yield an empty string ("") in scenarios where errors occur, such as unbound variables or exceptions during runtime.

- Unbound Variables: If an expression references a variable that has not been defined or is out of scope (unbound), the expression will evaluate to an empty string.
- Runtime Exceptions: Any exceptions that occur during the execution of an expression, whether due to incorrect function usage, invalid data types, or other unforeseen issues, will result in the expression yielding an empty string. For example, array index out of range.

## 示例表达式

- `nth(1, tokens(clientid, '.'))`:  Extract the prefix of a dot-separated client ID.
- `strlen(username, 0, 5)`: Extract a partial username.
- `coalesce(regex_extract(clientid,'[0-9]+'),'vin-1000')`: Extract digits from client ID using a regular expression. If the regular expression yields empty string, then return `'000'`.
- `iif(true, "Value if true", "Value if false")`: Returns `Value if true`
- `iif("", "Value if true", "Value if false")`: Returns `Value if false`
- `iif("hello", "Value if true", "Value if false")`: Returns `Value if true`
- `iif(regex_match(clientid,'^foo\.+*'),'foo','bar')`: Returns `foo` if `clientid` starts with `foo.`, otherwise `bar`.
