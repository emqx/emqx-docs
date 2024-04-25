# MQTT 客户端属性

在 EMQX 中，用户除了可以使用预定义的名称，如 `clientid` 和 `username` 作为 MQTT 客户端标识符外，还可以通过客户端属性提取功能在客户端连接时设置自定义属性，并将其用于认证和授权等功能。这一功能旨在通过从客户端元数据的各个来源提取客户端属性值，支持灵活模板化的 MQTT 客户端识别。这一功能在多租户环境、个性化客户端配置和简化的认证过程等场景中尤为有用。

## 功能介绍

当客户端连接到 EMQX 时，其客户端属性会被初始化。在初始化期间，EMQX 根据在 `mqtt.client_attrs_init` 配置中设置的预定义规则提取属性。例如，它可以从客户端 ID、用户名或客户证书的常用名称中提取子串。这种提取在认证过程之前发生，确保属性在后续步骤中能够被使用，如在 HTTP 请求体模板或 SQL 模板中用于组成认证和授权请求时。

客户端属性就存储在一个称为 `client_attrs` 的字段中，该字段位于客户端的会话或连接上下文中。`client_attrs` 信息字段保存以键值对形式的属性。这个字段保持在与客户端会话相关的内存中，以便在客户端连接生命周期中快速访问。

`client_attrs` 在初始化之后还可以使用认证结果携带的信息来扩展。例如 JWT 中的 `client_attrs` 字段，和HTTP 认证响应中的 `client_attrs` 字段。

### 提取客户端属性

本节描述了 EMQX 从哪些客户端属性中提取以及如何提取属性。

#### 客户端元数据和属性的来源

在 EMQX 中，客户端元数据和属性有各种来源，并存储在特定的系统属性中，以便在客户端连接生命周期中使用。以下是这些现有客户端信息的来源和存储位置：

- **MQTT 连接报文**：当客户端连接到 EMQX 时，它发送一个包含多个信息片段的连接报文，如 `clientid`、`username`、`password` 和 `user properties`。
- **TLS 证书**：如果客户端使用 TLS 连接，客户的 TLS 证书可以提供额外的元数据，例如：
  - `cn`（常用名称）：证书的一部分，可以识别设备或用户。
  - `dn`（区分名称）：证书中包含关于证书持有者的几个描述性字段的完整主题字段。
  - 服务器名称指示（SNI）：当前用作多租户租户 ID。
- **IP 连接数据**：包括客户端的 IP 地址和端口号，这些都是客户端连接时 EMQX 自动捕获的。

#### Variform 表达式

提取过程使用了一种称为 Variform 的函数调用风格模板渲染表达式。
这种方法设计用来通过允许函数调用和变量引用来指定属性提取，动态处理数据。
需要注意的是，Variform 表达式不是完全可编程的，并且仅限于预定义的函数和变量。

作为示例，考虑以下表达式：

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

由于其设计简洁，非常适合嵌入配置中。以下是一个示例。

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

##### 预绑定变量

预绑定变量可以直接在提取表达式中使用。包括以下预绑定变量：

- `cn`：客户证书常用名称。
- `dn`：客户证书区分名称（主题）。
- `clientid`
- `username`
- `user_property`：客户在 MQTT v5 连接包中提供的用户属性。
- `ip_address`：客户端的源 IP 地址。
- `port`：客户端的源端口号。
- `zone`：区域名称

##### 预定义函数

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

##### 示例表达式

`nth(1, tokens(clientid, '.'))`：提取以点分隔的客户端ID的前缀。

`strlen(username, 0, 5)`: 提取部分用户名。

`coalesce(regex_extract(clientid,'(\\d+)'),'000')`: 使用正则表达式提取客户端 ID 中的数字，如果正则表达式提取不到，则返回 `'000'`。

### 合并认证数据

EMQX 会将认证结果中的 `client_attrs` 字段合并到初始化的 `client_attrs` 中。当前支持合并的认证模式如下：

- **JWT 声明**：如果使用 JWT 进行认证，它们可以包含 `client_attrs` 声明，携带有关客户的额外元数据，如角色、权限或其他标识符。

- **HTTP 认证响应**：如果 EMQX 配置为使用外部 HTTP 服务进行认证，此服务的响应可能包含有关客户的额外属性或元数据，可以配置为在 EMQX 中捕获并存储。例如，如果 HTTP 响应包括 `"client_attrs": {"group": "g1"}`，EMQX 将把这些数据合并到客户现有的属性中，然后可以在授权请求中使用这些属性。

## 客户端属性的应用

提取和合并的属性可以用于构建认证和授权请求。 `client_attrs.NAME` 可以用于认证和授权模板渲染。如果定义了名为 `client_attrs.alias` 的属性，可以将其合并到 HTTP 请求体或 SQL 查询中，增强这些请求的灵活性和特异性。

例如，对于名为 `client_attrs.alias` 的属性，您可以使用 `${client_attrs.alias}` 来构建作为HTTP认证请求的HTTP请求体。有关更多详情，请参见[认证占位符](../access-control/authn/authn.md#authentication-placeholders)。

## 配置属性提取

您可以通过配置文件或仪表板配置属性提取功能。

### 通过配置文件配置属性提取

配置示例：

```
<!-- 代码示例 -->
```

配置项解释：

{% emqxce %}

有关配置的详细信息，请参见[配置手册](https://www.emqx.io/docs/en/v@CE_VERSION@/hocon/)。

{% endemqxce %}

{% emqxee %}

有关配置的详细信息，请参见[配置手册](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)。

{% endemqxee %}

### 通过 Dashboard 配置属性提取

<!-- 在前端开发完成后添加描述 -->
