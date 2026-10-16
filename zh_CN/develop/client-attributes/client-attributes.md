# 客户端属性

客户端属性是 EMQX 提供的一种机制，允许开发人员根据不同的应用场景需求，为 MQTT 客户端设置额外的属性。这些属性可以用于 EMQX 的认证授权、数据集成和 MQTT 扩展功能等功能中，在实现灵活开发中发挥重要作用。通过利用客户端元数据，此功能还支持 MQTT 客户端识别的灵活模板化，这对于个性化客户端配置和简化认证过程至关重要，增强了开发工作的适应性和效率。

## 工作流程

客户端属性的设置、存储和使用流程如下：

**1. 设置客户端属性：**

当客户端成功连接到 EMQX 时，EMQX 会触发连接与认证事件，并在此过程中根据预先定义的配置来[设置客户端属性](#设置客户端属性)。

**2. 客户端属性存储与销毁：**

一旦设定了属性，它们会以键值对的形式存储在客户端会话的 `client_attrs` 字段中。当客户端会话结束时，这些属性会被删除。

对于保留会话，客户端接管时，会替换覆盖会话中已有的客户端属性。除此之外，无法通过其他方式修改或删除客户端属性。

**3. 使用客户端属性：**

EMQX 的其他功能允许在相关配置项中使用 `${client_attrs.NAME}` 占位符，动态提取属性值，并将其配置或数据的一部分来使用。

## 设置客户端属性

当客户端成功连接到 EMQX 时，EMQX 会触发连接和认证事件，并根据预先定义的配置来设置客户端属性。支持以下设置方式：

- 从客户端元数据中提取
- 在客户端认证过程中设置

### 从客户端元数据中提取

通过预设的配置，从客户端连接时的用户名、客户端 ID 等元数据中提取并处理生成子串，并将其设置为客户端属性。这个过程是在认证之前发生的，确保了属性可以在认证过程中使用。您可以通过配置文件或 Dashboard 配置客户端属性功能。

想要在 Dashboard 中配置客户端属性，点击左侧导航中的**管理** -> **MQTT 配置**，在**客户端属性**配置中点击**添加**，填写属性名称和属性表达式。

![客户端属性设置](./assets/client_attributes_config_ee.png)

其中，**属性**为属性的名称，**属性表达式**为属性的提取配置。

属性表达式支持使用 [Variform 表达式](../../guides/configuration/configuration.md#variform-表达式)以及其中的[预定义的函数](../../guides/configuration/configuration.md#预定义函数)对值进行动态处理。例如:

- 要提取由点分隔的客户端 ID 的前缀：`nth(1, tokens(clientid, '.'))`
- 要截取用户名部分信息：`substr(username, 0, 5)`

对应的配置文件如下：

```bash
mqtt {
    client_attrs_init = [
        {
            expression = "nth(1, tokens(clientid, '.'))"
            set_as_attr = clientid_prefix
        },
        {
            expression = "substr(username, 0, 5)"
            set_as_attr = sub_username
        }
    ]
}
```

在属性表达式中支持配置以下值：

- `clientid`：客户端 ID
- `username`：用户名
- `cn`：TLS 证书的 CN 字段
- `dn`：TLS 证书的 DN 字段
- `cert_san.dns`：TLS 客户端证书中的 DNS 名称
- `cert_san.ip`：TLS 客户端证书中的 IPv4 和 IPv6 地址
- `cert_san.email`：TLS 客户端证书中的电子邮件地址
- `cert_san.uri`：TLS 客户端证书中的 URI
- `user_property.*`：从 MQTT CONNECT 数据包的 User-Property 中提取属性值，例如 `user_property.foo`
- `zone`：继承自 MQTT 监听器的 zone 名称

更多客户端属性配置的详细信息，请参见 [EMQX 企业版配置手册](https://docs.emqx.com/zh/enterprise/v@EE_VERSION@/hocon/)。

#### 使用证书主题备用名称初始化客户端属性

从 EMQX Enterprise 6.3.0 开始，当 TLS 连接在 EMQX 终结时，`mqtt.client_attrs_init` 表达式可以从客户端证书中提取主题备用名称（Subject Alternative Name，SAN）。每个 `cert_san.*` 变量都是数组。可以使用 Variform 数组函数选择某个值，也可以将多个值合并为一个客户端属性。

例如，以下配置将第一个 DNS 名称设置为 `client_attrs.san_dns`，并将所有 DNS 名称以逗号分隔后设置为 `client_attrs.san_dns_all`：

```hocon
mqtt {
    client_attrs_init = [
        {
            expression = "nth(1, cert_san.dns)"
            set_as_attr = san_dns
        },
        {
            expression = "join_to_string(',', cert_san.dns)"
            set_as_attr = san_dns_all
        }
    ]
}
```

`cert_san.*` 变量只能用于初始化客户端属性。要在认证、授权或其他支持客户端属性的功能中使用 SAN 值，需要先将其保存为客户端属性，再通过 `${client_attrs.NAME}` 引用。

如果证书不包含指定类型的 SAN，对应变量为空数组。当 `nth()` 选择的条目不存在时，EMQX 不会设置目标客户端属性。如果提取的 SAN 值包含回车符（`\r`）、换行符（`\n`）等不可打印控制字符，EMQX 将拒绝客户端连接。

::: warning 重要提示
只有 TLS 连接在 EMQX 终结且客户端向 EMQX TLS 监听器提供证书时，EMQX 才能提取 SAN。Proxy Protocol v2 不携带 SAN 信息。如果负载均衡器终结 TLS，EMQX 无法获取 `cert_san.*` 值。如果负载均衡器不终结 TLS，而是将 TLS 连接转发给 EMQX，则 EMQX 仍可从客户端证书中提取 SAN。
:::

### 在客户端认证过程中设置

在客户端认证过程中，可以通过认证器返回的信息设置客户端属性，目前支持：

- [JWT 认证](../../guides/access-control/authn/jwt.md)：签发 Token 时，在 Payload 中通过 `client_attrs` 字段设置客户端属性。
- [HTTP 认证](../../guides/access-control/authn/http.md)：在认证成功的 HTTP 响应中，通过 `client_attrs` 字段设置客户端属性。

从 EMQX 7.0 开始，以下数据库认证器和 LDAP 认证器也可以设置客户端属性：

- [MySQL 认证](../../guides/access-control/authn/mysql.md)和 [PostgreSQL 认证](../../guides/access-control/authn/postgresql.md)：将查询结果列的别名设置为 `client_attrs.<name>`。
- [Redis 认证](../../guides/access-control/authn/redis.md)：将属性存储在名为 `client_attrs.<name>` 的 Redis Hash 字段中，并在查询命令中包含这些字段。
- [MongoDB 认证](../../guides/access-control/authn/mongodb.md)：将属性存储在认证文档中 `client_attrs` 字段对应的嵌套对象中。
- [LDAP 认证](../../guides/access-control/authn/ldap.md)：将通过认证的用户条目中的 LDAP 属性映射为客户端属性。

属性名称必须以字母或数字开头，并且只能包含字母、数字、连字符和下划线。属性值支持字符串。从 EMQX 7.0 开始，还支持数字和布尔值，EMQX 会将其转换为字符串；对于名称无效或值为数组、嵌套对象、数据库 `NULL` 等不支持类型的属性，EMQX 会将其丢弃并记录警告日志。

认证结果可以通过 `client_attrs` Map 提供属性。从 EMQX 7.0 开始，支持的认证后端还可以通过独立的 `client_attrs.<name>` 字段提供属性。如果两种形式包含同名属性，`client_attrs.<name>` 字段的值优先。

### 合并认证数据

当通过多个来源或多个认证器设置客户端属性时，EMQX 会按照属性名称以及设置的先后顺序对属性进行合并：

- 从客户端元数据中提取的属性将被认证器中设置的属性覆盖。
- 认证链中多个认证器设置属性时，后设置的属性将覆盖前面的属性。

## 客户端属性的应用

在其他 EMQX 功能中，客户端属性可以通过 `${client_attrs.NAME}` 占位符提取，并作为配置或数据的一部分来使用。目前仅支持用于客户端认证与授权，更多功能将来后续开放。

### 客户端认证

通过[认证占位符](../../guides/access-control/authn/authn.md#认证占位符)用于 SQL 语句、查询命令或 HTTP 请求体中的动态参数，例如：

```sql
# MySQL/PostgreSQL - 认证查询 SQL
SELECT password_hash, salt, is_superuser FROM mqtt_user WHERE sn = ${client_attrs.sn} LIMIT 1

# HTTP - 认证请求 Body
{
 "sn": "${client_attrs.sn}",
 "password": "${password}"
}
```

具体使用方式请参考每个认证器对应的文档。

::: tip
客户端认证仅能使用从客户端元数据中提取设置的属性。
:::

### 客户端授权

通过[数据查询占位符](../../guides/access-control/authz/authz.md#数据查询占位符)与[主题占位符](../../guides/access-control/authz/authz.md#主题占位符)，用于 SQL 语句、查询命令，以及主题当中。

#### 示例场景：

通过客户端属性，为每个客户端设置了各自的 `role`, `productId`, `deviceId` 属性。需要将这些属性用于授权检查中：

- **role**: 用来限制客户端的访问权限，只允许 role 为 `admin` 的客户端订阅和发布管理消息，如匹配 `admin/#` 的主题。
- **productId**: 用来限制客户端只能订阅适用于当前产品的 OTA 消息，如 `OTA/{prodoctId}`。
- **deviceId**: 用来限制客户端只能在属于自己的主题上发布和订阅消息：
  - 发布：`up/{productId}/{deviceId}`
  - 订阅：`down/{productId}/{deviceId}`


可以使用[授权-内置数据库](../../guides/access-control/authz/mnesia.md)配置如下规则实现：

| 权限 | 操作 | 主题 |
| --- |  --- |  --- |
| 允许 | 订阅和发布 | `${client_attrs.role}/#` |
| 允许 | 订阅 | `OTA/${client_attrs.productId}` |
| 允许 | 发布 | `up/${client_attrs.productId}/${client_attrs.deviceId}` |
| 允许 | 订阅 | `down/${client_attrs.productId}/${client_attrs.deviceId}` |

相较于直接使用客户端 ID 等静态属性，更灵活地管理客户端的授权。这种灵活性使得我们能够基于不同的角色、产品和设备来细粒度地控制客户端的访问权限。
