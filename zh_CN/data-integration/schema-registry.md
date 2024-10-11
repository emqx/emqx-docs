# Schema Registry

::: tip 注意

Schema Registry 是 EMQX 企业版功能。

:::

物联网设备终端种类繁杂，各厂商使用的编码格式各异，所以在接入物联网平台的时候就产生了统一数据格式的需求，以便平台之上的应用进行设备管理。

Schema Registry 管理编解码使用的 Schema、处理编码或解码请求并返回结果。Schema Registry 配合规则引擎，可适配各种场景的设备接入和规则设计。

EMQX Schema Registry 目前可支持以下格式的 Schema：

- [Avro](https://avro.apache.org)
- [Protobuf](https://developers.google.com/protocol-buffers/)
- [JSON Schema](https://json-schema.org/)

Avro 和 Protobuf 是依赖 Schema 的数据格式，编码后的数据为二进制，解码后为 [Map 格式](#规则引擎内部数据格式-map)。解码后的数据可直接被规则引擎和其他插件使用。Schema Registry 为 Avro 和 Protobuf 等内置编码格式维护 Schema 文本。

JSON Schema 可以用来验证输入的 JSON 对象是否遵循了 schema 定义，或者在将数据输出到下游之前，规则引擎输出的 JSON 对象是否有效。

下图展示了 Schema Registry 的一个应用案例。多个设备上报不同格式的数据，经过 Schema Registry 解码之后，变为统一的内部格式，然后转发给后台应用。

<img src="./assets/schema-registry.png" alt="schema-registry" style="zoom:67%;" />

## 架构设计

EMQX 可以将 Schema 用于消息的编码、解码，以及验证发布的消息是否符合 Schema 规范。Schema Registry 为 Avro 和 Protobuf 内置编码格式维护 Schema 文本。Schema API 提供了通过 Schema Name 的添加、查询和删除操作，因此编码和解码时需要指定 Schema Name。

![schema_registry1](./assets/schema_registry1.svg)

常见的使用案例是，使用规则引擎来调用 Schema Registry 提供的编码和解码接口，然后将编码或解码后的数据作为后续动作的输入。

编码调用示例：

```erlang
schema_encode(SchemaName, Map) -> Bytes
```

解码调用示例：

```erlang
schema_decode(SchemaName, Bytes) -> Map
```

当对 JSON 格式的 MQTT 消息进行编码时，在用 schema 编码之前，您也需要用 `json_decode` 先对其进行解码，使它变为规则引擎内部数据格式 (Map)，示例如下：

```erlang
schema_encode(SchemaName, json_decode(Map)) -> Bytes
```

在编码前或解码后检查 JSON 数据是否可以根据 JSON schema 进行验证时，使用以下编解码验证示例：

```erlang
schema_check(SchemaName, Map | Bytes) -> Boolean
```

## 编解码 + 规则引擎

EMQX 的消息处理层面可分为消息路由 (Messaging)、规则引擎 (Rule Engine)、数据格式转换 (Data Conversion) 三个部分。

EMQX 的 PUB/SUB 系统将消息路由到指定的主题。规则引擎可以灵活地配置数据的业务规则，按规则匹配消息，然后指定相应动作。数据格式转换发生在规则匹配的过程之前，先将数据转换为可参与规则匹配的 Map 格式，然后进行匹配。

<img src="./assets/SchemaAndRuleEngine.png" alt="SchemaAndRuleEngine" style="zoom:67%;" />

### 规则引擎内部数据格式(Map)

规则引擎内部使用的数据格式为 Erlang Map，所以如果原数据内容为二进制或者其他格式，必须使用编解码函数(比如上面提到的 schema_decode 和 json_decode 函数) 将其转换为 Map。

Map 是一个 Key-Value 形式的数据结构，形如 #{key => value}。例如，`user = #{id => 1, name => "Steve"}` 定义了一个 `id` 为 `1`，`name` 为 `"Steve"` 的 `user` Map。

SQL 语句提供了 "." 操作符嵌套地提取和添加 Map 字段。下面是使用 SQL 语句对这个 Map 操作的示例:

```sql
SELECT user.id AS my_id
```

SQL 语句的筛选结果为 `#{my_id => 1}`。

### JSON 编解码

规则引擎的 SQL 语句提供了对 JSON 格式字符串的编解码支持，将 JSON 字符串和 Map 格式相互转换的 SQL 函数为 json_decode() 和 json_encode():

```sql
SELECT json_decode(payload) AS p FROM "t/#" WHERE p.x = p.y
```

上面这个 SQL 语句将会匹配到 payload 内容为 JSON 字符串： `{"x" = 1, "y" = 1}` , 并且 topic 为 `t/a` 的 MQTT 消息。

`json_decode(payload) as p` 将 JSON 字符串解码为下面的 Map 数据结构，从而可以在 `WHERE` 子句中使用 p.x 和 p.y 使用 Map 中的字段：

```erlang
#{
  p => #{
    x => 1,
    y => 1
  }
}
```

**注意:** `AS` 子句是必须的，将解码之后的数据赋值给某个Key，后面才能对其进行后续操作。

## 外部 Schema Registry

从 EMQX 版本 5.8.1 开始，支持在 EMQX 中配置外部 Confluent Schema Registry (CSR)。该功能允许用户在规则处理时动态获取外部 Schema Registry 中的 Schema，从而实现高效的消息编码和解码。

### 在 Dashboard 中创建外部 Schema Registry

您可以直接通过 EMQX Dashboard 配置外部 Schema Registry，方便地管理 Schema 集成。

进入 EMQX Dashboard 的 **集成** -> **Schema** 页面。在 Schema 页面中选择 **外部 Schema** 选项卡。

点击右上角的**创建**按钮，并配置以下字段：

- **名称**：输入外部 Schema Registry 的名称，该名称将在编码和解码函数中使用。
- **类型**：选择外部 Schema Registry 的类型。目前仅支持 `Confluent`。
- **URL**：输入您的 Confluent Schema Registry 的端点地址。
- **认证**：如果选择 `基础认证`，请输入访问外部 Schema Registry 所需的认证信息（用户名和密码）。

完成设置后，点击**创建**按钮。

### 通过配置文件配置外部 Schema Registry

您也可以通过 EMQX 配置文件配置外部 Confluent Schema Registry。以下是配置示例：

```hcl
schema_registry {
  external {
    my_external_registry {
      type = confluent
      url = "https://confluent.registry.url:8081"
      auth {
        username = "myuser"
        password = "secret"
      }
    }
  }
}
```

在此示例中：

- `my_external_registry` 是分配给外部 Schema Registry 的名称。
- `type = confluent` 指定外部 Schema Registry 的类型。
- `url` 是 Confluent Schema Registry 的端点地址。
- `auth` 包含访问外部 Schema Registry 所需的认证信息（用户名和密码）。

### 在规则引擎中使用外部 Schema Registry

配置外部 Schema Registry 后，您可以在 EMQX 规则引擎中使用多个函数，利用外部 Schema Registry 中存储的 Schema 对 payload 进行编码和解码。

配置的外部 CSR 可以在以下函数中使用：

```sql
avro_encode('my_external_registry', payload, my_schema_id)
avro_decode('my_external_registry', payload, my_schema_id)
schema_encode_and_tag('my_local_avro_schema', 'my_external_registry', payload, 'my_subject')
schema_decode_tagged('my_external_registry', payload)
```

在下面所有函数使用示例中，使用了以下示例值和变量名：

- `my_external_registry` 是您在 EMQX 中为外部 Schema Registry 指定的名称。
- `my_schema_id` 是注册在 CSR 中的 Schema ID（在 CSR 中始终是整数）。
- `my_local_avro_schema` 是在 EMQX 中配置的本地 Avro Schema 名称。
- `my_subject` 是在 CSR 中定义的主题名称。

#### 函数使用示例

##### `avro_encode`

`avro_encode` 使用外部 Schema Registry 中的 Schema ID 对 payload 进行编码。Schema 会在运行时动态获取，并缓存以供后续使用。在 Confluent Schema Registry 中，Schema ID 是整数。

::: tip 提示

编码时，payload 必须是规则引擎的内部数据格式，即已解码的 Map。因此在示例中使用了 `json_decode`。

:::

示例：

```sql
select
  -- 123 是在 CSR 中注册的 Schema ID
  avro_encode('my_external_registry', json_decode(payload), 123) as encoded
from 't'
```

##### `avro_decode`

该函数根据外部 Schema Registry 中的 Schema ID 对 Avro payload 进行解码。Schema 会在运行时动态获取，并缓存以供后续操作。

示例：

```sql
select
  -- 123 是在 CSR 中注册的 Schema ID
  avro_decode('my_external_registry', payload, 123) as decoded
from 't'
```

##### `schema_encode_and_tag`

此函数使用本地注册的 Avro Schema、外部 CSR 的 Schema 名称和主题对 payload 进行编码，并将编码后的 payload（已为内部 Map 格式）标记为带有 Schema ID。Schema ID 是通过将本地 Schema 注册到 CSR 获得的。

示例：

```sql
select
  schema_encode_and_tag(
    'my_local_avro_schema',
    'my_external_registry',
    json_decode(payload),
    'my_subject'
  ) as encoded
from 't'
```

##### `schema_decode_tagged`

此函数使用 CSR 名称对 payload 进行解码，假设该 payload 带有从 CSR 获取的 Schema ID。

```sql
select
  schema_decode_tagged(
    'my_external_registry',
    payload
  ) as decoded
from 't'
```
