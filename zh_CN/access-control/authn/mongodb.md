# 使用 MongoDB 进行密码认证

作为密码认证方式的一种，EMQX 支持通过集成 MongoDB 进行密码认证，目前 EMQX 支持**单节点**、**Replica Set**、**Sharding** <!--需要超链接--> 三种模式部署的 MongoDB 服务器。

::: tip
前置准备：

- 熟悉 [EMQX 认证基本概念](../authn/authn.md)。
  :::

## 通过 Dashboard 配置

在 [EMQX Dashboard](http://127.0.0.1:18083/#/authentication) 页面，点击左侧导航栏的**访问控制** -> **认证**，在随即打开的**认证**页面，单击**创建**，依次选择**认证方式**为 `Password-Based`，**数据源**为 `MongoDB`，进入**配置参数**页签：

![use mongodb to authenticate](./assets/authn-mongodb.png)



您可根据如下说明完成相关配置：

**连接**：在此部分完成到 MongoDB 数据库的连接设置。

- **部署模式**：选择 MongoDB 数据库的部署模式，可选值：**单节点**、**Replica Set**、**Sharding**
- **服务**（**列表**）：填入 MongoDB 服务器地址 (`host:port`) ；当**部署模式**选为 **Replica Set** 或 **Sharding** 时，您需在此提供所有相关 MongoDB  服务器的地址，不同地址之间以 `,` 分隔，格式为 `host1:port1,host2:port2,...`
- **Replica Set Name**：字符串，用于指定 Replica Set 的名称，仅需在**部署模式**设置为 **Replica Set** 时设置。
- **数据库**：数据库名称。 <!--需要确认-->
- **Collection**： <!--需要补充-->
- 用户名（可选）：填入用户名。
- **密码**（可选）：填入认证密码。
- **读模式**（可选）：读取模式，可选值：**master、slave_ok**；默认值：**master**；**master** 意味着序列中的每个查询都必须从主服务器读取新数据。 如果连接的服务器不是主服务器，则第一次读取将失败，其余操作将中止。**slave_ok** 表示允许每个查询从从服务器读取陈旧数据（来自主服务器的新数据也可以）；仅需在**部署模式**设置为 **Replica Set** 时设置。
- **写模式**（可选）：写入模式， <!--需要补充-->可选值：`unsafe`、 `safe`；默认值：`unsafe`。仅需在**部署模式**设置为 **Replica Set** 时设置。

**TLS 配置**：配置是否启用 TLS。

**连接配置**：在此部分设置并发连接以及连接超时等待时间。

- **Pool size**（可选）：填入一个整数用于指定从 EMQX 节点到 MongoDB 数据库的并发连接数；默认值：**8**。
- **连接超时**（可选）：填入连接超时等待时长，可选单位：**小时**、**分钟**、**秒**、**毫秒**。

**认证配置**：在此部分进行认证加密算法相关的配置。

- **Password Hash 字段名**： <!--需要补充-->

- **密码加密方式**：选择存储密码时使用的散列算法，如 plain、md5、sha、bcrypt、pbkdf2 等。

  1. 选择 **plain**、**md5**、**sha**、**sha256** 或 **sha512** 算法，需配置：
     - **加盐方式**：用于指定盐和密码的组合方式，除需将访问凭据从外部存储迁移到 EMQX 内置数据库中外，一般不需要更改此选项；可选值：**suffix**（在密码尾部加盐）、**prefix**（在密码头部加盐）、**disable**（不启用）。注意：如选择 **plain**，加盐方式应设为 **disable**。

  1. 选择 **bcrypt** 算法，无需额外配置。

  1. 选择 **pkbdf2** 算法，需配置：

     - **伪随机函数**：指定生成密钥使用的散列函数，如 sha256 等。
     - **迭代次数**：指定散列次数，默认值：**4096**。<!--后续补充取值范围-->
     - **密钥长度**（可选）：指定希望得到的密钥长度。如不指定，密钥长度将由**伪随机函数**确定。
     - **is_superuser 字段名**：指定是否超级用户。
     - **查询 Filter**：<!--需要补充-->

点击**创建**完成相关配置。

## 通过配置文件配置

您也可以通过配置文件完成相关配置，关于 单节点、ReplicaSet 和 Sharding 的详细配置方式，可参考：

- [authn-mongodb:standalone](../../admin/cfg.md#authn-mongodb:standalone)
- [authn-mongodb:sharded-cluster](../../admin/cfg.md#authn-mongodb:sharded-cluster) 
- [authn-mongodb:replica-set](../../admin/cfg.md#authn-mongodb:replica-set)

以下为各部署模式下的配置文件示例：

:::: tabs type:card

::: tab 单节点部署模式

```hocon
{
  mechanism = password_based
  backend = mongodb
  enable = true

  password_hash_algorithm {
    name = sha256
    salt_position = suffix
  }

  collection = "mqtt_user"
  filter { username = "${username}" }

  mongo_type = single
  server = "127.0.0.1:27017"

  database = "mqtt"
  username = "emqx"
  password = "secret"
}
```

:::

::: tab Replica Set 部署模式

```hocon
{
  mechanism = password_based
  backend = mongodb
  enable = true

  password_hash_algorithm {
    name = sha256
    salt_position = suffix
  }

  collection = "mqtt_user"
  filter { username = "${username}" }

  mongo_type = rs
  servers = "10.123.12.10:27017,10.123.12.11:27017,10.123.12.12:27017"
  replica_set_name = "rs0"

  database = "mqtt"
  username = "emqx"
  password = "secret"
}
```

:::

::: tab Sharding 部署模式

```hocon
{
  mechanism = password_based
  backend = mongodb
  enable = true

  password_hash_algorithm {
    name = sha256
    salt_position = suffix
  }

  collection = "mqtt_user"
  filter { username = "${username}" }

  mongo_type = sharded
  servers = "10.123.12.10:27017,10.123.12.11:27017,10.123.12.12:27017"

  database = "mqtt"
  username = "emqx"
  password = "secret"
}
```

:::

::::
