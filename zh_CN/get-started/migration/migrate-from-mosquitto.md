# 从 Mosquitto 迁移到 EMQX

本页介绍了如何将现有的 Eclipse Mosquitto 部署迁移到 EMQX。本指南适用于计划从轻量级单节点 MQTT Broker 升级至可扩展的分布式 MQTT 平台的系统管理员。迁移过程利用了 EMQX 对标准 MQTT 协议的兼容性，并提供了一个清晰的路径，用于迁移配置、安全认证信息和集成逻辑。

## 迁移概览

迁移过程分为三个主要阶段：

1. **盘点 Mosquitto 资源**：收集配置文件（`mosquitto.conf`）、安全文件（密码文件、ACL、证书），并了解当前数据流。
2. **配置 EMQX**：将 Mosquitto 设置转换为 EMQX 的配置文件（`emqx.conf`，HOCON 格式），导入用户凭据，并使用规则引擎重建访问控制和数据集成逻辑。
3. **更新设备与集成**：将设备连接重定向至 EMQX 集群（通常无需修改端口配置），并验证系统运行是否正常。

| 参数 / 文件资源     | Mosquitto 示例                  | EMQX 示例                                   | 说明                                 |
| ------------------- | ------------------------------- | ------------------------------------------- | ------------------------------------ |
| **主配置文件**      | `/etc/mosquitto/mosquitto.conf` | `/etc/emqx/emqx.conf`                       | EMQX 使用分层的 HOCON 配置格式。     |
| **网络端口**        | `1883` (TCP), `8883` (SSL)      | `1883` (TCP), `8883` (SSL)                  | 标准端口一致，通常无需修改设备配置。 |
| **用户凭据**        | `/etc/mosquitto/passwd`         | 内置数据库（Mnesia）                        | 可通过 API 导入现有密码哈希值。      |
| **访问控制**        | `/etc/mosquitto/acl_file`       | `/etc/emqx/acl.conf`                        | 可直接映射允许/拒绝规则。            |
| **桥接（Bridges）** | `connection bridge_name`        | 数据连接器与规则（Data Connectors & Rules） | 使用动态数据路由替代静态桥接。       |
| **持久化**          | `mosquitto.db`                  | `data/`（Mnesia + RocksDB）                 | EMQX 自动处理会话持久化。            |

## 阶段一：盘点 Mosquitto 资源

### 收集配置与证书文件

首先确定关键配置文件的位置，通常在 `mosquitto.conf` 中定义：

- **主配置文件：** 通过 `include_dir` 引用，或默认位于 `/etc/mosquitto/mosquitto.conf`。
- **证书文件：** 查找 `certfile`、`keyfile` 和 `cafile` 的路径。
- **安全文件：** 查找 `password_file` 和 `acl_file`。

将证书文件（`server.crt`、`server.key`、`ca.crt`）复制到 EMQX 节点，一般放置于 `/etc/emqx/certs/`。

### 分析认证与授权机制

确定当前的认证方式：

- **密码文件：** 最常见方式。可迁移到 EMQX 内部数据库。
- **插件（mosquitto-auth-plug）：** 若使用 SQL 或 LDAP，需在 EMQX 中配置相应的认证后端。

## 阶段二：配置 EMQX，使其与 Mosquitto 保持一致

### 重建 MQTT 监听器

Mosquitto 按顺序定义监听器，而 EMQX 按类型（TCP、SSL、WebSocket）在 `emqx.conf` 中分组定义。

**Mosquitto (`mosquitto.conf`) 示例：**

```properties
# 默认监听器
port 1883
max_connections -1

# SSL 监听器
listener 8883
certfile /etc/mosquitto/certs/server.crt
keyfile /etc/mosquitto/certs/server.key
```

**EMQX (`emqx.conf`) 示例：**

```hocon
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_connections = infinity
}

listeners.ssl.default {
  bind = "0.0.0.0:8883"
  ssl_options {
    certfile = "/etc/emqx/certs/server.crt"
    keyfile  = "/etc/emqx/certs/server.key"
  }
}
```

### 映射 MQTT 协议参数

将核心协议配置项进行对应转换，以保持客户端行为一致。

| Mosquitto 指令                 | EMQX HOCON 参数                | 说明                               |
| ------------------------------ | ------------------------------ | ---------------------------------- |
| `max_queued_messages`          | `mqtt.max_mqueue_len`          | 每个客户端的离线消息最大缓冲数量。 |
| `persistent_client_expiration` | `mqtt.session_expiry_interval` | 断开连接后会话状态的保留时长。     |
| `message_size_limit`           | `mqtt.max_packet_size`         | MQTT 报文的最大允许大小。          |
| `log_dest file`                | `log.file.enable = true`       | 启用日志文件输出。                 |

**注意：** Mosquitto 仅支持全局会话过期时间，而 EMQX（MQTT 5.0）支持按客户端自定义会话过期时间。
 对于旧版 MQTT 3.1.1 客户端，可在 EMQX 中设置全局默认值以保持一致。

### 迁移认证

EMQX 支持多种认证后端。在大多数从 Mosquitto 迁移的场景中，主要目标是**保留现有用户认证信息**，避免用户重置密码。

#### 方案 1：重新创建用户（批量导入）

如果您能够获取原始的明文密码，可以通过 EMQX HTTP API 批量导入。

**批量导入 CSV 格式示例：**

创建一个名为 `users.csv` 的文件，并包含以下列：

```csv
user_id,password,is_superuser
device001,secret123,false
admin,adminPass,true
```

**导入命令：**

使用 `curl` 命令上传该文件。参数 `type=plain` 表示 EMQX 在导入过程中会自动对密码进行哈希处理。

```bash
curl -v -u admin:public -X POST \
  -H "Content-Type: multipart/form-data" \
  -F "filename=@users.csv" \
  "http://localhost:18083/api/v5/authentication/password_based:built_in_database/import_users?type=plain"
```

- 将 `admin:public` 替换为你的 EMQX Dashboard 用户名与密码。
- 确认认证器 (`password_based:built_in_database`) 已存在并与您的配置匹配。

#### 方案 2：导入 Mosquitto 密码文件（高级）

如果您拥有大量用户，并且仅持有 `mosquitto.passwd` 文件（其中包含已哈希的密码），可以通过 Erlang 脚本将这些用户数据直接导入到 EMQX 的内置数据库中。

**步骤 1：配置认证机制**

在导入数据之前，需要在 EMQX 中配置[基于密码的认证](../../guides/access-control/authn/pwoverview.md)，并使用[内置数据库后端](../../guides/access-control/authn/mnesia.md)。必须按照以下设置，以确保与 Mosquitto 默认的密码哈希算法保持一致：

- **算法 (Algorithm)：** `pbkdf2`
- **MAC 函数 (MAC Function)：** `sha512`
- **迭代次数 (Iterations)：** `101`
- **密钥长度 (DK Length)：** `32`

> **注意：** 上述参数（`101` 次迭代，`sha512`）与 Mosquitto 默认配置完全一致。尽管这些参数与 EMQX 的默认安全配置（通常更为严格）不同，但在迁移时必须保持相同设置，以便验证原有密码哈希的有效性。

**步骤 2：复制密码文件**

将 `mosquitto.passwd` 文件复制到 EMQX 节点（例如 `/tmp/mosquitto.passwd`），并确保 `emqx` 系统用户对该文件具有读取权限。

**步骤 3：执行导入脚本**

在 EMQX 节点上运行以下命令。该脚本会读取 `mosquitto.passwd` 文件内容，解析并解码 Base64 格式的盐值（Salt）和哈希值（Hash），然后将用户记录直接写入数据库。

```bash
emqx eval "
File = \"/tmp/mosquitto.passwd\",
{ok, Bin} = file:read_file(File),
Lines = binary:split(Bin, <<\"\n\">>, [global, trim]),
lists:foreach(fun(Line) ->
    case binary:split(Line, <<\":\">>) of
        [Username, <<\"\$7$\", Rest/binary>>] ->
            [_, SaltB64, HashB64] = binary:split(Rest, <<\"$\">>, [global]),
            Salt = base64:decode(SaltB64),
            Hash = binary:part(emqx_utils:bin_to_hexstr(base64:decode(HashB64), lower), 0, 64),
            Record = {user_info, {'mqtt:global', Username}, Hash, Salt, false},
            mnesia:dirty_write(emqx_authn_mnesia, Record);
        _ -> ok
    end
end, Lines)."
```

##### 可选方案：外部数据库

对于需要与现有用户管理系统集成的企业级部署，您也可以选择将用户数据迁移至外部 SQL 数据库（如 MySQL 或 PostgreSQL）。
EMQX 支持动态 SQL 查询，可灵活适配不同的数据库架构与字段格式，从而无缝集成至企业现有的身份验证体系。

#### 方案 3：双向 TLS 认证（mTLS）

如果您的 Mosquitto 部署使用 X.509 客户端证书（Mutual TLS）进行身份认证，那么在迁移至 EMQX 时，需要在 EMQX 的监听器中启用并配置客户端证书验证。

**Mosquitto 配置示例：**

```properties
require_certificate true
use_identity_as_username true
cafile /etc/mosquitto/ca.crt
```

**EMQX 配置示例：**

```hocon
listeners.ssl.default {
  bind = "0.0.0.0:8883"
  ssl_options {
    cacertfile = "/etc/emqx/certs/ca.crt"
    verify = verify_peer
    fail_if_no_peer_cert = true
  }
}
```

- 请确保将同一 CA 证书（`ca.crt`）复制到 EMQX 节点。
- 若启用 `use_identity_as_username`，EMQX 将默认使用证书的 CN（Common Name）作为用户名。

### 迁移授权（ACL）

完成认证配置后，迁移主题级访问控制规则。

Mosquitto 的 ACL 语法与 EMQX 的 `acl.conf` 非常相似。

**Mosquitto (`acl_file`) 示例：**

```properties
user Alice
topic read sensors/#
pattern write devices/%u/data
```

**EMQX (`acl.conf`) 示例：**

```erlang
{allow, {user, "Alice"}, subscribe, ["sensors/#"]}.
{allow, all, publish, ["devices/${username}/data"]}.
```

- 将 `%u` 替换为 `${username}`（或 `${clientid}`）。
- 将 `read` 映射为 `subscribe`， `write` 映射为 `publish`。

### 配置数据集成（替换 Bridges 与脚本）

Mosquitto 使用桥接功能实现消息转发，并使用外部脚本（Python/Node.js）进行数据处理。在 EMQX 中，这些功能可由内置的[规则引擎](../../develop/data-integration/rules.md)和[数据集成](../../develop/data-integration/data-bridges.md)替代。

> EMQX 规则引擎允许您在消息转发至外部系统之前，对消息进行筛选、过滤与转换，并通过连接器将结果输出到目标系统。

**场景 1：转发数据到另一 MQTT Broker**

代替 Mosquitto 中的 `connection bridge_name` 配置：

1. 在 EMQX Dashboard 中创建 **MQTT Broker 连接器**。

2. 创建一条**规则**：

   ```sql
   SELECT * FROM "#"
   ```

   并将结果转发至该连接器。

**场景 2：替换 Python 处理脚本**

假设您原本使用一个 Python 脚本，订阅 `sensors/+/temp` 主题，筛选出温度值大于 30 的数据，并将其写入数据库。

在 EMQX 中，可以通过规则引擎实现同样的逻辑，而无需外部脚本：

1. 删除脚本。

2. 创建规则：

   ```sql
   SELECT payload.temp as temperature, topic, timestamp
   FROM "sensors/+/temp"
   WHERE temperature > 30
   ```

3. 添加动作：配置一个数据集成，例如连接 InfluxDB 或 HTTP 服务，将筛选后的数据直接写入目标系统。

## 阶段三：更新设备与集成

### 更新客户端连接

由于 EMQX 使用标准 MQTT 端口（`1883` / `8883`），如果您的设备是通过 DNS 域名进行连接的，通常无需修改设备配置。

您只需更新 DNS 记录，将域名（例如 `mqtt.yourdomain.com`）指向 EMQX 集群的负载均衡器（Load Balancer）或集群节点的 IP 地址。

### 验证连接状态

通过 EMQX Dashboard 监控设备连接状态，确保客户端能够成功接入。

- 查看连接数是否随设备上线而增加。

  > 您也可以使用命令行工具验证连接状态：
  >
  > ```bash
  > emqx_ctl clients list
  > ```
  >
  > 或在 Dashboard 中通过菜单**监控** -> **客户端**查看连接详情。

- 检查日志是否存在认证错误（例如密码哈希算法不匹配或缺少证书等问题）。

## 高级迁移方案

本节适用于希望实现不中断服务迁移的场景。

### 桥接过渡策略（Zero Downtime）

若希望在迁移过程中不中断服务，可按以下步骤进行：

1. **并行部署 EMQX 与 Mosquitto。**

   在现有 Mosquitto 实例旁部署 EMQX 节点，确保两者可正常通信。

2. **配置 Mosquitto 桥接至 EMQX：**

   在 Mosquitto 中设置桥接，将所有消息转发至 EMQX。在 `mosquitto.conf` 中添加以下配置：

   ```properties
   # mosquitto.conf
   connection migrate_uplink
   address emqx-server:1883
   topic # out 0
   topic # in 0
   ```

3. **迁移消费者：**

   将后端应用程序或数据消费者的连接端点切换至 EMQX。 此时它们将同时接收来自 EMQX 连接设备与仍连接到 Mosquitto 的设备的数据。

4. **逐步迁移设备：**

   将终端设备逐步从 Mosquitto 的连接地址切换至 EMQX 的新接入点。

5. **下线 Mosquitto：**

   当确认 Mosquitto 已无活跃连接后，移除桥接配置并关闭 Mosquitto 服务。

## 验证清单

在切换生产环境流量之前，请确保以下项目均已验证通过：

-  **监听器：** TCP（1883）与 SSL（8883）端口已开放并可正常接受连接。
-  **认证：** 用户可使用原密码正常登录。
-  **ACL：** 用户仅能访问授权主题。
-  **数据流：** 设备发布的消息能被订阅端或后端应用正常接收。
-  **持久化：** Broker 重启后保留消息可正常恢复（确保 `retain_available = true`）。

## 总结

从 Mosquitto 迁移至 EMQX，可在保持协议兼容的同时显著提升系统的可扩展性与可靠性。通过映射原有配置，并利用 EMQX 的规则引擎替代外部脚本与桥接机制，可有效简化系统架构，为大规模 MQTT 部署做好准备。
