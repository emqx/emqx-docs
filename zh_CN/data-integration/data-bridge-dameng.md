# 将 MQTT 数据写入到达梦数据库

[达梦数据库（DM8）](https://www.dameng.com/) 是国产的大型通用数据库管理系统，被广泛应用于政务、金融、电力、电信等行业。EMQX 支持与达梦数据库集成，使您能够将 MQTT 消息和客户端事件保存到达梦数据库以便构建数据管道、进行分析，或进行设备连接管理与系统集成。

本页详细介绍了 EMQX 与达梦数据库的数据集成，并提供了实用的规则和动作创建指导。

::: tip

仅 EMQX 6.x 版本支持达梦数据库 Sink 功能。达梦集成通过 ODBC 驱动完成，需要先在运行 EMQX 的机器上配置 unixODBC 与达梦 ODBC 驱动（见下文）。

:::

## 工作原理

达梦数据库数据集成是 EMQX 的开箱即用功能，结合了 EMQX 的设备接入、消息传输能力与达梦数据库强大的数据存储能力。通过内置的[规则引擎](./rules.md)组件和动作（Sink），您可以将 MQTT 消息和客户端事件存储到达梦数据库中，也可以通过事件触发对达梦数据库中数据的更新或删除操作。

将 MQTT 数据摄取到达梦数据库的工作流程如下：

1. **消息发布和接收**：工业物联网设备通过 MQTT 协议成功连接到 EMQX，并根据其运行状态、读数或触发的事件，发布实时 MQTT 数据到 EMQX。当 EMQX 接收到这些消息时，它将在其规则引擎中启动匹配过程。
2. **消息数据处理**：当消息到达时，通过规则引擎处理。规则根据预定义的标准确定哪些消息需要路由到达梦数据库。如果任何规则指定了载荷转换，那么这些转换将被应用。
3. **数据写入到达梦数据库**：规则触发将消息写入达梦数据库的动作。借助 SQL 模板，用户可以从规则处理结果中提取数据来构造 SQL，并通过 ODBC 发送到达梦数据库执行，从而将消息的特定字段写入或更新到数据库的相应表和列中。
4. **数据存储和利用**：数据现存储在达梦数据库中，企业可以利用其查询能力应用于各种用例。

## 特性与优势

与达梦数据库的数据集成提供了一系列特性和优势：

- **实时数据流**：EMQX 专为处理实时数据流而构建，确保从源系统到达梦数据库的数据传输的高效性和可靠性。
- **高性能和可扩展性**：EMQX 和达梦数据库都具有扩展性和可靠性，适用于处理大规模的物联网数据。
- **数据转换的灵活性**：EMQX 提供了强大的基于 SQL 的规则引擎，允许在将数据存储到达梦数据库之前进行预处理。
- **批量写入**：集成使用 `odbc:param_query` 进行批量参数化写入，显著降低网络往返与数据库开销。

## 准备工作

本节介绍在 EMQX 中创建达梦数据集成之前需要做的准备工作，包括如何安装并配置 ODBC 驱动程序、设置达梦数据库服务器并创建数据库和数据表。

### 前置准备

- 了解[规则](./rules.md)。
- 了解[数据集成](./data-bridges.md)。

### 安装并配置 ODBC 驱动程序

为了能够访问达梦数据库，您需要在运行 EMQX 的机器上安装并配置 unixODBC 与达梦 ODBC 驱动。

::: tip 重要

EMQX 使用 Erlang/OTP 的 `odbc` 应用与达梦建立连接。`odbc` 的 `odbcserver` 端口程序从 **`/etc/`**（而非 `/usr/local/etc/`）读取 ODBC 配置。因此 **`odbcinst.ini` 与 `odbc.ini` 必须放在 `/etc/` 下**（或软链到 `/etc/`），否则 `DSN=`/驱动名无法解析。

:::

1. 安装 unixODBC 与达梦 ODBC 驱动（达梦安装包位于 `/opt/dmdbms`，驱动为 `/opt/dmdbms/bin/libdodbc.so`）。
2. 编辑 `/etc/odbcinst.ini`，加入达梦驱动：
   ```
   [DM8 ODBC DRIVER]
   Description = ODBC DRIVER FOR DM8
   Driver = /opt/dmdbms/bin/libdodbc.so
   ```
3. 编辑 `/etc/odbc.ini`，配置数据源（DSN）：
   ```
   [dm8]
   Description = DM ODBC DSN
   Driver = DM8 ODBC DRIVER
   SERVER = 192.168.1.10
   UID = SYSDBA
   PWD = 你的密码
   TCP_PORT = 5237
   ```
   ::: tip
   也可以不配置 DSN，在连接器里直接填 `driver`（驱动名或 `.so` 绝对路径）和 `server/port/username/password`（字段连接串形式）。
   :::
4. 验证连接：`odbcinst -j` 确认 unixODBC 与驱动；用 `isql dm8 SYSDBA 你的密码` 执行 `select 1` 确认可达。

## 创建连接器

在 EMQX 中执行以下命令创建达梦连接器：

```bash
curl -XPOST http://localhost:18083/api/v5/connectors \
  -d '{
    "type": "dameng",
    "name": "dameng",
    "enable": true,
    "server": "192.168.1.10",
    "port": 5237,
    "username": "SYSDBA",
    "password": "你的密码",
    "driver": "DM8 ODBC DRIVER",
    "database": "DM8",
    "charset": "utf8",
    "pool_size": 8,
    "resource_opts": {"health_check_interval": "20s"}
  }'
```

主要连接参数：

| 参数 | 说明 |
| --- | --- |
| `server` | 达梦主机（`host` 或 `host:port`）。 |
| `port` | 达梦端口，默认 `5236`。 |
| `username` / `password` | 登录用户（默认 `SYSDBA`）与密码。 |
| `driver` | ODBC 驱动名（如 `DM8 ODBC DRIVER`，需 `/etc/odbcinst.ini` 已配置）或驱动 `.so` 绝对路径（如 `/opt/dmdbms/bin/libdodbc.so`）。 |
| `dsn` | 可选；配置了 `/etc/odbc.ini` 的 DSN 时可使用 `DSN=<名称>` 直连。 |
| `database` | 数据库名（透传给 ODBC 连接串 `Database=`，如 `DM8`）。 |
| `charset` | 字符集（透传给 `Charset=`，如 `utf8`）。 |
| `pool_size` | 连接池大小，默认 `8`。 |

## 创建动作（规则 Sink）

在 EMQX 中执行以下命令创建达梦动作：

```bash
curl -XPOST http://localhost:18083/api/v5/actions \
  -d '{
    "type": "dameng",
    "name": "dameng",
    "parameters": {
      "connector": "dameng",
      "sql": "insert into SYSDBA.t_mqtt_msg(msgid, topic, qos, payload) values ( ${id}, ${topic}, ${qos}, ${payload} )",
      "undefined_vars_as_null": false
    },
    "resource_opts": {"batch_size": 100, "batch_time": "100ms", "query_mode": "sync"}
  }'
```

动作参数：

| 参数 | 说明 |
| --- | --- |
| `sql` | 插入 SQL 模板（占位符 `${...}` 取自规则处理结果）。**INSERT 必须显式列出字段**，连接器会在动作创建时用 `describe_table` 探测表的列类型。 |
| `undefined_vars_as_null` | 为 `true` 时，未匹配到的变量按 `null` 写入；为 `false`（默认）时，未匹配变量按错误处理。 |
| `resource_opts.batch_size` | 批量行数，默认 `100`。 |
| `resource_opts.batch_time` | 批量聚合时间，默认 `100ms`。 |

### SQL 模板示例

```sql
insert into SYSDBA.t_mqtt_msg(msgid, topic, qos, payload)
values ( ${id}, ${topic}, ${qos}, ${payload} )
```

- `${id}`、`${topic}`、`${qos}`、`${payload}` 等占位符取自规则的输出字段。
- 批量写入使用参数化查询（`odbc:param_query`），无需手动转义字符串。

## 创建规则

在 EMQX 中创建规则，选择 `dameng` 动作，即可将匹配的消息写入达梦数据库。示例：

```bash
curl -XPOST http://localhost:18083/api/v5/rules \
  -d '{
    "name": "写入达梦",
    "sql": "SELECT * FROM \"t/#\"",
    "actions": [{"function": "dameng:dameng"}]
  }'
```

## 达梦数据库数据集成示例

发布一条消息到规则匹配的主题，即可看到数据写入达梦：

```bash
mosquitto_pub -t 't/1' -m 'hello dameng'
```

之后查询达梦表：

```sql
SELECT * FROM SYSDBA.t_mqtt_msg;
```

## 注意事项

- 达梦连接串支持 `DSN=` 与字段模式两种写法。字段模式下 `Driver` 可填驱动名或 `.so` 绝对路径。
- 若使用 DSN/驱动名，务必保证 `/etc/odbcinst.ini` 与 `/etc/odbc.ini` 配置正确（Erlang `odbcserver` 读 `/etc/`）。
- 批量写入依赖 `odbc:param_query`，所有行的列数必须一致。
- 若规则输出字段与目标列不匹配，可设置 `undefined_vars_as_null: true` 以 `null` 填充缺失字段。
