# 将 MQTT 数据写入到 GreptimeDB

{% emqxce %}
:::tip
EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。
:::
{% endemqxce %}

[GreptimeDB](https://github.com/GreptimeTeam/greptimedb) 是一个开源、分布式、云原生时序数据库，融合时序数据处理和分析能力。GreptimeDB 专为云而生，充分利用云的优势，如弹性、可扩展性和高可用性。EMQX 目前支持通过数据桥接的方式连接不同版本的 GreptimeDB, GreptimeCloud 以及 GreptimeDB 企业版。

本页详细介绍了 EMQX 与 GreptimeDB 的数据集成并提供了实用的规则和数据桥接创建指导。

## 工作原理

GreptimeDB 数据集成是 EMQX 开箱即用的功能，它结合了 EMQX 的实时数据捕获和传输能力以及 GreptimeDB 的数据存储和分析能力。

下图展示了 EMQX 和 GreptimeDB 之间的数据集成的典型架构：

![EMQX-GreptimeDB 集成](./assets/emqx-integration-greptimedb.png)

通过内置的[规则引擎](./rules.md)组件，集成简化了从 EMQX 到 GreptimeDB 的数据摄取过程，无需复杂编码。工作流程如下：

1. **消息发布和接收**：工业设备通过 MQTT 协议成功连接到 EMQX，并定期使用 MQTT 协议发布能耗数据。这些数据包括生产线标识符和能耗值。当 EMQX 接收到这些消息时，它将在其规则引擎中启动匹配过程。
2. **规则引擎处理消息**：内置的规则引擎根据主题匹配处理来自特定来源的消息。当消息到达时，它通过规则引擎进行匹配，规则引擎将处理消息数据。这可能包括转换数据格式、过滤特定信息或用上下文信息丰富消息。
3. **数据写入到 GreptimeDB**：规则引擎中定义的规则触发操作将消息写入 GreptimeDB。GreptimeDB 数据桥提供 Line Protocol 模板，允许灵活定义数据格式，将特定消息字段写入 GreptimeDB 中相应的表和列。

将能耗数据写入 GreptimeDB 后，您可以灵活使用 SQL 语句或 Prometheus 查询语言来分析数据。例如：

- 连接到如 Grafana 等可视化工具以生成图表并显示能耗数据。
- 连接到 ERP 等应用系统进行生产分析和生产计划调整。
- 连接到业务系统以进行实时能源使用分析，促进以数据驱动的能源管理。

## 特性与优势

与 GreptimeDB 的数据集成为您的业务带来以下特性和优势：

- **易于上手使用**：EMQX 与 GreptimeDB 在开发、部署方面均提供了用户友好的使用体验。EMQX 提供了标准的 MQTT 协议以及开箱即用的各类认证、授权和集成功能，GreptimeDB 提供了 Time-Series Table，schemaless 等友好设计。两者的集成能够加快业务的整合与开发过程。
- **高效数据处理**：EMQX 能够高效处理大量物联网设备连接和消息吞吐量。GreptimeDB 在数据写入、存储和查询方面表现出色，满足物联网场景下的数据处理需求，不会对系统造成过大压力。
- **消息转换**：消息可以在写入 GreptimeDB 之前在 EMQX 规则中进行丰富的处理和转换。
- **高效存储和可扩展性**：EMQX 和 GreptimeDB 都具有集群扩展能力，允许随着业务增长灵活地水平扩展以满足不断扩大的需求。
- **高级查询能力**：GreptimeDB 为时戳数据的高效查询和分析提供了优化的功能、操作符和索引技术，使得能够从物联网时间序列数据中提取精确的洞察。


## 准备工作

本节介绍了在 EMQX 中创建 GreptimeDB 数据桥接之前需要做的准备工作，包括如何设置 GreptimeDB 服务器。

### 前置准备

- 了解 [规则](./rules.md)。
- 了解[数据集成](./data-bridges.md)。

### 安装 GreptimeDB

1. 通过 Docker 安装并启动 GreptimeDB，详细步骤请参考[下载安装GreptimeDB](https://greptime.cn/download)。

```bash
# 启动一个 GreptimeDB 容器
docker run -p 4000-4004:4000-4004 \
-p 4242:4242 -v "$(pwd)/greptimedb:/tmp/greptimedb" \
--name greptime --rm \
greptime/greptimedb standalone start \
--http-addr 0.0.0.0:4000 \
--rpc-addr 0.0.0.0:4001 \
--mysql-addr 0.0.0.0:4002 \
--user-provider=static_user_provider:cmd:greptime_user=greptime_pwd
```

2. `user-provider` 参数指定了 GreptimeDB 的用户鉴权账户，你还可以通过文件的方式指定，参考[鉴权](https://docs.greptime.cn/user-guide/clients/authentication#authentication)文档。

3. GreptimeDB 正常启动后，你可以通过 [http://localhost:4000/dashboard](http://localhost:4000/dashboard) 访问 GreptimeDB Dashboard，其中 username 和 password 分别输入 `greptime_user` 和 `greptime_pwd`。


## 创建 GreptimeDB 数据桥接

本节演示了如何在 Dashboard 中创建一个 GreptimeDB 数据桥接。以下示例假设您在同一台本地机器上运行 EMQX 和 GreptimeDB。如果您在远程运行 GreptimeDB 和 EMQX，请相应调整设置。

1. 转到 Dashboard **数据集成** -> **数据桥接**页面。

2. 点击页面右上角的**创建**。

3. 在**数据桥接类型**中选择 GreptimeDB，点击**下一步**。

4. 输入数据桥接名称，要求是大小写英文字母和数字的组合。

5. 输入 GreptimeDB 连接信息：
   - **服务器地址**：输入 `127.0.0.1:4001`。如果是 GreptimeCloud 需要指定端口为 443，即输入 `{url}:443` 。
   - **数据库**：输入数据库名称 `public`，如果 GreptiemCloud，请输入 service 名称。
   - **用户名**和**密码**：设置成 `greptime_user` 和 `greptime_pwd`。
   - **时间精度**：默认为毫秒。

6. 配置数据格式，通过一段语句指定数据点的测量、标签集、字段集和时间戳，键值均支持变量，可按照[行协议](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/)进行设置。<!--定义数据格式为 JSON 或 Line Protocol， -->GreptimeDB 使用和 InfluxDB 兼容的数据格式。

   <!--对于 **JSON** 格式，需设置数据的 **Measurement**，**Fields**，**Timestamp** 与 **Tags**，键值均支持变量，可以使用[行协议](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/)进行设置。-->

   <!--对于 **Line Protocol** 格式，请通过一段语句指定数据点的 Measurement、Fields、Timestamp 与 Tags，键值均支持变量，可按照[行协议](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/)进行设置。-->

   ::: tip

   - 如希望输入带符号的整型值，请在占位符后添加 `i` 作为类型标识，例如 `${payload.int}i`。
   - 对于无符号整型值，请在占位符后添加 `u` 作为类型标识，例如 `${payload.uint}u`。

   :::

7. 高级配置（可选），根据情况配置同步/异步模式，队列等参数，详细请参考[配置参数](./data-bridges.md)。

8. 设置完成后，您可点击**测试连接**按钮进行验证。

9. 点击**创建**按钮完成数据桥接创建。

至此您已经完成数据桥接创建流程。在数据桥接列表（集成 -> 数据桥接）中应出现 GreptimeDB 数据桥接，**资源状态**为`已连接`。

## 创建规则

您可以继续创建一条规则来指定需要写入的数据。

1. 转到 Dashboard **数据集成** -> **规则页面**。
2. 点击页面右上角的**创建**。
3. 输入规则 ID `my_rule`，在 SQL 编辑器中输入规则，此处选择将 `t/#` 主题的 MQTT 消息存储至 GreptimeDB，请确规则选择出来的字段（SELECT 部分）包含第 7 步中用到的变量，此处规则 SQL 如下：


  ```sql
  SELECT
    *
  FROM
    "t/#"
  ```

4. 添加动作，在动作下拉框中选择 使用数据桥接转发 选项，选择先前创建好的 GreptimeDB 数据桥接。
5. 点击最下方创建按钮完成规则创建。

至此您已经完成整个创建过程，可以前往 **数据集成** -> **Flows** 页面查看拓扑图，此时应当看到 `t/#` 主题的消息经过名为 `my_rule` 的规则处理，处理结果交由 GreptimeDB 进行存储。

## 测试数据桥接与规则

使用 MQTTX 向 `t/1` 主题发布消息，此操作同时会触发上下线事件：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello GreptimeDB" }'
```

分别查看两个数据桥接运行统计，命中、发送成功次数均 +1。

前往 GreptimeDB dashboard 查看数据是否已经写入 GreptimeDB 中。
