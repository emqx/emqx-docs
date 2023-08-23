# GreptimeDB

[GreptimeDB](https://github.com/GreptimeTeam/greptimedb) 是一个开源、分布式、云原生时序数据库，融合时序数据处理和分析能力。GreptimeDB 专为云而生，充分利用云的优势，如弹性、可扩展性和高可用性。

EMQX 目前支持通过数据桥接的方式连接不同版本的 GreptimeDB, GreptimeCloud 以及 GreptimeDB 企业版。

{% emqxce %}
:::tip
EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。
:::
{% endemqxce %}

:::tip 前置准备

- 了解 [规则](./rules.md)。
- 了解 [数据桥接](./data-bridges.md)。

:::

## 功能清单

- [批量模式](./data-bridges.md#批量模式)
- [缓存队列](./data-bridges.md#缓存队列)


## 快速开始

本节介绍如何使用 GreptimeDB 数据桥接，包括如何设置 GreptimeDB 服务器、创建用于将数据转发到 GreptimeDB 的数据桥接和规则，以及测试数据桥接和规则等主题。

本教程假设您在同一台本地机器上运行 EMQX 和 GreptimeDB。如果您在远程运行 GreptimeDB 和 EMQX，请相应调整设置。

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


### 创建 GreptimeDB 数据桥接

1. 转到 Dashboard **数据集成** -> **数据桥接**页面。

2. 点击页面右上角的**创建**。

3. 在**数据桥接类型**中选择 GreptimeDB，点击**下一步**。

4. 输入数据桥接名称，要求是大小写英文字母和数字的组合。

5. 输入 GreptimeDB 连接信息：
   - **服务器地址**：输入 `127.0.0.1:4001`。如果是 GreptimeCloud 需要指定端口为 443，即输入 `{url}:443` 。
   - **数据库**：输入数据库名称 `public`，如果 GreptiemCloud，请输入 service 名称。
   - **用户名**和**密码**：设置成 `greptime_user` 和 `greptime_pwd`。
   - **时间精度**：默认为毫秒。
   
7. 定义数据格式为 JSON 或 Line Protocol， GreptimeDB 使用和 InfluxDB 兼容的数据格式：

   - 对于 **JSON** 格式，需设置数据的 **Measurement**，**Fields**，**Timestamp** 与 **Tags**，键值均支持变量，可以使用[行协议](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/)进行设置。

   - 对于 **Line Protocol** 格式，请通过一段语句指定数据点的 Measurement、Fields、Timestamp 与 Tags，键值均支持变量，可按照[行协议](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/)进行设置。
   
     ::: tip
   
     - 如希望输入带符号的整型值，请在占位符后添加 `i` 作为类型标识，例如 `${payload.int}i`。
     - 对于无符号整型值，请在占位符后添加 `u` 作为类型标识，例如 `${payload.uint}u`。
   
     :::
   

8. 高级配置（可选），根据情况配置同步/异步模式，队列等参数，详细请参考[配置参数](./data-bridges.md)。

9. 设置完成后，您可点击**测试连接**按钮进行验证。

10. 点击**创建**按钮完成数据桥接创建。

至此您已经完成数据桥接创建流程。在数据桥接列表（集成 -> 数据桥接）中应出现 GreptimeDB 数据桥接，**资源状态**为`已连接`。

### 创建规则

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

### 测试数据桥接与规则

使用 MQTTX 向 `t/1` 主题发布消息，此操作同时会触发上下线事件：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello GreptimeDB" }'
```

分别查看两个数据桥接运行统计，命中、发送成功次数均 +1。

前往 GreptimeDB dashboard 查看数据是否已经写入 GreptimeDB 中。
