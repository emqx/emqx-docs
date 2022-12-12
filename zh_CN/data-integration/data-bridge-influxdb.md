# InfluxDB Bridge

InfluxDB 是一个用于存储和分析时间序列数据的数据库，其强大的数据吞吐能力以及稳定的性能表现使其非常适合物联网领域。InfluxDB Bridge 完整支持 InfluxDB Cloud、InfluxDB OSS 以及 InfluxDB Enterprise 多种软件类型以及不同版本。

## 先决条件

- 了解 [InfluxDB 行协议](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/)，InfluxDB Bridge 使用行协议进行数据写入。
- 了解 [规则](./rules.md)。
- 了解 [数据桥接](./data-bridges.md)。

## 特性

- [连接池](./data-bridges.md#连接池) <!-- TODO 确认改版后知否支持-->
- [异步请求模式](./data-bridges.md#异步请求模式)
- [批量模式](./data-bridges.md#批量模式)
- [缓存队列](./data-bridges.md#缓存队列)

## 配置参数
<!-- TODO 链接到配置手册对应配置章节。 -->

## 快速开始

### 安装 InfluxDB

1. 通过 Docker 安装并启动 InfluxDB，详细步骤请参考 [Install InfluxDB](https://docs.influxdata.com/influxdb/v2.5/install/)。

```bash
# 启动一个 InfluxDB 容器
docker run --name influxdb -p 8086:8086 influxdb:2.5.1
```

2. 浏览器访问 http://localhost:8086 打开 InfluxDB UI，设置用户名、密码、组织名称、Bucket 名称。

3. 前往 InfluxDB UI Load Data -> API Token，按照 [Create All-Access tokens](https://docs.influxdata.com/influxdb/v2.5/install/#create-all-access-tokens) 指引创建 Token。

### 创建 InfluxDB Bridge

1. 转到 Dashboard 数据集成 -> 数据桥接页面。
2. 点击页面右上角的创建。
3. 在数据桥接类型中选择 InfluxDB，点击下一步。
4. 输入数据桥接名称，要求是大小写英文字母或数字组合。
5. 根据情况选择 InfluxDB 版本，默认为 V2。
6. 输入 InfluxDB 连接信息
   1. 服务器地址填写 **127.0.0.1:8086**。如果是 InfluxDB Cloud 需要指定端口为 443，即填入 **{url}:443** 并启用 TLS 连接。
   2. 选择 Token 认证，填入先前设置的组织名称、Bucket 以及生成的 Toekn。
7. 定义解析数据，设置数据的 Measurement，Fields，Timestamp 与 Tags，键值均支持变量，可以使用行协议进行设置。
8. 调优配置（可选），根据情况配置同步/异步模式，队列与批量等参数，详细请参考[配置配置](#配置参数)。
9.  点击创建按钮完成数据桥接创建。

至此您已经完成数据桥接创建流程，为了指定需要写入的数据，您还需要创建一条规则并在规则中选择该数据桥接作为动作：

1. 转到 Dashboard 数据集成 -> 规则页面。
2. 点击页面右上角的创建。
3. 输入规则 ID `my_rule`，在 SQL 编辑器中输入规则，此处选择将 `t/#` 主题的 MQTT 消息存储至 InfluxDB，请确规则选择出来的字段（SELECT 部分）包含上述数据桥接所需的字段，此处规则 SQL 如下：

  ```sql
  SELECT 
    *
  FROM
    "t/#"
  ```

4. 添加动作，在动作下拉框中选择 使用数据桥接转发 选项，选择先前创建好的 InfluxDB Bridge。
5. 点击最下方创建按钮完成规则创建。

至此您已经完成整个创建过程，可以前往 数据集成 -> Flows 页面查看拓扑图，此时应当看到 `t/#` 主题的消息经过名为 `my_rule` 的规则处理，处理结果交由 InfluxDB Bridge 进行存储。

### 测试

使用 MQTTX 向 `t/1` 主题发布消息，此操作同时会触发上下线事件：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello InfluxDB" }'
```

分别查看两个数据桥接运行统计，命中、发送成功次数均 +1。

前往 InfluxDB UI Data Explorer 查看数据是否已经写入 InfluxDB 中。
