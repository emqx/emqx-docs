# TDengine

通过 TDengine 数据桥接可以将客户端消息和事件存储到 TDengine 中，也可以通过事件触发对 TDengine 中数据的更新或删除操作，从而实现对诸如设备在线状态、上下线历史等的记录。

## 先决条件

- 了解 [规则](./rules.md)。
- 了解 [数据桥接](./data-bridges.md)。

## 特性

- [连接池](./data-bridges.md#连接池)
- [异步请求模式](./data-bridges.md#异步请求模式)
- [批量模式](./data-bridges.md#批量模式)
- [缓存队列](./data-bridges.md#缓存队列)
- [SQL 预处理](./data-bridges.md#SQL-预处理)

## 快速开始

### 安装 TDengine

通过 Docker 安装并启动 TDengine：

```bash
# 启动一个 TDengine 容器
docker run --name TDengine -p 6041:6041 tdengine/tdengine

# 进入容器
docker exec -it TDengine bash

# 在容器中连接到 TDengine 服务器
taos

# 创建并选择数据库

CREATE DATABASE mqtt;

use mqtt;
```

### 连接到 TDengine

需要创建两个 TDengine 数据桥接分别完成消息存储与事件记录：

#### 消息存储

本节我们将创建第一个 TDengine 数据桥接来实现对客户端发布消息的存储。

1. 转到 Dashboard **数据集成** -> **数据桥接**页面。
2. 点击页面右上角的创建。
3. 在数据桥接类型中选择 TDengine，点击下一步。
4. 输入数据桥接名称，要求是大小写英文字母或数字组合。
5. 输入 TDengine 连接信息，主机列表填写 **127.0.0.1:6041**，数据库填写 `mqtt`，用户名为 `root`，密码为 `taosdata`。
6. 配置 SQL 模板，使用如下 SQL 完成数据插入，此处为[预处理 SQL](./data-bridges.md#sql-预处理)，字段不应当包含引号，SQL 末尾不要带分号 `;`:

  ```sql
    INSERT INTO mqtt.t_mqtt_msg(ts, msgid, mqtt_topic, qos, payload, arrived) 
      VALUES (${ts}, ${id}, ${topic}, ${qos}, ${payload}, ${timestamp})
  ```
  
  请在 TDengine 中使用以下 SQL 创建存储消息的数据表 `t_mqtt_msg`，该表存储每条消息的发布者客户端 ID、主题、Payload 以及发布时间：

  ```sql
    CREATE TABLE t_mqtt_msg (
      ts timestamp,
      msgid NCHAR(64),
      mqtt_topic NCHAR(255),
      qos TINYINT,
      payload BINARY(1024),
      arrived timestamp
    );
  ```

7. 高级配置（可选），根据情况配置同步/异步模式，队列与批量等参数，详细请参考[配置参数](#配置参数)。
8. 点击创建按钮完成数据桥接创建。

至此您已经完成数据桥接创建，接下来将继续创建一条规则来指定需要写入的数据：

1. 转到 Dashboard **数据集成** -> **规则**页面。
2. 点击页面右上角的创建。
3. 输入规则 ID `my_rule`，在 SQL 编辑器中输入规则，此处选择将 `t/#` 主题的 MQTT 消息存储至 TDengine，请确认规则选出的字段（SELECT 部分）包含所有 SQL 模板中用到的变量，此处规则 SQL 如下：

  ```sql
    SELECT
      *,
      now_timestamp('millisecond')  as ts
    FROM
      "t/#"
  ```

4. 添加动作，在动作下拉框中选择 使用数据桥接转发 选项，选择先前创建好的 TDengine 数据桥接。
5. 点击最下方创建按钮完成规则创建。

至此您已经完成整个创建过程，可以前往 **数据集成** -> **Flows** 页面查看拓扑图，此时应当看到 `t/#` 主题的消息经过名为 `my_rule` 的规则处理，处理结果交由 TDengine 存储。

#### 上下线记录

在上面一个章节，我们实现了对指定主题消息的存储，本节我们将演示如何通过 TDengine 实现对设备上下线的记录。

注意：除 SQL 模板与规则外，其他操作步骤与[消息存储](#消息存储)章节完全相同。

数据桥接的 SQL 模板如下，请注意字段不应当包含引号，SQL 末尾不要带 `;`:

```sql
    INSERT INTO emqx_client_events(ts, clientid, event) VALUES (
      ${ts},
      ${clientid},
      ${event}
    )
```

  请在 TDengine 中使用以下 SQL 创建存储消息的数据表 `emqx_client_events`，该表存储上下线的客户端 ID、事件类型以及事件发生时间：

```sql
    CREATE TABLE emqx_client_events (
      ts timestamp,
      clientid VARCHAR(255),
      event VARCHAR(255)
    );
```

规则 SQL 如下：

```sql
    SELECT
      *,
      now_timestamp('millisecond')  as ts
    FROM 
      "$events/client_connected", "$events/client_disconnected"
```

### 测试

使用 MQTTX 向 `t/1` 主题发布消息，此操作同时会触发上下线事件：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello TDengine" }'
```

分别查看两个数据桥接运行统计，命中、发送成功次数均 +1。

查看数据是否已经写入表中，`emqx_messages` 表：

```bash
emqx_data=# select * from t_mqtt_msg;
 id |              msgid               | sender | topic | qos | retain |            payload
        |       arrived
----+----------------------------------+--------+-------+-----+--------+-------------------------------+---------------------
  1 | 0005F298A0F0AEE2F443000012DC0002 | emqx_c | t/1   |   0 |        | { "msg": "hello TDengine" } | 2023-01-19 07:10:32
(1 row)
```

`emqx_client_events` 表：

```bash
emqx_data=# select * from emqx_client_events;
 id | clientid |        event        |     created_at
----+----------+---------------------+---------------------
  3 | emqx_c   | client.connected    | 2023-01-19 07:10:32
  4 | emqx_c   | client.disconnected | 2023-01-19 07:10:32
(2 rows)
```
