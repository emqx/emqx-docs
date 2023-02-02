# PostgreSQL

通过 PostgreSQL 数据桥接可以将客户端消息和事件存储到 PostgreSQL 中，也可以通过事件触发对 PostgreSQL 中数据的更新或删除操作，从而实现对诸如设备在线状态、上下线历史等的记录。

*注意*: 这个文档也适用于Timescale 以及 MatrixDB。

## 先决条件

- 了解 [规则](./rules.md)。
- 了解 [数据桥接](./data-bridges.md)。

## 特性

- [连接池](./data-bridges.md#连接池)
- [异步请求模式](./data-bridges.md#异步请求模式)
- [批量模式](./data-bridges.md#批量模式)
- [缓存队列](./data-bridges.md#缓存队列)
- [SQL 预处理](./data-bridges.md#SQL-预处理)

## 配置参数
<!-- TODO 链接到配置手册对应配置章节。 -->

## 快速开始

### 安装 PostgreSQL

通过 Docker 安装并启动 PostgreSQL：

```bash
# 启动一个 PostgreSQL 容器并设置密码为 public
docker run --name PostgreSQL -p 5432:5432 -e POSTGRES_PASSWORD=public -d postgres

# 进入容器
docker exec -it PostgreSQL bash

# 在容器中连接到 PostgreSQL 服务器，需要输入预设的密码
psql -U postgres -W

# 创建并选择数据库

CREATE DATABASE emqx_data;

\c emqx_data;
```

### 连接到 PostgreSQL

需要创建两个 PostgreSQL 数据桥接分别完成消息存储与事件记录：

#### 消息存储

本节我们将创建第一个 PostgreSQL 数据桥接来实现对客户端发布消息的存储。

1. 转到 Dashboard **数据集成** -> **数据桥接**页面。
2. 点击页面右上角的创建。
3. 在数据桥接类型中选择 PostgreSQL，点击下一步。
4. 输入数据桥接名称，要求是大小写英文字母或数字组合。
5. 输入 PostgreSQL 连接信息，主机列表填写 **127.0.0.1:3306**，数据库填写 `emqx_data`，用户名为 `postgres`，密码为 `public`。
6. 配置 SQL 模板，使用如下 SQL 完成数据插入，此处为[预处理 SQL](./data-bridges.md#sql-预处理)，字段不应当包含引号，SQL 末尾不要带分号 `;`:

  ```sql
  INSERT INTO t_mqtt_msg(msgid, sender, topic, qos, payload, arrived) VALUES(
    ${id},
    ${clientid},
    ${topic},
    ${qos},
    ${payload},
    TO_TIMESTAMP((${timestamp} :: bigint)/1000)
  )
  ```
  
  请在 PostgreSQL 中使用以下 SQL 创建存储消息的数据表 `t_mqtt_msg`，该表存储每条消息的发布者客户端 ID、主题、Payload 以及发布时间：

  ```sql
    CREATE TABLE t_mqtt_msg (
      id SERIAL primary key,
      msgid character varying(64),
      sender character varying(64),
      topic character varying(255),
      qos integer,
      retain integer,
      payload text,
      arrived timestamp without time zone
    );
  ```

7. 高级配置（可选），根据情况配置同步/异步模式，队列与批量等参数，详细请参考[配置参数](#配置参数)。
8. 点击创建按钮完成数据桥接创建。

至此您已经完成数据桥接创建，接下来将继续创建一条规则来指定需要写入的数据：

1. 转到 Dashboard **数据集成** -> **规则**页面。
2. 点击页面右上角的创建。
3. 输入规则 ID `my_rule`，在 SQL 编辑器中输入规则，此处选择将 `t/#` 主题的 MQTT 消息存储至 PostgreSQL，请确规则选择出来的字段（SELECT 部分）包含所有 SQL 模板中用到的变量，此处规则 SQL 如下：

  ```sql
  SELECT 
    *
  FROM
    "t/#"
  ```
4. 添加动作，在动作下拉框中选择 使用数据桥接转发 选项，选择先前创建好的 PostgreSQL 数据桥接。
5. 点击最下方创建按钮完成规则创建。

至此您已经完成整个创建过程，可以前往 **数据集成** -> **Flows** 页面查看拓扑图，此时应当看到 `t/#` 主题的消息经过名为 `my_rule` 的规则处理，处理结果交由 PostgreSQL 存储。

#### 上下线记录

在上面一个章节，我们实现了对指定主题消息的存储，本节我们将演示如何通过 PostgreSQL 实现对设备上下线的记录。

注意：除 SQL 模板与规则外，其他操作步骤与[消息存储](#消息存储)章节完全相同。

数据桥接的 SQL 模板如下，请注意字段不应当包含引号，SQL 末尾不要带分号 `;`:

```sql
INSERT INTO emqx_client_events(clientid, event, created_at) VALUES (
  ${clientid},
  ${event},
  TO_TIMESTAMP((${timestamp} :: bigint)/1000)
)
```

  请在 PostgreSQL 中使用以下 SQL 创建存储消息的数据表 `emqx_client_events`，该表存储上下线的客户端 ID、事件类型以及事件发生时间：

```sql
CREATE TABLE emqx_client_events (
  id SERIAL primary key,
  clientid VARCHAR(255),
  event VARCHAR(255),
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

规则 SQL 如下：

```sql
SELECT
  *
FROM 
  "$events/client_connected", "$events/client_disconnected"
```

### 测试

使用 MQTTX 向 `t/1` 主题发布消息，此操作同时会触发上下线事件：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello PostgreSQL" }'
```

分别查看两个数据桥接运行统计，命中、发送成功次数均 +1。

查看数据是否已经写入表中，`emqx_messages` 表：

```bash
emqx_data=# select * from t_mqtt_msg;
 id |              msgid               | sender | topic | qos | retain |            payload
        |       arrived
----+----------------------------------+--------+-------+-----+--------+-------------------------------+---------------------
  1 | 0005F298A0F0AEE2F443000012DC0002 | emqx_c | t/1   |   0 |        | { "msg": "hello PostgreSQL" } | 2023-01-19 07:10:32
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
