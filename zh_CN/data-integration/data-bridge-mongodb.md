# MongoDB

通过 MongoDB 数据桥接可以将客户端消息和事件存储到 MongoDB 中。

:::tip 前置准备

- 了解 [规则](./rules.md)。
- 了解 [数据桥接](./data-bridges.md)。

:::

## 功能清单

- [连接池](./data-bridges.md#连接池)
- [异步请求模式](./data-bridges.md#异步请求模式)
- [批量模式](./data-bridges.md#批量模式)
- [缓存队列](./data-bridges.md#缓存队列)

<!-- TODO 配置参数 需要补充链接到配置手册对应配置章节。 -->

## 快速开始

### 安装 MongoDB

通过 Docker 安装并启动 MongoDB：

```bash
# 启动一个 MongoDB 容器并设置密码为 public
docker run -d --name mongodb -p 27017:27017 mongo

# 进入容器
docker exec -it mongodb bash

# 在容器中连接到 MongoDB 服务器，
mongo

# 创建用户
use admin
db.createUser({ user: "admin", pwd: "public", roles: [ { role: "root", db: "admin" } ] })

# 创建名为 emqx_data 的数据库
use emqx_data

# 创建名为 emqx_messages 的集合
db.createCollection('emqx_messages')
```

### 连接到 MongoDB

创建 MongoDB 数据桥接完成对客户端发布消息的存储。

1. 转到 Dashboard **数据集成** -> **数据桥接**页面。
2. 点击页面右上角的创建。
3. 在数据桥接类型中选择 MongoDB，点击下一步。
4. 输入数据桥接名称，要求是大小写英文字母或数字组合。
5. **部署模式** 与 **SRV 记录**根据情况选择，此处选择 **single** 与不启用。
6. 输入 MongoDB 连接信息，**数据库名字**填写 **emqx_data**，**服务器地址**填写 **127.0.0.1:27017**，**用户名**填写 **admin**，**密码**填写 **public**，**集合**填写 **emqx_messages**，其余字段使用默认值即可。
7. 配置 **有效载荷模板**，该模板用于设置数据内容并将通过 MongoDB insert 操作插入到数据库中，此处将 `clientid`、`topic`、`qos`、`timestamp`、`payload` 字段进行存储，对应模板如下：

```json
{
  "clientid": "${clientid}",
  "topic": "${topic}",
  "qos": ${qos},
  "timestamp": ${timestamp},
  "payload": ${payload}
}
```

  :::tip
  有效载荷模板有几点注意事项：

  - 所有的**键**需要使用双引号 `"` 包裹；
  - 不支持自动推导**值**的数据类型：
    - 字符类型的字段需要使用 `"` 包裹，否则会报错；
    - 数值类型字段不需要包裹，如果包裹则识别为字符类型；
    - 时间戳、日期和时间类型不做特殊处理则识别为数值或字符类型，如果要以日期和时间类型存储，需要在规则 SQL 中使用 `mongo_date` 函数对字段进行处理，参考 [时间与日期函数](./rule-sql-builtin-functions.md#时间与日期函数)。
  - 允许嵌套对象，当 **值** 为 JSON 对象时：
    - 模板中禁止使用双引号嵌套该值，这会导致执行错误；
    - 对象将按自身结构嵌套存储；
    - 如需将对象存储为 JSON 字符，可以在规则 SQL 中使用 `json_encode` 函数转换，模板中对应的**值**仍然禁止使用双引号包裹。
  :::

1. 高级配置（可选），根据情况配置同步/异步模式，队列与批量等参数，详细请参考[配置参数](#配置参数)。
2. 点击创建按钮完成数据桥接创建。

至此您已经完成数据桥接创建，接下来将继续创建一条规则来指定需要写入的数据：

1. 转到 Dashboard **数据集成** -> **规则**页面。
2. 点击页面右上角的创建。
3. 输入规则 ID `my_rule`，在 SQL 编辑器中输入规则，此处选择将 `t/#` 主题的 MQTT 消息存储至 MongoDB，请确规则选择出来的字段（SELECT 部分）包含所有 SQL 模板中用到的变量。此处规则 SQL 如下：

```sql
SELECT
  *
FROM
  "t/#"
```

您也可以使用以下 SQL 将 `timestamp` 保存为日期类型数据、将 JSON 格式的 `payload` 保存为 JSON 字符串：

```sql
SELECT
  *,
  mongo_date(timestamp) as timestamp,
  json_decode(payload) as payload
FROM
  "t/#"
```

4. 添加动作，在动作下拉框中选择 使用数据桥接转发 选项，选择先前创建好的 MongoDB 数据桥接。
5. 点击最下方创建按钮完成规则创建。

至此您已经完成整个创建过程，可以前往 **数据集成** -> **Flows** 页面查看拓扑图，此时应当看到 `t/#` 主题的消息经过名为 `my_rule` 的规则处理，处理结果交由 MongoDB 存储。


### 测试

使用 MQTTX 向 `t/1` 主题发布消息：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello MongoDB" }'
```

查看数据桥接运行统计，命中、发送成功次数均 +1。

查看数据是否已经写入`emqx_messages` 集合中：

```bash
> db.emqx_messages.find().pretty()
{
    "_id" : ObjectId("63db7059df489d01ed000009"),
    "clientid" : "emqx_c",
    "payload" : {
      "msg" : "hello MongoDB"
    },
    "qos" : 0,
    "timestamp" : NumberLong("1675325529070"),
    "topic" : "t/1"
}
```

使用第二种规则 SQL 时，对应数据内容如下：

```bash
> db.emqx_messages.find().pretty()
{
    "_id" : ObjectId("63db7535df489d01ed000013"),
    "clientid" : "emqx_c",
    "payload" : "{ \"msg\": \"hello MongoDB\" }",
    "qos" : 0,
    "timestamp" : ISODate("2023-02-02T08:33:36.715Z"),
    "topic" : "t/1"
}
```
