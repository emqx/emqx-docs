# 升级到 EMQX 7.0 前删除旧版队列和消息流

本页介绍如何在 EMQX 6.1 上查找并删除旧版消息队列和 MQTT 消息流，以便之后升级到 EMQX 7.0。

旧版队列或消息流是指在 EMQX 6.1.1 之前创建的队列或消息流。当时队列和消息流没有自己的名称。客户端只能通过 `$q/<topic_filter>` 前缀访问旧版队列，只能通过 `$s/<offset>/<topic_filter>` 前缀访问旧版消息流。EMQX 7.0 移除了这两个前缀。请在升级到 EMQX 7.0 之前、客户端仍可使用这两个前缀时，完成本页中的步骤。

## 受影响的用户

同时满足以下两个条件时，您会受到影响：

- 您的集群在 EMQX 6.1.0 上创建过队列或消息流，之后升级到了 EMQX 6.1.1 或更高版本。
- 您之后没有删除这些队列或消息流。

从 EMQX 6.1.1 或更高版本开始部署的集群没有旧版队列或消息流。EMQX 6.1.1 及更高版本不会创建新的旧版记录，即使客户端使用 `$q/` 前缀订阅也不会。

## 不清理就升级的后果

如果在存在旧版队列或消息流的情况下升级到 EMQX 7.0：

- 旧版记录及其存储的消息仍然保留，并继续占用存储空间。
- 客户端无法再订阅该记录。EMQX 7.0 会将 `$q/...` 或 `$s/...` 订阅当作普通 MQTT 主题过滤器处理。
- 客户端也无法通过 `$queue/<name>` 或 `$stream/<name>` 形式访问该记录，因为旧版名称以 `/` 开头，而有效名称不能以 `/` 开头。

## 前提条件

- 运行 EMQX 6.1.1 或更高版本。本页中的命令使用 EMQX 6.1.1 引入的 REST API。如果您运行的是 EMQX 6.1.0，请先升级到 EMQX 6.1.1 或更高的 6.1 补丁版本。升级会保留现有的队列和消息流。
- 创建一个具有管理员角色的 API 密钥。参见[创建 API 密钥](../../guides/api.md#创建-api-密钥)。本页示例使用 `key:secret` 作为 API Key 和 Secret Key，使用 `localhost:18083` 作为 Dashboard 监听地址。请替换为您自己的值。
- 安装 [jq](https://jqlang.org/)。示例使用 jq 过滤 API 响应，并对名称进行 URL 编码。

## 查找旧版队列和消息流

EMQX 6.1.1 及更高版本会根据主题过滤器为每条旧版记录生成名称：`/<topic_filter>`。例如，主题过滤器为 `sensor/+/temp` 的旧版队列，其名称为 `/sensor/+/temp`。

通过 Dashboard、REST API 或 `$queue/`、`$stream/` 订阅创建的名称只能包含字母、数字、`_`、`-` 和 `.`，因此不会以 `/` 开头。以 `/` 开头的名称一定是旧版记录。

列出旧版队列：

```bash
queues_response=$(curl -fsS -u key:secret "http://localhost:18083/api/v5/queues?limit=1000")
printf '%s\n' "$queues_response" \
  | jq -r '.data[] | select(.name | startswith("/")) | [.name, .topic_filter, .is_lastvalue] | @tsv'
printf '%s\n' "$queues_response" | jq '.meta'
```

列出旧版消息流：

```bash
streams_response=$(curl -fsS -u key:secret "http://localhost:18083/api/v5/streams?limit=1000")
printf '%s\n' "$streams_response" \
  | jq -r '.data[] | select(.name | startswith("/")) | [.name, .topic_filter, .is_lastvalue] | @tsv'
printf '%s\n' "$streams_response" | jq '.meta'
```

每行输出依次为名称、主题过滤器，以及该记录是否使用最后值语义。例如：

```
/sensor/+/temp	sensor/+/temp	false
```

每个命令还会输出响应中的 `meta` 对象。如果所有页面都没有输出旧版记录，且最后一页的 `meta.hasnext` 为 `false`，则说明不存在该类型的旧版记录。

::: tip

当还有下一页记录时，响应字段 `meta.hasnext` 为 `true`。此时请重复相应的请求，并将 `meta` 中的 `cursor` 值作为查询参数，例如 `?limit=1000&cursor=<cursor>`。请继续请求，直到 `meta.hasnext` 为 `false`，并检查每一页是否存在旧版记录。

:::

您也可以在 Dashboard 中查看旧版记录。在左侧菜单中进入**队列**或**流**页面。旧版记录的名称以 `/` 开头。

## 选择处理方式

对于每条旧版记录，请确定是否需要保留其中存储的消息。

### 保留已存储的消息

新建的队列或消息流只接收创建之后发布的消息。EMQX 不会从旧版记录中复制已存储的消息。如需保留这些消息，请在删除旧版记录之前，让客户端消费完其中剩余的消息：

1. 创建一个与旧版记录具有相同主题过滤器和设置的命名队列或消息流。以下命令会获取旧版记录的完整配置，仅修改其名称，然后使用修改后的配置创建命名记录。

   对于旧版队列 `/sensor/+/temp`，创建名为 `sensor_temp` 的队列：

   ```bash
   curl -fsS -u key:secret \
     http://localhost:18083/api/v5/queue/%2Fsensor%2F%2B%2Ftemp \
     | jq '.name = "sensor_temp"' \
     | curl -fsS -u key:secret -X POST \
       -H "Content-Type: application/json" \
       http://localhost:18083/api/v5/queues \
       -d @-
   ```

   对于旧版消息流 `/factory/#`，创建名为 `factory` 的消息流：

   ```bash
   curl -fsS -u key:secret \
     http://localhost:18083/api/v5/stream/%2Ffactory%2F%23 \
     | jq '.name = "factory"' \
     | curl -fsS -u key:secret -X POST \
       -H "Content-Type: application/json" \
       http://localhost:18083/api/v5/streams \
       -d @-
   ```

   这些命令会保留数据保留期、容量限制、键表达式以及队列派发策略等设置。

   此后，每条新的匹配消息会同时进入旧版记录和命名记录。

2. 将消费者切换到新的订阅形式，例如使用 `$queue/sensor_temp` 代替 `$q/sensor/+/temp`，或使用 `$stream/factory` 代替 `$s/<offset>/factory/#`。同时更新授权规则，允许新的主题过滤器。订阅格式参见[消息队列](../../develop/message-queue/message-queue-concept.md)和 [MQTT 消息流](../../develop/mqtt-stream/mqtt-stream-concept.md)。

3. 至少保留一个使用旧版 `$q/` 或 `$s/` 订阅的消费者，直到它处理完所需的存储消息。超过数据保留期的消息无论如何都会被 EMQX 删除。默认数据保留期为 7 天。

4. 按照[删除旧版队列和消息流](#删除旧版队列和消息流)中的步骤删除旧版记录。

### 丢弃已存储的消息

如果不需要已存储的消息，请直接删除旧版记录。删除队列或消息流会同时删除其中存储的所有消息，且无法撤销。

删除旧版记录之前，请将仍使用 `$q/` 或 `$s/` 订阅的客户端迁移到命名队列或消息流。删除之后，这类客户端在该订阅上不会再收到消息，EMQX 也不会重新创建该旧版记录。订阅本身仍会成功，因此客户端不会收到错误。对于 `$q/` 订阅，EMQX 会记录一条 `mq_auto_create_error` 日志，其中 `reason` 为 `invalid_name`。您可以通过该日志找出仍在使用 `$q/` 前缀的客户端。

## 删除旧版队列和消息流

### 对名称进行 URL 编码

旧版名称包含 `/`，还可能包含 `+` 和 `#`。将名称放入请求路径之前，必须先进行 URL 编码。不编码时，EMQX 会返回 `404 Not Found`。此外，未经编码的 `#` 表示 URL 片段的开始，该片段不会发送到服务器。

使用 jq 对名称编码：

```bash
jq -rn --arg name '/sensor/+/temp' '$name | @uri'
```

该命令输出 `%2Fsensor%2F%2B%2Ftemp`。对于消息流名称 `/factory/#`，输出为 `%2Ffactory%2F%23`。

### 删除单条记录

删除旧版队列 `/sensor/+/temp`：

```bash
curl -fsS -u key:secret -X DELETE \
  http://localhost:18083/api/v5/queue/%2Fsensor%2F%2B%2Ftemp
```

删除旧版消息流 `/factory/#`：

```bash
curl -fsS -u key:secret -X DELETE \
  http://localhost:18083/api/v5/stream/%2Ffactory%2F%23
```

请求成功时返回 HTTP 状态码 `204`，没有响应体。记录不存在时，EMQX 返回 `404`，错误码为 `NOT_FOUND`。

### 删除全部旧版记录

删除所有旧版队列：

```bash
queues_response=$(curl -fsS -u key:secret "http://localhost:18083/api/v5/queues?limit=1000")
printf '%s\n' "$queues_response" | jq '.meta'
for name in $(printf '%s\n' "$queues_response" \
  | jq -r '.data[] | select(.name | startswith("/")) | .name | @uri'); do
  curl -fsS -u key:secret -X DELETE "http://localhost:18083/api/v5/queue/$name" -w "$name %{http_code}\n"
done
```

删除所有旧版消息流：

```bash
streams_response=$(curl -fsS -u key:secret "http://localhost:18083/api/v5/streams?limit=1000")
printf '%s\n' "$streams_response" | jq '.meta'
for name in $(printf '%s\n' "$streams_response" \
  | jq -r '.data[] | select(.name | startswith("/")) | .name | @uri'); do
  curl -fsS -u key:secret -X DELETE "http://localhost:18083/api/v5/stream/$name" -w "$name %{http_code}\n"
done
```

每个代码块会删除一个 API 分页中返回的旧版记录，并输出该页的 `meta` 对象。如果 `meta.hasnext` 为 `true`，请保存其中的 `cursor` 值，在请求 URL 中添加 `&cursor=<cursor>` 后重复运行该代码块，直到 `meta.hasnext` 为 `false`。每条删除操作的输出为编码后的名称和 HTTP 状态码。请确认所有状态码均为 `204`。

### 通过 Dashboard 删除

1. 在左侧菜单中进入**队列**或**流**页面。
2. 找到名称以 `/` 开头的记录。
3. 点击**操作**栏中的**删除**，然后点击**确认**。

### 验证

再次运行[查找旧版队列和消息流](#查找旧版队列和消息流)中的命令，并检查所有分页。只有当所有页面都没有输出旧版记录，且队列和消息流最后一页响应中的 `meta.hasnext` 均为 `false` 时，才能确认集群中已没有旧版记录。此时可以继续升级到 EMQX 7.0。
