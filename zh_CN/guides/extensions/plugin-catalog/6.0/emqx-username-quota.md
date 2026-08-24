# 按用户名的会话配额

该插件用于强制执行按用户名的会话配额。

- 会话计数按用户名维护，并在集群范围内同步。
- 当达到配置的配额时，认证会以 `quota_exceeded` 被拒绝。
- 使用已存在的 `clientid` 重连不会占用额外的配额。
- 按用户名的配额覆盖（overrides）支持自定义上限、无限会话或阻断连接。

:::: tip 提示
如果部署中可以通过在 `client_attrs_init` 配置中设置 `client_attrs.tns`，将用户名作为命名空间，则可以通过基于命名空间的会话数量限制实现这类控制。
仅当命名空间分配采用其他方案时，才需要使用此插件。
::::

## 配置

| 字段 | 默认值 | 校验规则 | 说明 |
|------|--------|----------|------|
| `max_sessions_per_username` | `100` | 必须为正整数（`>= 1`）。小于 `1` 或非数字的值会被拒绝。 | 每个用户名默认的最大并发会话数。单个用户名可通过 overrides API 覆盖此值。 |
| `snapshot_min_age_ms` | `300000` | 必须在 `120000` 至 `900000` 范围内。超出范围的值会被截断到边界。 | 触发重建前快照的最小存活时间，单位为毫秒。用于避免在大型集群上频繁重建。 |
| `snapshot_request_timeout_ms` | `5000` | 字符串值如可转换为正整数，也会被接受。 | 列表 API 快照请求处理的超时预算。 |

通过标准的插件配置 API 更新插件配置：

`PUT /api/v5/plugins/<name-vsn>/config`

## 运行时 API

该插件通过插件 API 网关暴露运行时 API。

基础路径：`/api/v5/plugin_api/emqx_username_quota`

### 会话查询

- `GET /quota/usernames`：列出所有有活跃会话的用户名。
- `GET /quota/usernames/:username`：获取单个用户名的详情。
- `GET /metrics`：以 Prometheus 文本格式导出插件指标。
- `POST /kick/:username`：踢除某个用户名的所有会话。

### 快照管理

- `DELETE /quota/snapshot`：强制重建快照。

### 配额覆盖

- `POST /quota/overrides`：设置按用户名的配额覆盖。
- `DELETE /quota/overrides`：删除按用户名的配额覆盖。
- `GET /quota/overrides`：列出所有配额覆盖。

### `GET /quota/usernames`

该接口从预先构建的快照返回结果，而不是在每次请求时扫描实时会话数据。

快照是按用户名会话计数的某一时刻副本，按计数排序以支持高效的基于游标的分页。快照在后台异步构建并缓存；仅当当前快照存活时间超过 `snapshot_min_age_ms` 时，才会触发新的构建。

当第一个请求到达且尚无快照时，服务器会等待正在进行的构建完成，最长等待时间为请求截止时间减 1 秒。如果构建及时完成，接口返回正常的 `200` 响应；否则返回带有部分数据的 `503`。

查询参数：

- `limit`：正整数，上限为 `100`（默认 `100`）
- `used_gte`：无游标时**必填**。表示最小会话计数过滤条件，仅包含会话数不少于该值的用户名。必须为正整数（`>= 1`）。
- `cursor`：可选的不透明游标，由上一次列表调用返回。若缺省，则返回第一页。

参数规则：

- 有 `used_gte` 无 `cursor`：OK（第一页）
- 有 `cursor` 无 `used_gte`：OK（`used_gte` 已内嵌在游标中）
- 同时有 `used_gte` 和 `cursor`：**400** `BAD_REQUEST`。过滤条件已锁定在游标中。
- 既无 `used_gte` 也无 `cursor`：**400** `BAD_REQUEST`

行为：

- 结果始终按会话计数再按用户名排序。
- 分页基于游标。第一页请求省略 `cursor`。
- 每一项包含 `username`、实时的 `used` 以及 `limit`（有效配额）。
- 如果实时 `used` 与快照计数不同，则包含 `snapshot_used`，使调用方能够同时看到缓存值和当前值。

成功响应结构：

- `data`：用户名配额条目
- `meta.limit`：页大小（分页上限）
- `meta.count`：本页条目数
- `meta.total`：快照中的总条目数
- `meta.next_cursor`：下一页的游标（在可用时）
- `meta.snapshot`：快照元数据：
  - `node`
  - `generation`（递增的快照 id）
  - `taken_at_ms`（快照时间戳，毫秒）

错误响应：

- `400 BAD_REQUEST`：缺少 `used_gte`，或在带游标时提供了 `used_gte`
- `400 INVALID_CURSOR`：游标引用了不可用的节点或格式错误
- `503 SERVICE_UNAVAILABLE`：快照正在重建
  - 响应体包含 `snapshot_build_in_progress: true`、`data` 和 `meta`
  - `data`：从正在构建的快照中读取的部分第一页（如果构建刚开始可能为空）
  - `meta.count`：部分条目数，`meta.partial: true`
  - 使用有界退避重试同一请求

### `DELETE /quota/snapshot`

强制立即重建快照。在异步发起重建后返回 `200` 及 `{"status": "ok"}`。快照将在后台重建。

### `GET /quota/usernames/:username`

返回单个用户名的详情。响应字段：`username`、`used`、`limit`、`clientids`。

如果该用户名没有活跃会话，返回 `404 NOT_FOUND`。

### `GET /metrics`

以 Prometheus 文本格式返回插件指标。
在 replicant 节点上，请求会被转发到快照所属的 core 节点。

当前导出：

- `emqx_username_count`：当前活跃快照中的用户名总数。

### `POST /kick/:username`

踢除某个用户名的所有会话。返回 `{"kicked": N}`，其中 N 为被踢除的会话数。

如果该用户名没有活跃会话，返回 `404 NOT_FOUND`。

### `POST /quota/overrides`

设置按用户名的配额覆盖。请求体为 JSON 数组：

```json
[
  {"username": "user1", "quota": 1000},
  {"username": "vip", "quota": "nolimit"},
  {"username": "blocked", "quota": 0}
]
```

覆盖语义：

| `quota` 取值     | 含义                                   |
|------------------|----------------------------------------|
| 正整数           | 该用户名的自定义会话上限               |
| `"nolimit"`      | 无限会话（不做配额限制）               |
| `0`              | 封禁：拒绝所有新连接                   |

覆盖会持久化到磁盘并在集群范围内复制。当某个用户名没有覆盖时，使用全局配置
`max_sessions_per_username`。

### `DELETE /quota/overrides`

按用户名删除覆盖。请求体为用户名字符串的 JSON 数组：

```json
["user1", "blocked"]
```

### `GET /quota/overrides`

列出所有覆盖。返回 `{"data": [{"username": "...", "quota": ...}, ...]}`。

## 基于快照的列表查询机制

本节说明插件如何为 `GET /quota/usernames` 和 `GET /metrics` 等列表类 API 构建并提供快照数据。

### 快照所属节点路由

快照在 core 节点上构建。`GET /quota/usernames` 和 `GET /metrics` 会被路由到快照所属的 core 节点，该节点选取为已排序的运行中 core 节点列表中的第一个。

### 蓝绿快照

维护两个快照缓冲区（蓝和绿）。当其中一个用于服务读取请求时，另一个用于构建下一个快照。一旦构建完成，两者角色互换。旧快照会一直可用，直到新快照就绪，从而避免重建期间出现数据空档。

### 后台快照构建

快照重建在后台进程中运行，采用基于让出（yield）的节流以避免阻塞服务器。构建进行期间列表 API 仍保持响应。

## 运维注意事项与限制

本节说明在生产环境中运行该插件时需要关注的运行时行为与限制。

### 连接突发下的配额超额

配额决策在认证阶段做出，而会话计数在会话生命周期钩子中最终确定。在高并发连接突发（尤其是在集群中）时，这会造成一个短暂的同步窗口，期间某个用户名被观察到的并发会话数可能暂时超过 `max_sessions_per_username`。

实际影响：

- 该插件在突发负载下以最终一致性提供集群范围的配额强制。
- 在极端连接扇入场景下，该插件不保证在任意时刻都能严格逐连接执行配额限制。

### 插件启动时的引导行为

当插件被安装到运行中的集群时，已有的客户端会话是在钩子注册之前建立的。启动时，插件通过遍历所有本地 channel 并注册每个会话来引导配额状态。

为避免以大量 DB 写操作压垮 Core 节点（尤其当 replicant 节点已有大量现有连接时），引导循环会被节流：

- 会话以每批 100 个进行注册。
- 每批之后，引导会等待最后写入的记录被复制回本地表后再继续。它每 10ms 轮询一次。
- 如果复制在 10 秒内未完成，会记录一条错误，并以 `error` 级别日志中止引导。
  超时前已注册的会话会被保留；其余会话将在重连时通过后续基于钩子的注册自然被纳入。

### 处理列表 API 返回的 `503` 响应

当服务器繁忙或正在构建快照时，列表 API 返回 `503`。

`503` 响应体包含一个 `data` 数组，其中是从正在构建的快照表中读取的部分第一页。这为调用方立即提供尽力而为的数据，而不是空响应。`meta.partial: true` 标志表明数据不完整。如果构建刚开始，部分页可能为空。

API 客户端指南：

- 检查 `data` 中立即可用的任何部分结果。
- 使用有界退避重试。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## 下载

各 EMQX 版本对应的插件安装包：

| EMQX 版本 | 插件版本 | 安装包 |
|---|---|---|
| 6.0.3 | 1.2.1 | [emqx_username_quota-1.2.1.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.0.3/emqx_username_quota-1.2.1.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.0.3/emqx_username_quota-1.2.1.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
