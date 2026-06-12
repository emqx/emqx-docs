# Per-username Session Quota

该插件用于按用户名实施会话配额限制。

- 会话计数按用户名维护，并在集群范围内同步。
- 当达到配置的配额时，认证会以 `quota_exceeded` 被拒绝。
- 使用已有 `clientid` 重连不会额外消耗配额。
- 支持按用户名配置配额覆盖，可实现自定义限制、无限会话或禁止连接。

> [!NOTE]
> 通过将用户名设置为命名空间（在 `client_attrs_init` 配置中设置 `client_attrs.tns`）也可以实现按用户名限制会话数。
> 只有在命名空间方案不是这样设计时，才需要使用本插件。

## 配置

插件配置项：

- `max_sessions_per_username`（默认：`100`）— 必须是正整数（>= 1）。
- `snapshot_min_age_ms`（默认：`300000`，范围：`120000`–`900000`）— 快照在允许重建前的最小存活时间。超出范围的值会被夹紧。
- `snapshot_request_timeout_ms`（默认：`5000`）

配置语义：

- `max_sessions_per_username`：每个用户名默认允许的最大并发会话数。单个用户名可通过 overrides API 覆盖该值。
- `snapshot_min_age_ms`：触发快照重建前，快照必须达到的最小存活时间（毫秒）。用于防止在大集群中频繁重建。
- `snapshot_request_timeout_ms`：列表 API 在处理快照请求时的超时预算。

校验规则：

- `max_sessions_per_username` 必须 >= 1。小于 1 或非数字的值会被拒绝。
- 对于数值字段，如果字符串可以转换为正整数，也会被接受。

通过标准插件配置 API 更新插件配置：

`PUT /api/v5/plugins/<name-vsn>/config`

## 运行时 API

该插件通过 plugin API gateway 暴露运行时 API。

基础路径：`/api/v5/plugin_api/emqx_username_quota`

**Snapshot**：`GET /quota/usernames` 返回的是预构建快照结果，而不是在每次请求时扫描实时会话数据。快照是按用户名会话计数生成的时间点副本，并按计数排序，以便高效支持基于游标的分页。快照在后台异步构建并缓存；只有当当前快照年龄超过 `snapshot_min_age_ms` 时才会触发新一轮构建。若实时计数与快照值发生偏移，响应项中会包含 `snapshot_used` 字段，便于调用方同时查看缓存值和当前值。

**首个请求等待**：如果首个请求到达时系统尚无任何快照，服务会等待正在进行的快照构建完成（最长等待时间为请求截止时间减去 1 秒）。若构建及时完成，则返回正常 `200` 响应；否则返回 `503`，并附带部分数据（见下文）。

### 会话查询

- `GET /quota/usernames` — 列出所有存在活跃会话的用户名
- `GET /quota/usernames/:username` — 获取指定用户名详情
- `GET /metrics` — 以 Prometheus 文本格式导出插件指标
- `POST /kick/:username` — 踢掉指定用户名的所有会话

### Snapshot 管理

- `DELETE /quota/snapshot` — 强制重建快照

### 配额覆盖

- `POST /quota/overrides` — 设置按用户名的配额覆盖
- `DELETE /quota/overrides` — 删除按用户名的配额覆盖
- `GET /quota/overrides` — 列出所有配额覆盖

### `GET /quota/usernames`

查询参数：

- `limit`：正整数，最大值 `100`（默认 `100`）
- `used_gte`：**必填**（未提供 `cursor` 时）— 会话数下限过滤条件。仅返回至少达到该会话数的用户名。必须是 >= 1 的正整数。
- `cursor`：可选，为上一次列表调用返回的不透明游标；若省略则返回第一页。

参数规则：

- 仅提供 `used_gte`：可以（第一页）
- 仅提供 `cursor`：可以（`used_gte` 已编码进游标）
- 同时提供 `used_gte` 和 `cursor`：返回 **400** `BAD_REQUEST` — 过滤条件已锁定在游标中
- 两者都不提供：返回 **400** `BAD_REQUEST`

行为说明：

- 结果始终按会话数、再按用户名排序。
- 分页基于游标。第一页不传 `cursor`。
- 每条记录都包含 `username`、实时 `used` 与 `limit`（实际生效配额）。
- 如果实时 `used` 与快照计数不同，则会返回 `snapshot_used`。

成功响应结构：

- `data`：用户名配额条目
- `meta.limit`：页大小（分页限制）
- `meta.count`：当前页条目数
- `meta.total`：快照中的总条目数
- `meta.next_cursor`：下一页游标（如有）
- `meta.snapshot`：快照元数据：
  - `node`
  - `generation`（递增快照 ID）
  - `taken_at_ms`（毫秒级快照时间戳）

错误响应：

- `400 BAD_REQUEST`：缺少 `used_gte`，或同时提供了 `used_gte` 和 `cursor`
- `400 INVALID_CURSOR`：游标引用了不可用节点或格式错误
- `503 SERVICE_UNAVAILABLE`：快照正在重建
  - 响应体包含 `snapshot_build_in_progress: true`、`data` 和 `meta`
  - `data`：从构建中的快照读取的部分第一页结果（若构建刚开始，可能为空）
  - `meta.count`：部分条目数，`meta.partial: true`
  - 请使用有界退避重试相同请求

### `DELETE /quota/snapshot`

强制立即重建快照。该接口在异步启动重建后返回 `200` 与 `{"status": "ok"}`。快照会在后台完成重建。

### `GET /quota/usernames/:username`

返回指定用户名的详情。响应字段包括：`username`、`used`、`limit`、`clientids`。

若该用户名没有活跃会话，则返回 `404 NOT_FOUND`。

### `GET /metrics`

以 Prometheus 文本格式返回插件指标。
在 replicant 节点上，请求会被转发到 snapshot owner 所在的 core 节点。

当前导出的指标：

- `emqx_username_count` — 当前快照中的用户名总数

### `POST /kick/:username`

踢掉指定用户名的所有会话。返回 `{"kicked": N}`，其中 `N` 是被踢掉的会话数。

若该用户名没有活跃会话，则返回 `404 NOT_FOUND`。

### `POST /quota/overrides`

设置按用户名的配额覆盖。请求体是一个 JSON 数组：

```json
[
  {"username": "user1", "quota": 1000},
  {"username": "vip", "quota": "nolimit"},
  {"username": "blocked", "quota": 0}
]
```

覆盖语义：

| `quota` 值       | 含义 |
|------------------|------|
| 正整数           | 该用户名的自定义会话上限 |
| `"nolimit"`      | 无限会话（不做配额控制） |
| `0`              | 封禁 — 拒绝所有新连接 |

覆盖值会持久化到磁盘，并在集群范围内复制。当某个用户名没有覆盖配置时，使用全局 `max_sessions_per_username`。

### `DELETE /quota/overrides`

按用户名删除覆盖配置。请求体是用户名字符串数组：

```json
["user1", "blocked"]
```

### `GET /quota/overrides`

列出所有覆盖配置。返回 `{"data": [{"username": "...", "quota": ...}, ...]}`。

## 架构

### Snapshot owner 路由

快照构建发生在 core 节点上。`GET /quota/usernames` 与 `GET /metrics` 会被路由到 snapshot owner 所在的 core 节点，该节点定义为当前运行中的 core 节点列表按排序后的第一个。

### 蓝绿快照

系统维护两套快照缓冲区（blue 和 green）。其中一套负责服务读取请求，另一套用于构建下一版快照。构建完成后，二者角色交换。这样在重建期间不会出现数据空窗期 —— 旧快照会一直可用，直到新快照准备完成。

### 后台快照构建

快照重建在后台进程中运行，并通过 yield 节流避免阻塞服务器。因此即使快照构建进行中，列表 API 仍然保持可响应。

## 运维说明

### 突发连接下的配额超限

配额判定发生在认证阶段，而会话计数最终落地发生在会话生命周期 Hook 阶段。
在高并发连接突发场景下（尤其是集群环境中），这会产生一个短暂的同步窗口，使得某个用户名的观测并发会话数可能暂时超过 `max_sessions_per_username`。

实际含义：

- 本插件在集群范围内提供的是“最终一致”的配额控制，能够应对突发负载。
- 它并不是在极端连接洪峰下的严格逐包准入闸门。

### 插件启动时的引导

当插件安装到一个正在运行的集群时，已有客户端会话是在 Hook 注册之前建立的。
插件启动时，会遍历所有本地 channel，将每个会话注册进配额状态，以完成初始化引导。

为了避免对 Core 节点造成大量 DB 写入风暴（特别是当 replicant 节点上已有大量连接时），引导循环做了节流：

- 每批注册 100 个会话。
- 每处理完一批，引导过程会等待最后一条写入记录复制回本地表之后再继续。轮询间隔为 10ms。
- 如果 10 秒内复制仍未完成，则记录一条 `error` 级日志并中止引导。
  在超时前已经注册的会话会被保留；剩余会话将在后续客户端重连触发 Hook 注册时自然补齐。

### 处理列表 API 返回的 `503`

当服务器繁忙或正在构建快照时，列表 API 会返回 `503`。

`503` 响应体中包含一个 `data` 数组，其内容是从构建中的快照表读取到的部分第一页结果。这样调用方可以立刻获得尽力而为的数据，而不是空响应。`meta.partial: true` 表示该数据并不完整。如果构建刚刚开始，这个部分页面也可能为空。

对 API 客户端的建议：

- 先检查 `data` 中是否已有部分可用结果。
- 使用有界退避策略进行重试。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## 下载

各 EMQX 版本对应的插件安装包：

| EMQX 版本 | 插件版本 | 安装包 |
|---|---|---|
| 6.2.0 | 1.2.0 | [emqx_username_quota-1.2.0.tar.gz](https://packages.emqx.io/emqx-plugins/6.2.0/emqx_username_quota-1.2.0.tar.gz) |
| 6.2.1 | 1.2.1 | [emqx_username_quota-1.2.1.tar.gz](https://packages.emqx.io/emqx-plugins/6.2.1/emqx_username_quota-1.2.1.tar.gz) |

<!-- PLUGIN-DOWNLOADS:END -->
