# 连接抖动配置

连接抖动检测用于识别短时间内过多的连接尝试。从 EMQX 6.3.0 开始，您可以分别为客户端 ID、用户名和源 IP 地址配置独立的检测策略。

## 检测维度

`flapping_detect` 配置包含以下维度。每个维度默认关闭。

| 配置项 | 检测键 | 行为 |
| --- | --- | --- |
| `by_clientid` | 客户端 ID | 统计使用同一客户端 ID 的连接尝试。 |
| `by_username` | 用户名 | 统计使用同一用户名的客户端连接尝试。未提供用户名的连接不计入此维度。 |
| `by_peerhost` | 源 IP 地址 | 统计来自同一源 IP 地址的连接尝试。 |

为维度配置检测参数即可启用该维度；将其设置为 `none` 可关闭。您可以启用 3 个维度的任意组合，每个已启用的维度均独立计数。

每个维度支持以下检测参数：

| 字段 | 说明 | 默认值 |
| --- | --- | --- |
| `window_time` | EMQX 统计连接尝试的时间窗口。 | `1m` |
| `max_count` | 在 `window_time` 内触发封禁的连接尝试次数。 | `15` |
| `ban_time` | 封禁匹配的客户端 ID、用户名或源 IP 地址的时长。 | `5m` |

当客户端 ID、用户名或源 IP 地址达到阈值时，EMQX 会为对应的标识或地址创建临时封禁记录。后续匹配的新连接会在认证前被拒绝，已建立的连接不会断开。封禁记录会自动过期，也可以通过 `/banned` REST API 查看或提前删除。

检测计数器由各节点独立维护，EMQX 不会合并由不同节点处理的连接尝试。某个节点检测到连接抖动后，生成的封禁记录会复制到整个集群。

## 配置示例

以下 HOCON 示例为客户端 ID 和源 IP 地址配置不同的策略，并关闭按用户名检测：

```hocon
flapping_detect {
  by_clientid {
    window_time = 1m
    max_count = 15
    ban_time = 5m
  }

  by_username = none

  by_peerhost {
    window_time = 30s
    max_count = 100
    ban_time = 10m
  }
}
```

您也可以为各个 Zone 单独配置这些设置。Zone 中的维度策略未指定部分字段时，会继承对应全局维度策略中的字段。

## 与 EMQX 6.3.0 之前配置的兼容性

从 EMQX 6.3.0 开始，以下扁平字段已弃用：

- `flapping_detect.enable`
- `flapping_detect.window_time`
- `flapping_detect.max_count`
- `flapping_detect.ban_time`

EMQX 继续接受这些字段以保持向后兼容。当 `enable = true` 时，EMQX 将扁平策略字段映射到 `by_clientid`。当 `enable = false`，或配置中包含扁平策略字段但未显式设置 `enable = true` 时，EMQX 将 `by_clientid` 设置为 `none`。

已弃用的字段仅影响按客户端 ID 检测。`by_username` 和 `by_peerhost` 仍为 `none`，需要显式配置才能启用。

如果配置同时包含 `by_clientid` 和已弃用的扁平字段，`by_clientid` 优先生效，包括将其设置为 `none` 的情况。

EMQX 提供了更多配置项以更好地满足定制化需求。详情请参见 [EMQX 企业版配置手册](https://docs.emqx.com/zh/enterprise/v@EE_VERSION@/hocon/)。

您也可以在 EMQX Dashboard 中点击**访问控制** -> **连接抖动**配置连接抖动检测。有关 Dashboard 操作步骤，参见[连接抖动检测](../access-control/flapping-detect.md)。
