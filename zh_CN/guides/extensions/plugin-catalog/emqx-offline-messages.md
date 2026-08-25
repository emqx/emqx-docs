# 离线消息

该插件将 MQTT 消息持久化到外部数据库，使订阅者在重新上线后仍可获取发布期间未到达的消息。

适用于标准 MQTT 会话持久化无法满足需求的场景，例如：消息保留时长需超过会话生命周期，或其他系统需要读取消息历史。

支持的后端：

- **MySQL**：使用两张表（`mqtt_msg`、`mqtt_sub`），SQL 语句可配置。
- **Redis**：使用 Hash 存储订阅与消息，按主题维度的 Sorted Set 用作索引。支持单实例、Sentinel、Cluster 三种部署模式。

同一时间只能启用一个后端，按需开启 `mysql.enable` 或 `redis.enable`。

## 插件配置

插件自带默认 `config.hocon`，覆盖两种后端的所有字段。Dashboard 的插件详情页基于同一份 schema 渲染表单，多数运维场景下直接在 UI 中完成配置即可，无需手工编辑文件。

常用顶层字段：

- `topics`：插件需要跟踪的主题过滤器列表。留空表示该后端不生效。
- `pool_size`：到后端的连接池大小。
- `batch_size` / `batch_time`：写入批量参数（设为 `1` / `0` 关闭批量）。
- `ssl.*`：后端连接的 TLS 配置。

### MySQL 字段

- `server`：`host:port`。
- `username`、`password`、`database`：账号信息。
- `init_default_schema`：为 `true` 时插件启动会自动创建默认的 `mqtt_msg` / `mqtt_sub` 表；如自行管理 schema，保持 `false`。
- `insert_message_sql`、`delete_message_sql`、`select_message_sql`、`insert_subscription_sql`、`select_subscriptions_sql`、`delete_subscription_sql`：可覆盖的 SQL 模板，支持 `${var}` 占位符，可引用消息字段（`id`、`from`、`topic`、`qos`、`payload`、`flags.retain`、`timestamp`）以及订阅字段（`clientid`、`topic`、`qos`）。

### Redis 字段

- `servers`：以逗号分隔的 `host:port` 列表。Sentinel 模式下需设置 `redis_type = "sentinel"` 并将 `sentinel` 填为主节点名。
- `redis_type`：`single` / `sentinel` / `cluster` 之一。
- `username`、`password`、`database`：账号与逻辑 DB（Cluster 模式忽略 `database`）。
- `message_key_prefix`（默认 `mqtt:msg`）、`subscription_key_prefix`（默认 `mqtt:sub`）：键名前缀。
- `message_ttl`：单条消息 TTL（秒），超过后会从按主题的 Sorted Set 索引中清理。

## 数据库 Schema

### MySQL

```sql
CREATE TABLE IF NOT EXISTS `mqtt_msg` (
  `id` bigint unsigned NOT NULL AUTO_INCREMENT,
  `msgid` varchar(64) DEFAULT NULL,
  `topic` varchar(180) NOT NULL,
  `sender` varchar(64) DEFAULT NULL,
  `qos` tinyint(1) NOT NULL DEFAULT '0',
  `retain` tinyint(1) DEFAULT NULL,
  `payload` blob,
  `arrived` datetime NOT NULL,
  PRIMARY KEY (`id`),
  INDEX topic_index(`topic`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8MB4;

CREATE TABLE IF NOT EXISTS `mqtt_sub` (
  `clientid` varchar(64) NOT NULL,
  `topic` varchar(180) NOT NULL,
  `qos` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`clientid`, `topic`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8MB4;
```

可通过 `init_default_schema = true` 让插件首次启动时自动建表；否则请在启用该后端前自行创建。

### Redis

Redis 中的结构按需创建，无需迁移步骤：

- `mqtt:sub:{clientid}`：Hash，`{topic} -> {qos}`。
- `mqtt:msg:{msgid}`：Hash，字段 `id`、`from`、`qos`、`topic`、`payload`、`ts`、`retain`。`msgid` 使用 base62 编码。
- `mqtt:msg:{topic}`：Sorted Set，成员为 base62 消息 ID，分值为时间戳，用于过期清理。

若 Redis 开启了 ACL，请确保连接用户拥有 `HSET`、`HDEL`、`HGETALL`、`HMSET`、`DEL`、`EXPIRE`、`ZADD`、`ZRANGE`、`ZREMRANGEBYSCORE`、`ZREM` 在 `mqtt:sub:*` 与 `mqtt:msg:*` 键上的权限。

## 安装

从下方 [下载](#下载) 区获取对应 EMQX 版本的插件包，然后通过 Dashboard、REST API 或 CLI 安装。

使用 REST API：

```bash
curl -u key:secret -X POST http://localhost:18083/api/v5/plugins/install \
  -H "Content-Type: multipart/form-data" \
  -F "plugin=@emqx_offline_messages-<version>.tar.gz"
```

随后启动插件（Dashboard 或 `emqx ctl plugins start emqx_offline_messages-<version>`），并在 Dashboard 中打开插件详情页完成后端配置。

## 验证

在没有订阅者的情况下，向插件跟踪的主题发布若干消息：

```bash
mqttx pub -q 1 -t 't/2' -m 'hello-from-offline1'
mqttx pub -q 1 -t 't/2' -m 'hello-from-offline2'
mqttx pub -q 1 -t 't/2' -m 'hello-from-offline3'
```

随后使用新的客户端 ID 订阅，应能收到插件持久化的消息回放。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## 下载

各 EMQX 版本对应的插件安装包：

| EMQX 版本 | 插件版本 | 安装包 |
|---|---|---|
| 5.10.4 | 2.0.0 | [emqx_offline_messages-2.0.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/e5.10.4/emqx_offline_messages-2.0.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/e5.10.4/emqx_offline_messages-2.0.0.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
