# 离线消息

该插件会在没有订阅者在线时持久化选定的 QoS 1/2 消息，并在之后有匹配的订阅者上线时将其重放。

支持的后端：

- MySQL
- Redis

## 配置

该插件通过标准的 EMQX 插件配置 API 进行配置：

`PUT /api/v5/plugins/<name-vsn>/config`

Schema 定义在 `priv/config.hocon` 中，包含针对 Redis 和 MySQL 的后端专属设置。

该插件仅在以下情况下持久化消息：

- 发布的 QoS 大于 `0`
- 主题匹配某个已配置的 `message.topic_filter`

## 构建与测试

在仓库根目录构建该插件：

```bash
make
```

运行该插件的 Common Test 测试套件：

```bash
make plugins/emqx_offline_messages-ct
```

## 数据库表结构

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
)
ENGINE=InnoDB DEFAULT CHARSET=utf8MB4;
```

```sql
CREATE TABLE IF NOT EXISTS `mqtt_sub` (
  `clientid` varchar(64) NOT NULL,
  `topic` varchar(180) NOT NULL,
  `qos` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`clientid`, `topic`)
)
ENGINE=InnoDB DEFAULT CHARSET=utf8MB4;
```

### Redis

Redis 在 `mqtt:sub:*` 和 `mqtt:msg:*` 键空间下使用哈希（hash）和有序集合（sorted set）。

- 订阅状态存储在以客户端 ID 为键的哈希中。
- 消息负载存储在以消息 ID 为键的哈希中。
- 主题索引存储在以主题为键的有序集合中。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## 下载

各 EMQX 版本对应的插件安装包：

| EMQX 版本 | 插件版本 | 安装包 |
|---|---|---|
| 6.1.2 | 2.0.0 | [emqx_offline_messages-2.0.0.tar.gz](https://packages.emqx.io/emqx-plugins/6.1.2/emqx_offline_messages-2.0.0.tar.gz) |
| 6.1.3 | 2.0.0 | [emqx_offline_messages-2.0.0.tar.gz](https://packages.emqx.io/emqx-plugins/6.1.3/emqx_offline_messages-2.0.0.tar.gz) |
| 6.1.4 | 2.0.0 | [emqx_offline_messages-2.0.0.tar.gz](https://packages.emqx.io/emqx-plugins/6.1.4/emqx_offline_messages-2.0.0.tar.gz) |

<!-- PLUGIN-DOWNLOADS:END -->
