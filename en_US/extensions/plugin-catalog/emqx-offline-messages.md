# Offline Messages

This plugin persists MQTT messages to an external database so that subscribers can retrieve them after coming back online, even when they were disconnected at publish time.

It is useful for scenarios where standard MQTT session persistence is not enough — for example when retention has to outlive a session, or when other systems also need to read the message history.

Supported backends:

- **MySQL** — uses two tables (`mqtt_msg`, `mqtt_sub`) with configurable SQL statements.
- **Redis** — uses hashes for subscriptions/messages and a sorted set as a per-topic index. Supports single, sentinel, and cluster deployments.

Only one backend is active at a time; enable either `mysql.enable` or `redis.enable`.

## Plugin Configuration

The plugin ships with a default `config.hocon` covering both backends. The Dashboard plugin detail page renders the same schema, so most operators configure it from the UI rather than editing the file directly.

Common top-level fields:

- `topics` — list of topic filters the plugin should track. An empty list means the plugin is inactive for that backend.
- `pool_size` — connection pool size to the backend.
- `batch_size` / `batch_time` — write batching parameters (set both to `1`/`0` to disable batching).
- `ssl.*` — TLS settings for the backend connection.

### MySQL Specific

- `server` — `host:port`.
- `username`, `password`, `database` — credentials.
- `init_default_schema` — when `true`, the plugin creates the default `mqtt_msg` / `mqtt_sub` tables on startup. Leave `false` if you manage the schema yourself.
- `insert_message_sql`, `delete_message_sql`, `select_message_sql`, `insert_subscription_sql`, `select_subscriptions_sql`, `delete_subscription_sql` — overridable SQL templates with `${var}` placeholders bound to MQTT message fields (`id`, `from`, `topic`, `qos`, `payload`, `flags.retain`, `timestamp`) and subscription fields (`clientid`, `topic`, `qos`).

### Redis Specific

- `servers` — comma-separated `host:port` list. For sentinel mode, set `redis_type = "sentinel"` and `sentinel` to the master name.
- `redis_type` — one of `single`, `sentinel`, `cluster`.
- `username`, `password`, `database` — credentials and logical DB (cluster mode ignores `database`).
- `message_key_prefix` (default `mqtt:msg`), `subscription_key_prefix` (default `mqtt:sub`) — key namespaces.
- `message_ttl` — per-message TTL in seconds; messages older than the TTL are cleaned up from the per-topic sorted-set index.

## Database Schema

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

Set `init_default_schema = true` to let the plugin create these tables on first start; otherwise create them yourself before enabling the backend.

### Redis

Redis structures are created on demand — no schema migration step is needed.

- `mqtt:sub:{clientid}` — hash, `{topic} -> {qos}`.
- `mqtt:msg:{msgid}` — hash, fields `id`, `from`, `qos`, `topic`, `payload`, `ts`, `retain`. `msgid` is base62-encoded.
- `mqtt:msg:{topic}` — sorted set, members are base62 message IDs, scores are timestamps. Used for expiration cleanup.

If Redis ACLs are in use, grant the connecting user `HSET`, `HDEL`, `HGETALL`, `HMSET`, `DEL`, `EXPIRE`, `ZADD`, `ZRANGE`, `ZREMRANGEBYSCORE`, `ZREM` on keys matching `mqtt:sub:*` and `mqtt:msg:*`.

## Installation

Download the tarball for your EMQX version from the [Download](#download) section below, then install it via the Dashboard, REST API, or CLI.

Using the REST API:

```bash
curl -u key:secret -X POST http://localhost:18083/api/v5/plugins/install \
  -H "Content-Type: multipart/form-data" \
  -F "plugin=@emqx_offline_messages-<version>.tar.gz"
```

Start the plugin (Dashboard, or `emqx ctl plugins start emqx_offline_messages-<version>`), then open the plugin detail page in the Dashboard to configure the backend.

## Verifying

Publish a few messages to a topic the plugin tracks while no client is subscribed:

```bash
mqttx pub -q 1 -t 't/2' -m 'hello-from-offline1'
mqttx pub -q 1 -t 't/2' -m 'hello-from-offline2'
mqttx pub -q 1 -t 't/2' -m 'hello-from-offline3'
```

Subscribe afterwards with a fresh client ID — messages stored by the plugin are replayed.

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## Download

Tarballs for each EMQX release:

| EMQX Version | Plugin Version | Package |
|---|---|---|
| 5.10.4 | 2.0.0 | [emqx_offline_messages-2.0.0.tar.gz](https://packages.emqx.io/emqx-plugins/e5.10.4/emqx_offline_messages-2.0.0.tar.gz) |

<!-- PLUGIN-DOWNLOADS:END -->
