# オフラインメッセージ

このプラグインは MQTT メッセージを外部データベースに永続化し、サブスクライバーが再接続後にメッセージを取得できるようにします。パブリッシュ時に切断されていてもメッセージを取り逃がしません。

標準の MQTT セッション永続化では不十分な場合、例えばセッションを超えてメッセージを保持したい場合や、他のシステムでもメッセージ履歴を参照する必要がある場合に有用です。

対応バックエンド：

- **MySQL**：2つのテーブル（`mqtt_msg`、`mqtt_sub`）を使用し、SQL文は設定可能です。
- **Redis**：サブスクリプション／メッセージはハッシュ、トピックごとのインデックスはソート済みセットで管理します。シングル、センチネル、クラスターの各デプロイに対応しています。

同時に有効化できるバックエンドは1つのみで、`mysql.enable` または `redis.enable` のいずれかを有効にしてください。

## プラグイン設定

プラグインには両バックエンド対応のデフォルト `config.hocon` が付属しています。ダッシュボードのプラグイン詳細ページでも同じスキーマが表示されるため、多くの運用者はファイルを直接編集せずUIから設定します。

共通のトップレベル項目：

- `topics`：プラグインが追跡するトピックフィルターのリスト。空リストの場合、そのバックエンドは無効となります。
- `pool_size`：バックエンドへの接続プールサイズ。
- `batch_size` / `batch_time`：書き込みバッチのパラメータ（両方を `1`/`0` に設定するとバッチ処理を無効化）。
- `ssl.*`：バックエンド接続の TLS 設定。

### MySQL 固有設定

- `server`：`host:port`。
- `username`、`password`、`database`：認証情報。
- `init_default_schema`：`true` の場合、プラグイン起動時にデフォルトの `mqtt_msg` / `mqtt_sub` テーブルを作成します。スキーマを自分で管理する場合は `false` のままにしてください。
- `insert_message_sql`、`delete_message_sql`、`select_message_sql`、`insert_subscription_sql`、`select_subscriptions_sql`、`delete_subscription_sql`：MQTT メッセージフィールド（`id`、`from`、`topic`、`qos`、`payload`、`flags.retain`、`timestamp`）およびサブスクリプションフィールド（`clientid`、`topic`、`qos`）にバインドされる `${var}` プレースホルダーを含む、上書き可能な SQL テンプレート。

### Redis 固有設定

- `servers`：カンマ区切りの `host:port` リスト。センチネルモードの場合は `redis_type = "sentinel"` とし、`sentinel` にマスター名を設定します。
- `redis_type`：`single`、`sentinel`、`cluster` のいずれか。
- `username`、`password`、`database`：認証情報と論理 DB（クラスター モードでは `database` は無視されます）。
- `message_key_prefix`（デフォルト `mqtt:msg`）、`subscription_key_prefix`（デフォルト `mqtt:sub`）：キーのネームスペース。
- `message_ttl`：メッセージごとの TTL（秒）。TTL を超えたメッセージはトピックごとのソート済みセットインデックスからクリーンアップされます。

## データベーススキーマ

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

`init_default_schema = true` に設定するとプラグイン起動時にこれらのテーブルが作成されます。そうでなければバックエンドを有効化する前に自分で作成してください。

### Redis

Redis の構造は必要に応じて作成されるため、スキーママイグレーションは不要です。

- `mqtt:sub:{clientid}`：ハッシュ、`{topic} -> {qos}`。
- `mqtt:msg:{msgid}`：ハッシュ、フィールドは `id`、`from`、`qos`、`topic`、`payload`、`ts`、`retain`。`msgid` は base62 エンコードされています。
- `mqtt:msg:{topic}`：ソート済みセット、メンバーは base62 メッセージ ID、スコアはタイムスタンプ。期限切れのクリーンアップに使用されます。

Redis ACL を使用している場合は、接続ユーザーに対して `mqtt:sub:*` および `mqtt:msg:*` にマッチするキーに対し、`HSET`、`HDEL`、`HGETALL`、`HMSET`、`DEL`、`EXPIRE`、`ZADD`、`ZRANGE`、`ZREMRANGEBYSCORE`、`ZREM` の権限を付与してください。

## インストール

以下の [Download](#download) セクションからご利用の EMQX バージョンに対応する tarball をダウンロードし、ダッシュボード、REST API、または CLI でインストールしてください。

REST API を使う場合：

```bash
curl -u key:secret -X POST http://localhost:18083/api/v5/plugins/install \
  -H "Content-Type: multipart/form-data" \
  -F "plugin=@emqx_offline_messages-<version>.tar.gz"
```

プラグインを起動（ダッシュボード、または `emqx ctl plugins start emqx_offline_messages-<version>`）し、ダッシュボードのプラグイン詳細ページでバックエンドを設定してください。

## 動作確認

プラグインが追跡しているトピックに対し、サブスクライブされていない状態でいくつかメッセージをパブリッシュします。

```bash
mqttx pub -q 1 -t 't/2' -m 'hello-from-offline1'
mqttx pub -q 1 -t 't/2' -m 'hello-from-offline2'
mqttx pub -q 1 -t 't/2' -m 'hello-from-offline3'
```

その後、新しいクライアント ID でサブスクライブすると、プラグインに保存されたメッセージが再生されます。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各 EMQX リリース向け tarball：

| EMQX バージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 5.10.4 | 2.0.0 | [emqx_offline_messages-2.0.0.tar.gz](https://packages.emqx.io/emqx-plugins/e5.10.4/emqx_offline_messages-2.0.0.tar.gz) |

<!-- PLUGIN-DOWNLOADS:END -->
