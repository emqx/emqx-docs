# オフラインメッセージ

このプラグインは、MQTTメッセージを外部データベースに永続化することで、サブスクライバーが再接続後にメッセージを取得できるようにします。パブリッシュ時に切断されていた場合でも対応可能です。

標準のMQTTセッション永続化だけでは不十分なシナリオ、例えばセッションを超えてメッセージを保持する必要がある場合や、他のシステムもメッセージ履歴を参照する必要がある場合に有用です。

対応バックエンド：

- **MySQL**：2つのテーブル（`mqtt_msg`、`mqtt_sub`）を使用し、SQL文は設定可能です。
- **Redis**：サブスクリプション／メッセージはハッシュで管理し、トピックごとのインデックスとしてソート済みセットを使用します。シングル、センチネル、クラスターの各デプロイに対応しています。

同時に有効なのは1つのバックエンドのみで、`mysql.enable` または `redis.enable` のいずれかを有効にしてください。

## プラグイン設定

プラグインには両バックエンドをカバーするデフォルトの `config.hocon` が付属しています。ダッシュボードのプラグイン詳細ページも同じスキーマを表示するため、多くの運用者はファイルを直接編集するよりUIから設定しています。

共通のトップレベルフィールド：

- `topics`：プラグインが追跡するトピックフィルターのリスト。空リストの場合、そのバックエンドではプラグインは無効となります。
- `pool_size`：バックエンドへの接続プールサイズ。
- `batch_size` / `batch_time`：書き込みのバッチ処理パラメータ（両方を `1` / `0` に設定するとバッチ処理を無効化）。
- `ssl.*`：バックエンド接続のTLS設定。

### MySQL固有設定

- `server`：`host:port`。
- `username`、`password`、`database`：認証情報。
- `init_default_schema`：`true` の場合、プラグイン起動時にデフォルトの `mqtt_msg` / `mqtt_sub` テーブルを作成します。スキーマを自分で管理する場合は `false` のままにしてください。
- `insert_message_sql`、`delete_message_sql`、`select_message_sql`、`insert_subscription_sql`、`select_subscriptions_sql`、`delete_subscription_sql`：MQTTメッセージフィールド（`id`、`from`、`topic`、`qos`、`payload`、`flags.retain`、`timestamp`）およびサブスクリプションフィールド（`clientid`、`topic`、`qos`）にバインドされる `${var}` プレースホルダーを含む、上書き可能なSQLテンプレート。

### Redis固有設定

- `servers`：カンマ区切りの `host:port` リスト。センチネルモードの場合は `redis_type = "sentinel"` とし、`sentinel` にマスター名を設定します。
- `redis_type`：`single`、`sentinel`、`cluster` のいずれか。
- `username`、`password`、`database`：認証情報および論理DB（クラスターでは `database` は無視されます）。
- `message_key_prefix`（デフォルト `mqtt:msg`）、`subscription_key_prefix`（デフォルト `mqtt:sub`）：キーの名前空間。
- `message_ttl`：メッセージごとのTTL（秒単位）。TTLを超えたメッセージはトピックごとのソート済みセットインデックスからクリーンアップされます。

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

`init_default_schema = true` に設定すると、プラグインの初回起動時にこれらのテーブルを作成します。そうでない場合は、バックエンドを有効にする前に自分で作成してください。

### Redis

Redisの構造はオンデマンドで作成されるため、スキーママイグレーションは不要です。

- `mqtt:sub:{clientid}`：ハッシュ、`{topic} -> {qos}`。
- `mqtt:msg:{msgid}`：ハッシュ、フィールドは `id`、`from`、`qos`、`topic`、`payload`、`ts`、`retain`。`msgid` はbase62エンコードされています。
- `mqtt:msg:{topic}`：ソート済みセット、メンバーはbase62のメッセージID、スコアはタイムスタンプ。期限切れのクリーンアップに使用。

Redis ACLを使用している場合は、接続ユーザーに対して `mqtt:sub:*` および `mqtt:msg:*` にマッチするキーで `HSET`、`HDEL`、`HGETALL`、`HMSET`、`DEL`、`EXPIRE`、`ZADD`、`ZRANGE`、`ZREMRANGEBYSCORE`、`ZREM` の権限を付与してください。

## インストール

以下の[ダウンロード](#download)セクションから、使用しているEMQXバージョンに対応したtarballをダウンロードし、ダッシュボード、REST API、またはCLI経由でインストールしてください。

REST APIを使う場合：

```bash
curl -u key:secret -X POST http://localhost:18083/api/v5/plugins/install \
  -H "Content-Type: multipart/form-data" \
  -F "plugin=@emqx_offline_messages-<version>.tar.gz"
```

プラグインを起動します（ダッシュボード、または `emqx ctl plugins start emqx_offline_messages-<version>`）。その後、ダッシュボードのプラグイン詳細ページを開き、バックエンドの設定を行ってください。

## 動作確認

プラグインが追跡しているトピックに対して、サブスクライブしていない状態でいくつかメッセージをパブリッシュします：

```bash
mqttx pub -q 1 -t 't/2' -m 'hello-from-offline1'
mqttx pub -q 1 -t 't/2' -m 'hello-from-offline2'
mqttx pub -q 1 -t 't/2' -m 'hello-from-offline3'
```

その後、新しいクライアントIDでサブスクライブすると、プラグインに保存されたメッセージがリプレイされます。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各EMQXリリース向けのtarball：

| EMQXバージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 5.10.4 | 2.0.0 | [emqx_offline_messages-2.0.0.tar.gz](https://packages.emqx.io/emqx-plugins/e5.10.4/emqx_offline_messages-2.0.0.tar.gz) |

<!-- PLUGIN-DOWNLOADS:END -->
