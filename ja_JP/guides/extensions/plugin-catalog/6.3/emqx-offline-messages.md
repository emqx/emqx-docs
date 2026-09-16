# オフラインメッセージ

このプラグインは、サブスクライバーがオンラインでない場合に選択されたQoS 1/2のメッセージをパーシステンスし、後で該当するサブスクライバーがオンラインになった際にそれらを再生します。

対応バックエンド：

- MySQL
- Redis

## 設定

プラグインは標準のEMQXプラグイン設定APIを通じて設定します：

`PUT /api/v5/plugins/<name-vsn>/config`

スキーマは`priv/config.hocon`に定義されており、RedisおよびMySQLのバックエンド固有の設定を含みます。

プラグインは以下の場合にのみメッセージをパーシステンスします：

- パブリッシュのQoSが`0`より大きい場合
- トピックが設定された`message.topic_filter`のいずれかにマッチする場合

## ビルドとテスト

リポジトリのルートからプラグインをビルドします：

```bash
make
```

このプラグインのCommon Testスイートを実行します：

```bash
make plugins/emqx_offline_messages-ct
```

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

Redisは`mqtt:sub:*`および`mqtt:msg:*`キー空間の下でハッシュとソート済みセットを使用します。

- サブスクリプション状態はクライアントIDをキーとしたハッシュに保存されます。
- メッセージのペイロードはメッセージIDをキーとしたハッシュに保存されます。
- トピックのインデックスはトピックをキーとしたソート済みセットに保存されます。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各EMQXリリースのtarball：

| EMQXバージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.3.0 | 2.0.1 | [emqx_offline_messages-2.0.1.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.3.0/emqx_offline_messages-2.0.1.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.3.0/emqx_offline_messages-2.0.1.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
