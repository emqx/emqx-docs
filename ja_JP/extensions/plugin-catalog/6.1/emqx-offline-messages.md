# オフラインメッセージ

このプラグインは、サブスクライバーがオンラインでない場合に選択されたQoS 1/2のメッセージを永続化し、後で該当するサブスクライバーがオンラインになった際にそれらを再生します。

対応バックエンド：

- MySQL
- Redis

## 設定

プラグインは標準のEMQXプラグイン設定APIを通じて設定します：

`PUT /api/v5/plugins/<name-vsn>/config`

スキーマは `priv/config.hocon` に定義されており、RedisおよびMySQL用のバックエンド固有の設定が含まれています。

プラグインがメッセージを永続化する条件は以下の通りです：

- パブリッシュのQoSが `0` より大きい
- トピックが設定された `message.topic_filter` のいずれかにマッチする

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

Redisは `mqtt:sub:*` および `mqtt:msg:*` キースペースの下でハッシュとソート済みセットを使用します。

- サブスクリプション状態はクライアントIDをキーとしたハッシュに保存されます。
- メッセージペイロードはメッセージIDをキーとしたハッシュに保存されます。
- トピックインデックスはトピックをキーとしたソート済みセットに保存されます。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各EMQXリリース用のtarball：

| EMQXバージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.1.2 | 2.0.0 | [emqx_offline_messages-2.0.0.tar.gz](https://packages.emqx.io/emqx-plugins/6.1.2/emqx_offline_messages-2.0.0.tar.gz) |
| 6.1.3 | 2.0.0 | [emqx_offline_messages-2.0.0.tar.gz](https://packages.emqx.io/emqx-plugins/6.1.3/emqx_offline_messages-2.0.0.tar.gz) |
| 6.1.4 | 2.0.0 | [emqx_offline_messages-2.0.0.tar.gz](https://packages.emqx.io/emqx-plugins/6.1.4/emqx_offline_messages-2.0.0.tar.gz) |

<!-- PLUGIN-DOWNLOADS:END -->
