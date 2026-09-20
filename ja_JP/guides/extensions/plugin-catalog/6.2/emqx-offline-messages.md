# オフラインメッセージ

このプラグインは、サブスクライバーがオンラインでない場合に選択された QoS 1/2 のメッセージをパーシステンスし、後で一致するサブスクライバーがオンラインになった際にそれらを再生します。

対応バックエンド：

- MySQL
- Redis

## 設定

プラグインは標準の EMQX プラグイン設定 API を通じて設定します：

`PUT /api/v5/plugins/<name-vsn>/config`

スキーマは `priv/config.hocon` に定義されており、Redis と MySQL のバックエンド固有の設定を含みます。

プラグインがメッセージをパーシステンスする条件は以下の通りです：

- パブリッシュの QoS が `0` より大きい
- トピックが設定された `message.topic_filter` のいずれかにマッチする

## ビルドとテスト

リポジトリのルートからプラグインをビルドします：

```bash
make
```

このプラグインの Common Test スイートを実行します：

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

Redis は `mqtt:sub:*` および `mqtt:msg:*` キースペースの下でハッシュとソート済みセットを使用します。

- サブスクリプション状態はクライアント ID をキーとしたハッシュに保存されます。
- メッセージのペイロードはメッセージ ID をキーとしたハッシュに保存されます。
- トピックのインデックスはトピックをキーとしたソート済みセットに保存されます。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各 EMQX リリースの tarball：

| EMQX バージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.2.1 | 2.0.0 | [emqx_offline_messages-2.0.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.1/emqx_offline_messages-2.0.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.1/emqx_offline_messages-2.0.0.sha256)) |
| 6.2.2 | 2.0.0 | [emqx_offline_messages-2.0.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.2/emqx_offline_messages-2.0.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.2/emqx_offline_messages-2.0.0.sha256)) |
| 6.2.3 | 2.0.0 | [emqx_offline_messages-2.0.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.3/emqx_offline_messages-2.0.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.3/emqx_offline_messages-2.0.0.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
