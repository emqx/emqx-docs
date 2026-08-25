# EMQX Backup Sync

このプラグインは、既存のデータバックアップAPIを使用して、プライマリEMQXクラスターからセカンダリEMQXクラスターへ選択されたバックアップデータを定期的に同期します。

セカンダリクラスターはプライマリクラスターに対してバックアップファイルのエクスポートを呼び出し、そのファイルをダウンロードし、ローカルにアップロードしてインポートします。選択された設定ルートはEMQXの既存の設定インポートのセマンティクスに従ってインポートされます。選択されたMnesiaテーブルセットはスナップショットとしてインポートされるため、セカンダリクラスターにのみ存在するレコードは削除されます。選択されていないルートおよびテーブルセットは変更されません。

## 設定

各セカンダリクラスターにプラグインをインストールして起動してください。プライマリクラスターにはこのプラグインのインストールは不要で、セカンダリクラスターからアクセス可能なDashboardのデータバックアップAPIが必要です。

```hocon
primary {
  base_url = "https://primary.example.com:18083/api/v5"
  api_key = "sync-key"
  api_secret = "sync-secret"
  ssl {
    enable = true
    server_name_indication = "primary.example.com"
    verify = "verify_peer"
    cacertfile = "/etc/emqx/certs/primary-ca.pem"
    certfile = ""
    keyfile = ""
  }
}

sync {
  interval = "5m"
  root_keys = [
    "connectors",
    "actions",
    "sources",
    "rule_engine",
    "listeners",
    "schema_registry"
  ]
  table_sets = [
    "banned",
    "builtin_authn",
    "builtin_authz"
  ]
  timeout = "30s"
  retain_remote_backup = false
  retain_backup_after_import = true
}
```

設定されたAPIキーはプライマリクラスターのデータバックアップエンドポイントへのアクセスが許可されている必要があります。`primary.api_key`および`primary.api_secret`は直接設定するか、`file://`パス（例：`file:///etc/emqx/backup-sync-api-key`）として設定可能です。

サポートされる`sync.root_keys`の値は`connectors`、`actions`、`sources`、`rule_engine`、`listeners`、`schema_registry`、`authentication`、および`authorization`です。

ルールは一般的にコネクター、アクション、ソース、およびスキーマレジストリのオブジェクトに依存します。`rule_engine`のみを同期し、依存関係を含めない場合、インポートに失敗するか不完全なランタイム動作を引き起こす可能性があります。セカンダリクラスターに既に存在しない限り、依存するルートを`sync.root_keys`に含めてください。

デフォルトでは、同期は`banned`、`builtin_authn`、`builtin_authz`のテーブルセットも含みます。これらの選択されたテーブルセットはセカンダリクラスター上で置き換えられます。設定のみの同期が必要な場合は、`sync.table_sets = []`に設定してください。サポートされる`sync.table_sets`の値は`banned`、`builtin_authn`、`builtin_authz`、`builtin_retainer`、`psk`、および`mt`です。プライマリのデータバックアップAPIはAPIキーで呼び出された場合、`dashboard_users`や`api_keys`は含みません。

## CLI

セカンダリノードで以下のコマンドを使用してローカルの同期ワーカーを確認できます。

```bash
emqx ctl backup_sync status
```

このコマンドはローカルノード、ヘルス状態、ワーカーの状態、選択されたコアノード、次回の同期予定、そして機密情報を含まない同期設定を表示します。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各 EMQX リリースに対応するプラグインパッケージ:

| EMQX バージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.0.3 | 0.1.0 | [emqx_backup_sync-0.1.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.0.3/emqx_backup_sync-0.1.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.0.3/emqx_backup_sync-0.1.0.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
