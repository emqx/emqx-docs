# EMQX Backup Sync

このプラグインは、既存のデータバックアップAPIを使用して、プライマリEMQXクラスターからセカンダリEMQXクラスターへ選択されたバックアップデータを定期的に同期します。

セカンダリクラスターはプライマリクラスターに対してバックアップファイルのエクスポートを要求し、そのファイルをダウンロードしてローカルにアップロードし、インポートします。選択された設定ルートはEMQXの既存の設定インポートのセマンティクスに従ってインポートされます。選択されたMnesiaテーブルセットはスナップショットとしてインポートされるため、セカンダリクラスターにのみ存在するそれらのテーブルセット内のレコードは削除されます。選択されていないルートおよびテーブルセットは変更されません。

## 設定

各セカンダリクラスターにプラグインをインストールして起動してください。プライマリクラスターにはこのプラグインをインストールする必要はなく、セカンダリクラスターからアクセス可能なDashboardのデータバックアップAPIが必要です。

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

設定されたAPIキーはプライマリクラスターのデータバックアップエンドポイントへのアクセスが許可されている必要があります。`primary.api_key`および`primary.api_secret`は直接設定するか、`file://`パス（例：`file:///etc/emqx/backup-sync-api-key`）として指定できます。

サポートされている`sync.root_keys`の値は`connectors`、`actions`、`sources`、`rule_engine`、`listeners`、`schema_registry`、`authentication`、および`authorization`です。

ルールは一般的にコネクター、アクション、ソース、およびスキーマレジストリのオブジェクトに依存します。`rule_engine`を依存関係なしで同期すると、インポートに失敗したり、実行時動作が不完全になる可能性があります。セカンダリクラスターに存在しない限り、それらの依存ルートを`sync.root_keys`に含めてください。

デフォルトでは、同期には`banned`、`builtin_authn`、`builtin_authz`のテーブルセットも含まれます。これらの選択されたテーブルセットはセカンダリクラスター上で置き換えられます。設定のみの同期が必要な場合は`sync.table_sets = []`に設定してください。サポートされている`sync.table_sets`の値は`banned`、`builtin_authn`、`builtin_authz`、`builtin_retainer`、`psk`、および`mt`です。プライマリのデータバックアップAPIはAPIキーで呼び出された場合、`dashboard_users`や`api_keys`は含まれません。

## CLI

セカンダリノード上で以下のコマンドを使用して、ローカルの同期ワーカーを確認できます。

```bash
emqx ctl backup_sync status
```

このコマンドはローカルノード、ヘルス状態、ワーカー状態、選択されたコアノード、次回の同期予定、および機密情報を含まない同期設定を表示します。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各EMQXリリースのtarball：

| EMQXバージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.1.3 | 0.1.0 | [emqx_backup_sync-0.1.0.tar.gz](https://packages.emqx.io/emqx-plugins/6.1.3/emqx_backup_sync-0.1.0.tar.gz) |

<!-- PLUGIN-DOWNLOADS:END -->
