# EMQX Backup Sync

このプラグインは、既存のデータバックアップAPIを使用して、プライマリEMQXクラスターからセカンダリEMQXクラスターへ選択されたバックアップデータを定期的に同期します。

セカンダリクラスターはプライマリクラスターに対してバックアップファイルのエクスポートを呼び出し、そのファイルをダウンロードし、ローカルにアップロードしてインポートします。選択された設定ルートはEMQXの既存の設定インポートのセマンティクスに従ってインポートされます。選択されたMnesiaテーブルセットはスナップショットとしてインポートされるため、セカンダリクラスターにのみ存在するレコードは削除されます。選択されていないルートおよびテーブルセットは変更されません。

## 設定

各セカンダリクラスターにプラグインをインストールして起動してください。プライマリクラスターにはこのプラグインのインストールは不要で、セカンダリクラスターからアクセス可能なDashboardのデータバックアップAPIがあれば十分です。

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

設定されたAPIキーはプライマリクラスターのデータバックアップエンドポイントへのアクセスが許可されている必要があります。`primary.api_key`および`primary.api_secret`は直接設定するか、`file://`パス（例: `file:///etc/emqx/backup-sync-api-key`）として指定できます。

サポートされている`sync.root_keys`の値は`connectors`、`actions`、`sources`、`rule_engine`、`listeners`、`schema_registry`、`authentication`、および`authorization`です。

ルールは通常、コネクター、アクション、ソース、およびスキーマレジストリのオブジェクトに依存します。`rule_engine`を依存関係なしに同期すると、インポートが失敗したり不完全なランタイム動作を引き起こす可能性があります。これらの依存ルートがセカンダリクラスターに存在しない場合は、`sync.root_keys`に含めてください。

デフォルトでは、同期には`banned`、`builtin_authn`、`builtin_authz`のテーブルセットも含まれます。これらの選択されたテーブルセットはセカンダリクラスター上で置き換えられます。設定のみの同期が必要な場合は`sync.table_sets = []`に設定してください。サポートされている`sync.table_sets`の値は`banned`、`builtin_authn`、`builtin_authz`、`builtin_retainer`、`psk`、および`mt`です。プライマリのデータバックアップAPIはAPIキーで呼び出した場合、`dashboard_users`や`api_keys`は含みません。

## CLI

セカンダリノード上で以下のコマンドを使用してローカルの同期ワーカーを確認できます。

```bash
emqx ctl backup_sync status
```

このコマンドはローカルノード、ヘルス状態、ワーカー状態、選択されたコアノード、次回の同期予定、および非機密の同期設定を表示します。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各EMQXリリースのtarball：

| EMQXバージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.0.3 | 0.1.0 | [emqx_backup_sync-0.1.0.tar.gz](https://packages.emqx.io/emqx-plugins/6.0.3/emqx_backup_sync-0.1.0.tar.gz) |

<!-- PLUGIN-DOWNLOADS:END -->
