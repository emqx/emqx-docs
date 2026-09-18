# EMQX Backup Sync

EMQX Backup Syncプラグインは、プライマリEMQXクラスターからセカンダリクラスターへ選択した設定および組み込みデータベースのデータを定期的に同期します。本プラグインはEMQX Enterprise 5.10.5以降の5.10リリースで利用可能です。

プラグインはセカンダリクラスターにのみインストールして起動してください。プライマリクラスターにはプラグインのインストールは不要ですが、セカンダリクラスターからプライマリクラスターのダッシュボードデータバックアップAPIにアクセス可能である必要があります。

## 同期の仕組み

プラグインは有効な設定で起動すると、非同期で初回同期を実行し、設定された間隔で繰り返します。セカンダリクラスターでは、稼働中のコアノードのうち1つだけが同期処理を行います。そのノードが利用不可になった場合は、次の同期間隔で別のコアノードが引き継ぎます。

各同期処理は以下の操作を実行します。

1. プライマリクラスターに対して選択された設定ルートおよびMnesiaテーブルセットのエクスポートを要求します。
2. プライマリクラスターからバックアップをダウンロードし、セカンダリクラスターにアップロードします。
3. セカンダリクラスターでバックアップをインポートします。
4. クリーンアップ設定に従い、バックアップファイルを削除または保持します。

設定ルートとMnesiaテーブルセットは異なるインポート動作を使用します。

- **設定ルート**：標準のEMQX設定インポート動作を使用します。プライマリクラスターの値は挿入または更新され、セカンダリクラスターにのみ存在する設定は削除されません。
- **Mnesiaテーブルセット**：スナップショット復元動作を使用します。対応するテーブルはプライマリクラスターのスナップショットで置き換えられます。

::: warning 重要なお知らせ

スナップショット復元は、セカンダリクラスターにのみ存在する選択されたテーブルセットのレコードを削除します。プラグイン起動前に `sync.table_sets` の内容を十分に確認してください。

:::

同期はプラグインが起動し、設定が有効な場合にのみ実行されます。有効な設定を適用すると、同期タスクが実行中でなければ即座に同期が開始されます。タスクが実行中の場合はキャンセル要求が送られ、更新された設定で後続の同期が試みられます。キャンセルは各処理段階の間でのみ確認されるため、進行中のインポート処理は完了する場合があります。

## プラグインの設定

1. プライマリクラスターで、[APIキーを作成](../../api.md#create-api-keys)し、`administrator`ロールを付与します。APIスコープが設定されている場合は、本プラグインが使用する`/data/*`エンドポイントへのアクセス権を付与するために`system`スコープを含めてください。

2. セカンダリクラスターからプライマリクラスターのダッシュボードAPIにアクセス可能であることを確認してください。両クラスターはEMQX 5.10で動作させてください。セカンダリクラスターは、後方互換性のないメジャーまたはマイナーバージョンで作成されたバックアップをインポートできません。

3. セカンダリクラスターにて、[プラグイン管理](../plugin-management.md)に従い`emqx_backup_sync`をインストールします。

4. プラグインを設定します。以下はTLS証明書検証を有効にする例です。

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

   `primary.api_key` と `primary.api_secret` は直接値を指定するか、`file://` パス（例：`file:///etc/emqx/backup-sync-api-key`）を指定できます。認証情報ファイルの末尾に改行がある場合、プラグインは使用前に改行を削除します。

   任意の稼働中コアノードが同期ノードになる可能性があるため、認証情報ファイルおよび `primary.ssl.cacertfile`、`primary.ssl.certfile`、`primary.ssl.keyfile` で指定されたファイルは、セカンダリクラスター内のすべてのコアノードで同じパスに存在し、EMQXが読み取り可能でなければなりません。

5. セカンダリクラスターでプラグインを起動します。

### 設定オプション

| オプション | デフォルト | 説明 |
| --- | --- | --- |
| `primary.base_url` | なし | プライマリクラスターのダッシュボードAPIのベースURL（`/api/v5`を含む）。 |
| `primary.api_key` | なし | プライマリクラスターへアクセスするためのAPIキー。直接値または`file://`パスを指定可能。 |
| `primary.api_secret` | なし | プライマリクラスターへアクセスするためのAPIシークレット。直接値または`file://`パスを指定可能。 |
| `primary.ssl.enable` | `false` | プライマリクラスターへのHTTPSリクエストでTLSオプションを有効化。 |
| `primary.ssl.server_name_indication` | `disable` | TLSハンドシェイク時に送信するサーバー名識別（SNI）。 |
| `primary.ssl.verify` | `verify_none` | TLS証明書検証モード。`verify_none`または`verify_peer`を指定可能。運用環境では`verify_peer`を推奨し、`primary.ssl.cacertfile`を設定してください。 |
| `primary.ssl.cacertfile` | なし | プライマリサーバーの検証に使用するCA証明書ファイルのパス。 |
| `primary.ssl.certfile` | なし | 相互TLS認証用のクライアント証明書ファイルのパス。 |
| `primary.ssl.keyfile` | なし | 相互TLS認証用のクライアント秘密鍵ファイルのパス。 |
| `sync.interval` | `5m` | 同期試行の間隔。 |
| `sync.root_keys` | [設定スコープ](#configuration-scope)参照 | プライマリクラスターからエクスポートする設定ルート。 |
| `sync.table_sets` | [設定スコープ](#configuration-scope)参照 | プライマリクラスターからエクスポートし、セカンダリクラスターでスナップショットとして復元するMnesiaテーブルセット。 |
| `sync.timeout` | `30s` | プライマリクラスターへの各HTTPリクエストのタイムアウト。 |
| `sync.retain_remote_backup` | `false` | エクスポートされたバックアップをプライマリクラスターに保持するか。デフォルトはクリーンアップ時に削除。 |
| `sync.retain_backup_after_import` | `true` | インポート後にセカンダリクラスターにアップロードされたバックアップを保持するか。 |

HTTPクライアントはリダイレクトを自動で追従しません。`primary.base_url`は最終的なダッシュボードAPIのアドレスを指定してください。

クリーンアップはエクスポート成功後に毎回実行されます。後続の処理が失敗または同期がキャンセルされても実行されます。デフォルトではエクスポートされたバックアップはプライマリクラスターから削除されます。トラブルシューティングのために保持したい場合は、`sync.retain_remote_backup = true`に設定してください。

## 設定スコープ

デフォルトの`sync.root_keys`は以下の通りです。

- `connectors`
- `actions`
- `sources`
- `rule_engine`
- `listeners`
- `schema_registry`

`authentication`および`authorization`も追加可能です。これら8つのルートはバックアップ同期に推奨されます。

`sync.root_keys`はプライマリクラスターの`/data/export` APIがサポートする他のルートも受け入れます。未知のルートは`400 Invalid root keys`エラーを返します。APIで受け入れられても同期できるとは限りません。例えば`node`や`rpc`などのルートはインポート時にスキップされます。追加する前に、インポート可能かつセカンダリクラスターに適切かを確認してください。

ルールは通常、コネクター、アクション、ソース、およびスキーマレジストリのオブジェクトに依存します。`rule_engine`を同期する場合は、依存するルートも含めてください。セカンダリクラスターに同等のオブジェクトが存在しない場合、インポートに失敗したり、インポートしたルールが期待通りに動作しない可能性があります。

デフォルトの`sync.table_sets`は`banned`、`builtin_authn`、`builtin_authz`です。`builtin_retainer`、`psk`、`mt`も選択可能です。設定を`sync.table_sets = []`にすると設定のみ同期します。

プラグインはAPIキーでデータバックアップAPIに認証するため、`dashboard_users`や`api_keys`のテーブルセットは同期できません。データバックアップAPIはAPIキーで要求されたエクスポートからこれらの機密テーブルセットを除外します。

## 同期状況の確認

セカンダリクラスターの任意のノードで以下のコマンドを実行してください。

```bash
emqx ctl backup_sync status
```

コマンドは選択されたコアノードに問い合わせ、全体の状態、ヘルス、同期の有効状態、同期タスクの状態、選択されたコアノード、次回同期までの時間、プライマリAPIのベースURL、間隔、ルートキー、テーブルセットを表示します。API認証情報は表示されません。

インポートが成功してもバックアップのクリーンアップに失敗した場合は同期失敗と報告されます。ヘルス出力およびEMQXログを参照し、失敗がエクスポート、ダウンロード、アップロード、インポート、クリーンアップ、またはワーカーキャンセルのどの段階で発生したかを特定してください。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各EMQXリリース用のtarball：

| EMQXバージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 5.10.5 | 0.1.0 | [emqx_backup_sync-0.1.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/e5.10.5/emqx_backup_sync-0.1.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/e5.10.5/emqx_backup_sync-0.1.0.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
