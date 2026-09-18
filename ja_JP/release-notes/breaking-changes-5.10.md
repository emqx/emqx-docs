# EMQX 5.10 における非互換変更

## 5.10.5

- [#17864](https://github.com/emqx/emqx/pull/17864) ダッシュボードのユーザーおよび API キーのエンドポイントは、特権スコープ（`system`、`user_management`、`api_key_management`、`sso_management`）とその他のスコープが混在したスコープリストを拒否するようになりました。これら4つの特権スコープは管理者相当の権限を持つため、制限付きスコープリストと組み合わせてもアカウントを意味のある形で制限できません。アカウントに管理者相当の権限を持たせるかどうかに応じて、特権のみのスコープリストか非特権のみのスコープリストのいずれかを使用してください。既存の混在したスコープセットを持つレコードは次回の更新まで動作を継続しますが、次回の更新時にはリストを分割する必要があります。

- [#18465](https://github.com/emqx/emqx/pull/18465) EMQX は ClickHouse、TDengine、SQL Server のアクションおよびバッチ挿入が有効な MySQL アクションにおいて、テンプレート化された `INSERT` 文を検証し安全にレンダリングするために制限付き SQL パーサーを使用するようになりました。コメントを含む既存のテンプレートやサポートされていない SQL 構文を使用しているテンプレートは、新しいパーサーで使用する前に更新が必要です。

  ClickHouse、TDengine、SQL Server はアクション作成時に無効なテンプレートを拒否します。MySQL は解析エラーをログに記録しますがアクション作成は継続する場合があり、サポートされていないテンプレートを使ったバッチリクエストは実行時に失敗します。

  サポートされるバックエンド固有構文：

  - **MySQL**: `ON DUPLICATE KEY UPDATE`
  - **ClickHouse**: `FORMAT Values` および `FORMAT JSONCompactEachRow`
  - **TDengine**: `INSERT ... USING ... TAGS` およびテーブル識別子の補間

  その他の挙動変更：

  - MySQL ブリッジはすべての接続で `ANSI_QUOTES` と `NO_BACKSLASH_ESCAPES` を無効化します。
  - ClickHouse ブリッジは SQL テンプレートからバッチ値区切り文字を推測し、設定された `batch_value_separator` を無視します。

- [#17593](https://github.com/emqx/emqx/pull/17593) `emqx ctl relup upgrade` に `--force` フラグを追加しました。デフォルトでは、`data/patches/` に `*.beam` ホットパッチファイルが存在する場合、アップグレードは中断されます（これらのパッチがアップグレード対象のモジュールを上書きするため）。`--force` を指定するとパッチを保持したままアップグレードを続行します。

- [#18823](https://github.com/emqx/emqx/pull/18823) `emqx ctl listeners` の出力におけるフィールド名の誤字を修正しました。

  コマンドはリスナーの有効フラグを `enbale` と表示していましたが、現在は `enable` と表示します。この出力を解析するスクリプトは修正が必要です。

## 5.10.4

- [#17244](https://github.com/emqx/emqx/pull/17244) ホットアップグレードの REST API エンドポイント（`/api/v5/relup/*`）を削除しました。ホットアップグレードは各ノード上の `emqx ctl relup` CLI でのみ操作可能となり、ダッシュボードからの操作はできなくなりました。

  対象リリースの tarball と同名の `.sha256` サイドカーを EMQX プロセスが読み取れる任意の場所に配置し、各ノードで `emqx ctl relup upgrade <TarballPath>` を実行してアップグレードを適用してください。対象バージョンは tarball 内の `releases/emqx_vars` (`REL_VSN`) から読み取られます。

## 5.10.3

- [#16491](https://github.com/emqx/emqx/pull/16491) macOS 13（Ventura）向けのパッケージ配布を終了しました。

## 5.10.2

- [#16062](https://github.com/emqx/emqx/pull/16062) RocketMQ アクションが設定されたペイロードテンプレートを無視し、ルール出力全体を送信してしまう問題を修正しました。

  以前の（誤った）動作に依存していた場合は、メッセージが期待通りにフォーマットされるようペイロードテンプレートを更新する必要があります。

## 5.10.1

- [#15752](https://github.com/emqx/emqx/pull/15752) リスナーの接続レート制限（`max_conn_rate` と `max_conn_burst`）は、アクセプター単位ではなくリスナー単位で適用されるようになり、5.9.0 以前の動作に戻りました。そのため、5.9.0、5.9.1、5.10.0 の設定は非互換であり、指定したレートは各リスナーに設定されたアクセプター数でスケールアップする必要があります。

## 5.10.0

- [#15289](https://github.com/emqx/emqx/pull/15289) すべてのコネクター、アクション、ソースに新しい `resource_opts.health_check_timeout` 設定を追加しました。デフォルト値は 60 秒です。ヘルスチェックがこの時間を超えて応答しない場合、コネクター／アクション／ソースは `disconnected` と見なされます。

  注意：デフォルトが 60 秒のため、以前は 60 秒以上かかって正常応答していた場合でも、今回の変更によりその状況では切断と判断される可能性があります。

- [#15286](https://github.com/emqx/emqx/pull/15286) 設定オプション `broker.routing.storage_schema` は非推奨となり無視されます。旧式の `v1` ルーティングストレージスキーマはサポートされず、これを使用しているクラスターでは EMQX の起動が拒否されます。`v1` ルーティングスキーマを使用しているクラスターのアップグレード手順は、[EMQX 5.10 以降のローリングアップグレードの考慮事項](../get-started/deploy/rolling-upgrades.md#rolling-upgrade-considerations-for-emqx-5.10-or-later) を参照してください。

- [#15239](https://github.com/emqx/emqx/pull/15239) `multi_tenancy.default_max_sessions` の型は `infinity` または正の整数のみとなりました。以前は `0` も許容されていました。

- [#15156](https://github.com/emqx/emqx/pull/15156) `dashboard.sso.oidc.issuer` フィールドにスキーマ検証を追加しました。この値は有効な URL であることがチェックされます。
