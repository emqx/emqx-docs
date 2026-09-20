# EMQX 5.10 における互換性のない変更点

## 5.10.4

- [#17244](https://github.com/emqx/emqx/pull/17244) ホットアップグレードの REST API エンドポイント（`/api/v5/relup/*`）を削除しました。ホットアップグレードは現在、各ノード上の `emqx ctl relup` CLI を通じてのみ操作され、ダッシュボード上には表示されません。

  対象のリリース tarball とその `.sha256` サイドカー（同じベース名、同じディレクトリ）を EMQX プロセスが読み取り可能な場所に配置してください。各ノードで `emqx ctl relup upgrade <TarballPath>` を実行してアップグレードを適用します。対象バージョンは tarball 内の `releases/emqx_vars`（`REL_VSN`）から読み取られます。

## 5.10.3

- [#16491](https://github.com/emqx/emqx/pull/16491) macOS 13（Ventura）向けのパッケージのリリースを停止しました。

## 5.10.2

- [#16062](https://github.com/emqx/emqx/pull/16062) RocketMQ アクションが設定されたペイロードテンプレートを無視し、ルール出力全体を送信してしまう問題を修正しました。

  以前の（誤った）動作に依存していた場合は、メッセージが期待通りにフォーマットされるようにペイロードテンプレートの更新が必要になる可能性があります。

## 5.10.1

- [#15752](https://github.com/emqx/emqx/pull/15752) リスナーの接続レート制限（`max_conn_rate` および `max_conn_burst`）は、アクター単位ではなくリスナー単位で適用されるようになり、5.9.0 以前の動作に戻りました。そのため、5.9.0、5.9.1、5.10.0 の設定は互換性がなく、指定されたレートはそれぞれのリスナーに設定されたアクター数に応じてスケールアップする必要があります。

## 5.10.0

- [#15289](https://github.com/emqx/emqx/pull/15289) すべてのコネクター、アクション、ソースに新しい `resource_opts.health_check_timeout` 設定を追加しました。デフォルト値は 60 秒です。ヘルスチェックがこの時間を超えて応答しない場合、コネクター／アクション／ソースは `disconnected` と見なされます。

  注意：デフォルトが 60 秒であるため、以前はそれ以上かかっても正常応答と見なされていたコネクター／アクション／ソースは、今後はそのような場合に切断状態と判定されます。

- [#15286](https://github.com/emqx/emqx/pull/15286) 設定オプション `broker.routing.storage_schema` は非推奨となり無視されます。従来の `v1` ルーティングストレージスキーマはサポートされなくなり、これを使用している古いバージョンのクラスターでは EMQX の起動が拒否されます。`v1` ルーティングスキーマを使用しているクラスターのアップグレード手順については、[EMQX 5.10 以降のローリングアップグレードの考慮事項](../get-started/deploy/rolling-upgrades.md#rolling-upgrade-considerations-for-emqx-5.10-or-later) を参照してください。

- [#15239](https://github.com/emqx/emqx/pull/15239) `multi_tenancy.default_max_sessions` の型は、`infinity` または正の整数のみとなりました。以前は `0` も受け入れられていました。

- [#15156](https://github.com/emqx/emqx/pull/15156) `dashboard.sso.oidc.issuer` フィールドにスキーマ検証を追加しました。この値は有効な URL であることがチェックされます。
