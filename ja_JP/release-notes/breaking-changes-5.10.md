# EMQX 5.10 の非互換変更点

## 5.10.4

- [#17244](https://github.com/emqx/emqx/pull/17244) ホットアップグレードのREST APIエンドポイント（`/api/v5/relup/*`）を削除しました。ホットアップグレードは現在、各ノード上の `emqx ctl relup` CLIを通じてのみ操作され、ダッシュボード上の操作はなくなりました。

  対象のリリースtarballとその `.sha256` サイドカー（同じベース名、同じディレクトリ）をEMQXプロセスが読み取れる場所に配置してください。各ノードで `emqx ctl relup upgrade <TarballPath>` を実行してアップグレードを適用します。対象バージョンはtarball内の `releases/emqx_vars`（`REL_VSN`）から読み取られます。

## 5.10.3

- [#16491](https://github.com/emqx/emqx/pull/16491) macOS 13（Ventura）向けパッケージのリリースを停止しました。

## 5.10.2

- [#16062](https://github.com/emqx/emqx/pull/16062) RocketMQアクションが設定されたペイロードテンプレートを無視し、ルール出力全体を送信してしまう問題を修正しました。

  以前の（誤った）動作に依存していた場合は、メッセージが期待通りにフォーマットされるようにペイロードテンプレートを更新する必要があります。

## 5.10.1

- [#15752](https://github.com/emqx/emqx/pull/15752) リスナーの接続レート制限（`max_conn_rate` と `max_conn_burst`）は、アクセプター単位ではなくリスナー単位で適用されるようになり、5.9.0以前の動作に戻りました。そのため、5.9.0、5.9.1、5.10.0の設定は非互換となり、指定されたレートは各リスナーに設定されたアクセプター数に応じてスケールアップする必要があります。

## 5.10.0

- [#15289](https://github.com/emqx/emqx/pull/15289) すべてのコネクター、アクション、ソースに新しい設定 `resource_opts.health_check_timeout` を追加しました。デフォルト値は60秒です。ヘルスチェックの応答がこの時間を超えると、コネクター／アクション／ソースは `disconnected` と見なされます。

  注意：デフォルトが60秒のため、以前は60秒以上かかって正常応答を返していた場合、今回からはその状況で切断と判断されます。

- [#15286](https://github.com/emqx/emqx/pull/15286) 設定オプション `broker.routing.storage_schema` は非推奨となり無視されます。旧式の `v1` ルーティングストレージスキーマはサポートされなくなり、これを使用している古いバージョンのクラスターではEMQXは起動を拒否します。`v1` ルーティングスキーマを使用しているクラスターのアップグレード手順は、[EMQX 5.10以降のローリングアップグレードの考慮事項](../get-started/deploy/rolling-upgrades.md#rolling-upgrade-considerations-for-emqx-5.10-or-later) を参照してください。

- [#15239](https://github.com/emqx/emqx/pull/15239) `multi_tenancy.default_max_sessions` の型は、これまで `0` も受け入れていましたが、現在は `infinity` または正の整数のみとなりました。

- [#15156](https://github.com/emqx/emqx/pull/15156) `dashboard.sso.oidc.issuer` フィールドにスキーマ検証を追加しました。現在、この値は有効なURLであることがチェックされます。
