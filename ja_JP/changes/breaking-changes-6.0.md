# EMQX 5.x と EMQX 6.0 の間の非互換の変更

## 非推奨のパッケージ

- [#15939](https://github.com/emqx/emqx/pull/15939) ライフサイクルが終了したシステム向けのパッケージのリリースを停止します：
  - Debian 10 (Buster)
  - Enterprise Linux (CentOS) 7
  - Ubuntu 20.04
  - macOS 13 (Ventura)

## 永続セッション

以前に永続セッション機能が有効でなかった場合、以下の情報は無視できます。

6.0 リリースでは、永続セッションとメッセージの内部表現が変更されます。
以前にクラスターがバージョン 5.x でこの機能を有効にして実行されていた場合、クリーンな状態から再作成する必要があります。
詳細なローリングアップグレード手順については、[ローリングアップグレードドキュメント](https://docs.emqx.com/ja/emqx/latest/deploy/rolling-upgrades.html#emqx-enterprise-rolling-upgrade) を参照してください。

- [#15496](https://github.com/emqx/emqx/pull/15496) 永続セッションの状態は Mnesia から EMQX の永続ストレージに基づく新しいデータベースに移動されました。
  その結果、6.0.0 リリースより前に作成された永続セッションの状態は、移動中に失われます。

  これにより、Mnesia のトランザクション分離が不十分なために発生する可能性があったセッション状態の破損の問題（[#14039](https://github.com/emqx/emqx/issues/14039) で報告）が解決されます。
  この変更により、シャーディングとより効率的なデータ表現のおかげで、永続セッションの全体的なパフォーマンスも向上します。


## 遺言メッセージの動作

永続セッションが遺言メッセージを公開する資格があるかどうかを決定する認可チェックは、クライアントの切断時に実行されるようになりました。
以前は、`Will-Delay-Interval` の有効期限が切れた後に実行されていました。

## 設定の変更

- `durable_sessions.heartbeat_interval` パラメータは `durable_sessions.checkpoint_interval` に名前が変更されました。

- `durable_sessions.idle_poll_interval` および `durable_sessions.renew_streams_interval` パラメータは、セッションが完全にイベントベースになったため削除されました。

- `durable_sessions.session_gc_interval` および `durable_sessions.session_gc_batch_size` パラメータは廃止されたため削除されました。

- `durable_storage.messages.n_sites` パラメータは `durable_storage.n_sites` に名前が変更されました。このパラメータはすべての永続ストレージで共通になりました。

- 新しい永続ストレージの設定を追加しました：`durable_storage.sessions` および `durable_storage.timers`。
- [#15613](https://github.com/emqx/emqx/pull/15613) Debian 10 向けのパッケージのリリースを停止しました。

- [#15635](https://github.com/emqx/emqx/pull/15635) RocketMQ アクションの `parameters.strategy` フィールドは、キーテンプレート（暗黙的に `key_dispatch` 戦略を選択）を受け付けなくなりました。
  代わりに、ユーザーは明示的に `parameters.strategy = key_dispatch` を設定し、`parameters.key` でキーテンプレートを提供する必要があります。

- [#15734](https://github.com/emqx/emqx/pull/15734) 永続セッションの信頼性とスループットを向上させました。

## レート制限

- [#15743](https://github.com/emqx/emqx/pull/15743) リスナー接続のレート制限（`max_conn_rate` および `max_conn_burst`）は、アクセプターごとではなくリスナーごとに適用されるようになり、5.9.0 より前の動作に戻りました。その結果、バージョン 5.9.0、5.9.1、および 5.10.0 の設定とは互換性がありません。指定されたレートは、それぞれのリスナーに設定されたアクセプターの数でスケールアップする必要があります。
