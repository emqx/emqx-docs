# EMQX 5.x と EMQX 6.0 の非互換な変更点

## 廃止されたパッケージ

- [#15939](https://github.com/emqx/emqx/pull/15939) サポート終了となったシステム向けのパッケージ提供を停止しました:
  - Debian 10 (Buster)
  - Enterprise Linux (CentOS) 7
  - Ubuntu 18.04
  - Ubuntu 20.04
  - macOS 13 (Ventura)

- [#16050](https://github.com/emqx/emqx/pull/16050) Amazon Linux 2 向けのパッケージ提供を停止しました。Amazon Linux 2 は 2026年6月30日にサポート終了となります。

## 永続セッション

永続セッション機能を有効にしていない場合、このセクションは無視できます。

EMQX 6.0 では、永続セッションおよびそのメッセージの内部表現が変更されました。
5.x で永続セッションを有効にして運用していたクラスタは、6.0 へアップグレードする際にクリーンな状態から再構築する必要があります。

詳細なアップグレード手順は [ローリングアップグレードのドキュメント](../deploy/rolling-upgrades.md#emqx-enterprise-rolling-upgrade) を参照してください。

- [#15496](https://github.com/emqx/emqx/pull/15496) 永続セッションの状態は Mnesia から EMQX 永続ストレージ上の新しいデータベースへ移行されました。
  - そのため、6.0.0 より前に作成された永続セッションの状態は移行時に失われます。
  - この変更により、Mnesia のトランザクション分離制限によるセッション状態の破損（[#14039](https://github.com/emqx/emqx/issues/14039) 参照）が解消されます。
  - また、シャーディングと効率的なデータ表現により、永続セッションのパフォーマンスとスケーラビリティが向上します。

## Will メッセージの挙動

永続セッションの認可チェックは、クライアント切断時に行われ、Will メッセージの公開可否が判断されるようになりました。

以前は、設定された `Will-Delay-Interval` の経過後に認可チェックが行われていました。

## 設定変更

**永続セッション**

- `durable_storage.messages.n_sites` パラメータは `durable_storage.n_sites` に名称変更され、すべての永続ストレージで共通となりました。
- `durable_storage.sessions` および `durable_storage.timers` が追加されました。
- [#15734](https://github.com/emqx/emqx/pull/15734) 永続セッションの信頼性とスループットが向上しました。

**永続ストレージ**

- `durable_storage.messages.n_sites` は `durable_storage.n_sites` に名称変更され、すべての永続ストレージタイプに適用されます。
- `durable_storage.sessions` および `durable_storage.timers` の新しい設定項目が追加されました。

**RocketMQ**

- [#15635](https://github.com/emqx/emqx/pull/15635) `parameters.strategy` フィールドはキー・テンプレート（以前は `key_dispatch` 戦略を暗示）を受け付けなくなりました。
  代わりに、`parameters.strategy = key_dispatch` を明示的に設定し、キー・テンプレートは `parameters.key` で指定してください。

**プラットフォームサポート**

- [#15613](https://github.com/emqx/emqx/pull/15613) Debian 10 向けのパッケージビルドを終了しました。

## レート制限

- [#15743](https://github.com/emqx/emqx/pull/15743) リスナーの接続レート制限（`max_conn_rate` および `max_conn_burst`）は、アクセプタ単位ではなくリスナー単位で適用されるようになり、5.9.0 以前の挙動に戻りました。そのため、5.9.0、5.9.1、5.10.0 の設定は非互換となります：同じ制限値を維持するには、各リスナーで設定したアクセプタ数分だけレート値を増やす必要があります。
