# EMQX 5.x から EMQX 6.0 への非互換変更点

## 廃止されたパッケージ

- [#15939](https://github.com/emqx/emqx/pull/15939) サポート終了済みのシステム向けパッケージのリリースを停止しました：
  - Debian 10 (Buster)
  - Enterprise Linux (CentOS) 7
  - Ubuntu 18.04
  - Ubuntu 20.04
  - macOS 13 (Ventura)

- [#16050](https://github.com/emqx/emqx/pull/16050) Amazon Linux 2 向けパッケージのリリースを停止しました。Amazon Linux 2 は2026年6月30日にサポート終了予定です。

## Durable Sessions（永続セッション）

以前に durable sessions 機能を有効にしていなかった場合は、このセクションは無視して構いません。

EMQX 6.0 では、durable sessions とそのメッセージの内部表現が変更されました。  
バージョン5.xで durable sessions を有効にして稼働していたクラスターは、6.0 へのアップグレード時にクリーンな状態から再作成する必要があります。

詳細なアップグレード手順は、[ローリングアップグレードのドキュメント](../get-started/deploy/rolling-upgrades.md#emqx-enterprise-rolling-upgrade)をご参照ください。

- [#15496](https://github.com/emqx/emqx/pull/15496) durable sessions の状態管理を Mnesia から EMQX durable storage ベースの新しいデータベースへ移行しました。
  - そのため、6.0.0 より前に作成されたすべての durable session 状態は移行時に失われます。
  - この変更により、Mnesia の限定的なトランザクション分離によるセッション状態の破損の可能性が解消されます（詳細は [#14039](https://github.com/emqx/emqx/issues/14039)）。
  - また、シャーディングとより効率的なデータ表現により、durable sessions のパフォーマンスとスケーラビリティが向上しています。

## Will メッセージの動作

durable sessions に対する認可チェックは、クライアント切断時に実施されるようになり、Will メッセージをパブリッシュしてよいか判断されます。

以前は、これらのチェックは設定された `Will-Delay-Interval` の期限切れ後に遅延して行われていました。

## 設定の変更点

**Durable Sessions**

- `durable_storage.messages.n_sites` パラメータは `durable_storage.n_sites` に名称変更されました。このパラメータはすべての durable storage 共通となっています。
- `durable_storage.sessions` と `durable_storage.timers` が新たに追加されました。
- [#15734](https://github.com/emqx/emqx/pull/15734) durable sessions の信頼性とスループットを改善しました。

**Durable Storage**

- `durable_storage.messages.n_sites` は `durable_storage.n_sites` に名称変更され、すべての durable storage タイプに適用されるようになりました。
- `durable_storage.sessions` と `durable_storage.timers` の新しい設定項目が追加されました。

**RocketMQ**

- [#15635](https://github.com/emqx/emqx/pull/15635) `parameters.strategy` フィールドは、これまで暗黙的に `key_dispatch` 戦略を意味していたキーのテンプレートを受け付けなくなりました。  
  代わりに、`parameters.strategy = key_dispatch` を明示的に設定し、キーのテンプレートは `parameters.key` に指定してください。

**プラットフォームサポート**

- [#15613](https://github.com/emqx/emqx/pull/15613) Debian 10 向けパッケージビルドを終了しました。

## レートリミット

- [#15743](https://github.com/emqx/emqx/pull/15743) リスナーの接続レート制限（`max_conn_rate` と `max_conn_burst`）は、アクター単位ではなくリスナー単位で適用されるようになり、5.9.0 より前の動作に戻りました。  
  そのため、5.9.0、5.9.1、5.10.0 の設定は互換性がなく、同じ実効制限を維持するには、各リスナーに設定されたアクター数分だけレート値をスケールアップする必要があります。
