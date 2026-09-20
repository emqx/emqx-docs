# EMQX 5.x から EMQX 6.0 への非互換変更点

## 廃止されたパッケージ

- [#15939](https://github.com/emqx/emqx/pull/15939) サポート終了済みのシステム向けパッケージのリリースを停止しました：
  - Debian 10 (Buster)
  - Enterprise Linux (CentOS) 7
  - Ubuntu 18.04
  - Ubuntu 20.04
  - macOS 13 (Ventura)

- [#16050](https://github.com/emqx/emqx/pull/16050) Amazon Linux 2 向けパッケージのリリースを停止しました。Amazon Linux 2 は 2026年6月30日にサポート終了予定です。

## Durable Sessions

Durable Sessions 機能を以前に有効化していなかった場合は、このセクションは無視してかまいません。

EMQX 6.0 では、Durable Sessions とそのメッセージの内部表現が変更されました。  
Durable Sessions を有効にしていたバージョン 5.x のクラスターは、6.0 へのアップグレード時にクリーンな状態から再作成する必要があります。

詳細なアップグレード手順については、[ローリングアップグレードのドキュメント](../get-started/deploy/rolling-upgrades.md#emqx-enterprise-rolling-upgrade)をご参照ください。

- [#15496](https://github.com/emqx/emqx/pull/15496) Durable Sessions の状態管理が Mnesia から EMQX Durable Storage ベースの新しいデータベースに移行されました。
  - その結果、6.0.0 より前に作成されたすべての Durable Sessions の状態は移行時に失われます。
  - この変更により、Mnesia の限定的なトランザクション分離によるセッション状態の破損の可能性が解消されました（[#14039](https://github.com/emqx/emqx/issues/14039)参照）。
  - また、シャーディングと効率的なデータ表現により、Durable Sessions のパフォーマンスとスケーラビリティが向上しています。

## Will メッセージの動作

Durable Sessions の認可チェックは、クライアント切断時点で実施されるようになり、Will メッセージのパブリッシュ可否が判断されます。

これまでは、設定された `Will-Delay-Interval` の期限切れ後にチェックが行われていました。

## 設定変更

**Durable Sessions**

- `durable_storage.messages.n_sites` パラメータは `durable_storage.n_sites` に名称変更されました。このパラメータはすべての Durable Storage 共通となっています。
- `durable_storage.sessions` と `durable_storage.timers` が追加されました。
- [#15734](https://github.com/emqx/emqx/pull/15734) Durable Sessions の信頼性とスループットが改善されました。

**Durable Storage**

- `durable_storage.messages.n_sites` は `durable_storage.n_sites` に名称変更され、すべての Durable Storage タイプに適用されるようになりました。
- `durable_storage.sessions` と `durable_storage.timers` の新しい設定項目が追加されました。

**RocketMQ**

- [#15635](https://github.com/emqx/emqx/pull/15635) `parameters.strategy` フィールドは、これまで暗黙的に `key_dispatch` 戦略を意味していたキーのテンプレート指定を受け付けなくなりました。  
  代わりに、`parameters.strategy = key_dispatch` を明示的に設定し、キーのテンプレートは `parameters.key` で指定してください。

**プラットフォームサポート**

- [#15613](https://github.com/emqx/emqx/pull/15613) Debian 10 向けパッケージビルドを終了しました。

## レートリミット

- [#15743](https://github.com/emqx/emqx/pull/15743) リスナーの接続レート制限（`max_conn_rate` と `max_conn_burst`）は、アクセプター単位ではなくリスナー単位で適用されるようになり、5.9.0 以前の動作に戻りました。  
  そのため、バージョン 5.9.0、5.9.1、5.10.0 の設定は非互換となり、同じ実効制限を維持するには、各リスナーに設定されたアクセプター数に応じてレート値をスケールアップする必要があります。
