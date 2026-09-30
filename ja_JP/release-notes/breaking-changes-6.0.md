# EMQX 6.0 の非互換変更点

## 6.0.4

- [#18515](https://github.com/emqx/emqx/pull/18515) Azure Blob Storage アクションの `blob` テンプレートフィールドは、Aggregated S3 の `key` と同じスキーマ検証を持つようになり、許可されたバインディングが遵守されていることを検証します。

- [#18528](https://github.com/emqx/emqx/pull/18528) OpenTelemetry 統合のエクスポーターエンドポイントは、スキームと明示的なポートを含む有効な URL である必要があります。サポートされるスキームは `http` と `https` です。ポートは明示的に設定しなければなりません。

- [#18974](https://github.com/emqx/emqx/pull/18974) `mqtt.max_connect_user_properties` を追加しました。これは MQTT v5 の CONNECT プロパティおよび Will プロパティで個別に受け入れられる User Property ペアの数を制限します。デフォルトは 100 で、制限を無効にするには `infinity` に設定してください。

## 6.0.3

- [#17157](https://github.com/emqx/emqx/pull/17157) ルールエンジンの設定 `rule_engine.limit_selects_in_namespace` を追加しました。デフォルトは `true` です。有効にすると、ネームスペースに属するルールは、同じネームスペース内のクライアントからのメッセージおよびクライアント関連イベントによってのみトリガーされます。

## 6.0.1

- [#16061](https://github.com/emqx/emqx/pull/16061) RocketMQ アクションが設定されたペイロードテンプレートを無視し、ルール出力全体を送信してしまう問題を修正しました。

  以前の（誤った）動作に依存していた場合は、メッセージが期待通りにフォーマットされるようにペイロードテンプレートを更新する必要があります。
