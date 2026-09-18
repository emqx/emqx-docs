# データ統合

ダッシュボードのデータ統合セクションでは、ルールの作成、さまざまなデータシステムとのデータ統合、スキーマ検証、メッセージ変換の機能を提供しています。また、設定の編集や機能の統計表示などの管理操作も行えます。サブセクションは以下の通りです。

- **Webhook**: HTTPサービスと統合したいが、データ処理にルールを使う必要がないユーザー向けに、より簡単で使いやすい作成および設定体験を提供します。詳細なステップバイステップガイドは[Webhook](../../develop/data-integration/webhook.md)をご覧ください。
- **Flowデザイナー（EMQX Enterprise機能）**: データ処理と統合を視覚的に確認・管理できる強力なツールです。詳細な紹介は[Flow Designer](../../develop/flow-designer/introduction.md)をご覧ください。
- [**ルール**](./rules.md): ルールの作成、ルールのテスト、ルールへのアクション追加、ルール実行の統計表示に関するすべての機能を提供します。
- **コネクター**: データ統合に欠かせないコンポーネントで、外部データシステムとの接続に使用します。コネクターの作成と管理方法は[Connector](../../develop/data-integration/connector.md)をご覧ください。
- **スキーマ検証（EMQX Enterprise機能）**: 特定のトピックにパブリッシュされるデータが事前定義されたデータ形式に準拠していることを検証するためのルールを使用します。検証ルールの作成方法は[ダッシュボードでのスキーマ検証の設定](../../develop/data-integration/schema-validation.md#configure-schema-validation-in-dashboard)をご覧ください。
- **スキーマ（EMQX Enterprise機能）**: スキーマ検証やSQLルールで使用するスキーマを作成できます。ダッシュボード上でのスキーマ作成方法は[スキーマレジストリの例 - Avro](../../develop/data-integration/schema-registry-example-avro.md)または[スキーマレジストリの例 - Protobuf](../../develop/data-integration/schema-registry-example-protobuf.md)をご覧ください。
- **メッセージ変換（EMQX Enterprise機能）**: システムを流れるデータに適応させるための変換式を定義します。変換の作成方法は[ダッシュボードでのメッセージ変換の設定](../../develop/data-integration/message-transformation.md#configure-message-transformation-in-dashboard)をご覧ください。
