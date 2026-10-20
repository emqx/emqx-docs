# EMQX Operator のアップグレード

EMQX Operator 3.0 では `apps.emqx.io/v3beta1` API が導入されており、以前の API バージョンで作成された EMQX カスタムリソースを変換できません。そのため、Operator 2.3 から 3.0 への移行はインプレースアップグレードではなく、マイグレーションが必要です。

ライブマイグレーションのワークフローは EMQX Operator 3.1 でサポート予定です。

## Operator 2.3 から 3.0 へのマイグレーション

既存クラスターのバックアップ、マニフェストの変換、Operator 3.0 下での新しいクラスターのデプロイ、クライアントトラフィックの切り替えについては、[Migrate from EMQX Operator 2.3 to 3.0](./migration/migrate-from-2.3-to-3.0.md) を参照してください。

## Operator 2.2 から 2.3 へのアップグレード

2.3 リリースラインにとどまる必要がある場合は、EMQX Operator 2.3 のドキュメントを使用してください。Operator 3.0 は `v3beta1` より前の EMQX API バージョンをサポートしていません。
