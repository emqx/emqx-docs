# EMQX 5.5 における互換性のない変更点

## e5.5.0

- EMQX における MQTT データブリッジ管理をリファクタリングし、より柔軟かつ効率的な管理手法を提供するために分割しました。これまで `/bridges` API で管理されていた操作は、`/connectors`、`/actions`、および `/sources` の3つの別々の API に分割されました。この分割により、MQTT データブリッジの各コンポーネントをより細かく制御・管理できるようになり、システムの柔軟性と使いやすさが向上しています。

  旧バージョンからの設定については、[Bridge with Other MQTT Services](../develop/data-integration/data-bridge-mqtt.md) の設定手順に従い、ユーザー自身で手動マイグレーションを行う必要があります。このマイグレーションには一定の時間と労力を要しますが、完了後は新バージョンによる改善の恩恵を受けることができます。

- [#12283](https://github.com/emqx/emqx/pull/12283) GCP PubSub Producer コネクターの `resource_opts` 設定スキーマを修正し、関連するフィールドのみを含むようにしました。  
  これにより、HOCON 設定（`connectors.gcp_pubsub_producer.*.resource_opts`）および HTTP API の `POST /connectors` / `PUT /connectors/:id` による該当コネクタータイプの作成に影響があります。
