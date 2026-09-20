# EMQX 5.5 における互換性のない変更点

## e5.5.0

- EMQXのMQTTデータブリッジ管理をリファクタリングし、より柔軟かつ効率的な管理手法を提供するために分割しました。これまで `/bridges` APIで管理されていた操作は、`/connectors`、`/actions`、および`/sources`の3つの別々のAPIに分割されました。この分割により、MQTTデータブリッジの各コンポーネントをより細かく制御・管理できるようになり、システムの柔軟性と使いやすさが向上しています。

  旧バージョンからの設定については、[Bridge with Other MQTT Services](../develop/data-integration/data-bridge-mqtt.md) の設定手順に従い、手動で移行する必要があります。この移行作業には時間と労力がかかる場合がありますが、完了後は新バージョンによる改善の恩恵を受けることができます。

- [#12283](https://github.com/emqx/emqx/pull/12283) GCP PubSub Producerコネクターの`resource_opts`設定スキーマを修正し、関連するフィールドのみを含むようにしました。
  これにより、HOCON設定（`connectors.gcp_pubsub_producer.*.resource_opts`）およびこの特定のコネクタータイプに対するHTTP APIの`POST /connectors`、`PUT /connectors/:id`でのGCP PubSub Producerコネクター作成に影響があります。
