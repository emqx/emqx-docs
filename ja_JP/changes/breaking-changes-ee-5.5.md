# EMQX 5.5 の非互換変更点

## e5.5.0

- EMQX における MQTT データブリッジ管理をリファクタリングし、より柔軟かつ効率的な管理手法を提供するために分割しました。これまで `/bridges` API で管理されていた操作は、`/connectors`、`/actions`、および `/sources` の3つの別々の API に分割されました。この分割により、MQTT データブリッジの各コンポーネントをより細かく制御・管理できるようになり、システムの柔軟性と使いやすさが向上しています。

  旧バージョンからの設定については、[Bridge with Other MQTT Services](../data-integration/data-bridge-mqtt.md) の設定手順に従って手動で移行する必要があります。この移行作業には時間と労力がかかる場合がありますが、完了すれば新バージョンの改善点を活用できます。

- [#12283](https://github.com/emqx/emqx/pull/12283) GCP PubSub Producer コネクターの `resource_opts` 設定スキーマを修正し、関連するフィールドのみを含むようにしました。  
  これにより、HOCON 設定（`connectors.gcp_pubsub_producer.*.resource_opts`）およびこのコネクタータイプに対する HTTP API の `POST /connectors` / `PUT /connectors/:id` による作成に影響があります。
