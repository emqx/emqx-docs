# EMQX 5.5 中的不兼容变更

## e5.5.0

{% emqxce %}

{% endemqxce %}

-  MQTT 数据桥接的管理方式进行了重构和拆分，以提供更灵活和高效的管理方式。
  原本通过 `/bridges` API 进行管理的操作现在被拆分到了 `/connectors`, `/actions` 和 `/sources` 这三个 API 中。这样的拆分能够更细粒度地控制和管理 MQTT 数据桥接的各个部分，提高了系统的灵活性和可用性。
  对于旧版本中的配置，用户需要按照 [与其他 MQTT 服务桥接](../data-integration/data-bridge-mqtt.md) 的指南重新配置进行手动迁移。这个迁移过程可能需要一些时间和精力，但是完成后用户可以享受到新版本优化带来的好处。

{% emqxee %}

- [#12283](https://github.com/emqx/emqx/pull/12283) 修复了 GCP PubSub 生产者连接器的 `resource_opts` 配置模式，使其仅包含相关字段。 这影响了通过 HOCON 配置（`connectors.gcp_pubsub_producer.*.resource_opts`）以及针对这种特定连接器类型的 HTTP API `POST /connectors` / `PUT /connectors/:id` 创建 GCP PubSub 生产者连接器。

{% endemqxee %}
