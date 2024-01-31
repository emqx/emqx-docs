# EMQX 5.5 中的不兼容变更

## e5.5.0

{% emqxce %}

{% endemqxce %}

{% emqxee %}

- [#12283](https://github.com/emqx/emqx/pull/12283) 修复了 GCP PubSub 生产者连接器的 `resource_opts` 配置模式，使其仅包含相关字段。 这影响了通过 HOCON 配置（`connectors.gcp_pubsub_producer.*.resource_opts`）以及针对这种特定连接器类型的 HTTP API `POST /connectors` / `PUT /connectors/:id` 创建 GCP PubSub 生产者连接器。

{% endemqxee %}
