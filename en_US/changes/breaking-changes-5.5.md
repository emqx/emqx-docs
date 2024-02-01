# Incompatible Changes in EMQX 5.5

## e5.5.0

{% emqxce %}

{% endemqxce %}

{% emqxee %}

- [#12283](https://github.com/emqx/emqx/pull/12283) Fixed the `resource_opts` configuration schema for the GCP PubSub Producer connector so that it contains only relevant fields.
  This affects the creation of GCP PubSub Producer connectors via HOCON configuration (`connectors.gcp_pubsub_producer.*.resource_opts`) and the HTTP APIs `POST /connectors` / `PUT /connectors/:id` for this particular connector type.

{% endemqxee %}
