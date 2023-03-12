# GCP Pub/Sub Bridge

EMQX GCP Pub/Sub integration can send MQTT messages and events to [Google Cloud Pub/Sub](https://cloud.google.com/pubsub?hl=en-us), which can help you flexibly choose services on Google Cloud and build IoT
applications more easily.

{% emqxce %}
:::tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}

The following steps will guide you to use this configuration.

## Setup

A few configuration steps on GCP's side must be performed before
configuring the GCP Pub/Sub Bridge on EMQX.

1. Create a [Service
   Account](https://developers.google.com/identity/protocols/oauth2/service-account#creatinganaccount)
   in your GCP account.  Ensure that the Service Account has
   permissions to at least publish messages to the topic of interest.
2. Create a Service Account Key for that account and download it in
   JSON format.
3. Create a Pub/Sub topic (remember that the Service Account must have
   permissions to publish to that topic).

## Example GCP Pub/Sub Bridge Usage via Dashboard

Go to the EMQX Dashboard, select the _"Data Integration"_ item on the
menu on the left, then _"Data Bridges"_.  Then, click _"+ Create"_.

In the dialog, select the _Google Pub/Sub_ bridge type, click next.
Fill the required fields for that bridge (those are marked with an
asterisk).  The _payload template_ field, if left blank, will encode
all visible inputs from the MQTT message using JSON format, such as
clientid, topic, payload, etc.  Otherwise, it'll use the user defined
template, filling placeholders of the form `${variable_name}` with the
corresponding value from the MQTT context.  For example, `${topic}`
will be replaced with `my/topic` if such is the MQTT message topic.

In the _GCP Service Account Credentials_ field, upload the Service
Account credentials in JSON format you exported during the setup step.

![Creating a GCP PubSub Bridge](./assets/gcp_pubsub/setup1.png)

![Creating a GCP PubSub Bridge](./assets/gcp_pubsub/setup2.png)

![Creating a GCP PubSub Bridge](./assets/gcp_pubsub/setup3.png)

Finally, after clicking _"Create"_, you'll be offered the option of
creating an associated rule.  This will allow MQTT messages matching
the rule to be forwarded to GCP Pub/Sub.  Refer to the
[_Rules_](./rules.md) for more info on creating rules.

<center>
<img src="./assets/gcp_pubsub/setup4.png" alt="Creating a rule for GCP PubSub Bridge"></img>
</center>

## Example GCP Pub/Sub Bridge Usage via Config File

Add the following configuration to the end of the `emqx.conf` file,
substituting the contents of `service_account_json` with the
corresponding contents of your Service Account JSON file.

```js
bridges.gcp_pubsub.my_gcp_pubsub_bridge {
  connect_timeout = "15s"
  enable = true
  max_retries = 2
  pipelining = 100
  pool_size = 8
  pubsub_topic = "my-topic"
  request_timeout = "15s"
  resource_opts {
    async_inflight_window = 100
    auto_restart_interval = "60s"
    batch_size = 1
    batch_time = "20ms"
    health_check_interval = "15s"
    max_queue_bytes = "100MB"
    query_mode = "async"
    worker_pool_size = 16
  }
  service_account_json {
    "auth_provider_x509_cert_url" = "https://www.googleapis.com/oauth2/v1/certs"
    auth_uri = "https://accounts.google.com/o/oauth2/auth"
    client_email = "test-516@emqx-cloud-pubsub.iam.gserviceaccount.com"
    client_id = "000000000000000000000"
    "client_x509_cert_url" = "https://www.googleapis.com/robot/v1/metadata/x509/test-pubsub.iam.gserviceaccount.com"
    private_key = "-----BEGIN PRIVATE KEY-----\nMIIEvAIBA...\n-----END PRIVATE KEY-----\n"
    private_key_id = "0000000000000000000000000000000000000000"
    project_id = "my-project-id"
    token_uri = "https://oauth2.googleapis.com/token"
    type = "service_account"
  }
}
```
