# Ingest MQTT Data into GCP Pub/Sub

EMQX supports seamless integration with [Google Cloud Pub/Sub](https://cloud.google.com/pubsub?hl=en-us) for real-time extraction, processing, and analysis of MQTT data. Additionally, it enables data push and subscription to various Google Cloud services such as Cloud Functions, App Engine, Cloud Run, Kubernetes Engine, and Compute Engine.

EMQX GCP Pub/Sub integration allows you to send MQTT messages and events to GCP Pub/Sub, which can help you flexibly choose services on Google Cloud and build IoT applications more easily.

{% emqxce %}
:::tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}

:::tip Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridge](./data-bridges.md)

:::

## Feature List

- [Connection pool](./data-bridges.md#connection-pool)
- [Async mode](./data-bridges.md#async-mode)
- [Batch mode](./data-bridges.md#batch-mode)
- [Buffer queue](./data-bridges.md#buffer-queue)

## Quick Start Tutorial

This section introduces how to configure the GCP Pub/Sub data bridge, including how to set up the GCP service, create data bridges and rules for forwarding data to GCP and test the data bridges and rules.

### Create Service Account Key in GCP

You need to create a service account and a service account key to use the GCP PubSub service.

1. Create a [Service Account](https://developers.google.com/identity/protocols/oauth2/service-account#creatinganaccount) in your GCP account.  Ensure that the Service Account has permission to at least publish messages to the topic of interest.

2. Click the email address for the service account you created. Click the **Key** tab. In the **Add key** drop-down list, select **Create new key** to create a Service Account key for that account and download it in JSON format.

   ::: tip

   Store the Service Account key securely for later use.

   :::

   <img src="./assets/gcp_pubsub/service-account-key.png" alt="service-account-key" style="zoom:50%;" />

### Create and Manage Topics in GCP

Before configuring the GCP Pub/Sub Bridge on EMQX, you need to create a topic and be familiar with the basic management operation in GCP.

1. In the Google Cloud console, go to the **Pub/Sub** ->**Topics** page. For detailed instructions, see [Create and manage topics](https://cloud.google.com/pubsub/docs/create-topic).

   ::: tip

   The Service Account must have permission to publish that topic.

   :::

2. In the **Topic ID** field, enter an ID for your topic. Click **Create topic**.

   <img src="./assets/gcp_pubsub/create-topic-GCP-console.png" alt="create-topic-GCP-console" style="zoom:50%;" />

3. Go to the **Subscriptions** page. Click the **Topic ID** in the list. Create a subscription to the topic.

   - Select **Pull** in **Delivery type**.
   - Select `7` Days for **Message retention duration**.

   For detailed instructions, see [GCP Pub/Sub Subscription](https://cloud.google.com/pubsub/docs/subscriber).

   <img src="./assets/gcp_pubsub/add-subscription-to-topic.png" alt="add-subscription-to-topic" style="zoom:50%;" />

4. Click **Subscription ID** -> **Messages** -> **Pull** can view the message sent to the topic.

   <img src="./assets/gcp_pubsub/subscriptions-id.png" alt="subscriptions-id" style="zoom:50%;" />

   <img src="./assets/gcp_pubsub/subscriptions-id-pull.png" alt="subscriptions-id-pull" style="zoom:50%;" />

### Create a GCP Pub/Sub Bridge

1. Go to EMQX Dashboard, click **Integration** -> **Data Bridge**.

2. Click **Create** on the top right corner of the page.

3. In the **Create Data Bridge** page, click to select **Google PubSub**, and then click **Next**.

4. In the **Name** field, enter a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.

5. In the **Bridge Role** field, select `Producer` or `Consumer` from the drop-down list according to your business needs and complete the corresponding configurations. 

   :::: tabs type:card
   
   ::: tab Configure as Producer Role

   - **GCP PubSub Topic**: Enter the topic ID `my-iot-core` you created in [Create and Manage Topic in GCP](#create-and-manage-topic-in-gcp).
   - **GCP Service Account Credentials**: Upload the Service Account credentials in JSON format you exported in [Create Service Account Key in GCP](#create-service-account-key-in-gcp).
   - **Payload Template**: Leave it blank or define a template.
      - If left blank, it will encode all visible inputs from the MQTT message using JSON format, such as clientid, topic, payload, etc.
      - If using the defined template, placeholders of the form `${variable_name}` will be filled with the corresponding value from the MQTT context.  For example, `${topic}` will be replaced with `my/topic` if such is the MQTT message topic.
   - **Attributes Template** and **Ordering Key Template** (optional): Similarly, you can define templates for formatting the attributes and/or ordering key of the outgoing message. 
      - For **Attributes**, both keys and values may use placeholders of the form `${variable_name}`.  Such values will be extracted from the MQTT context.  If a key template resolves to an empty string, that key is omitted from the outgoing message to GCP PubSub.
      - For **Ordering Key**, placeholders of the form `${variable_name}` may be used.  If the resolved value is an empty string, the `orderingKey` field will not be set for the GCP PubSub outgoing message.
   - Advanced settings (optional):  Choose whether to use **sync** or **async** query mode as needed.

   :::

   ::: tab Configure as Consumer Role

   - In the **GCP Service Account Credentials** field, upload the Service Account credentials in JSON format you exported in [Create Service Account Key in GCP](#create-service-account-key-in-gcp).

   - The **Topic Mapping** field must contain at least one GCP PubSub-to-MQTT topic mapping. The **MQTT Payload Template** subfield specifies the MQTT payload that should be used, and has the following GCP PubSub message fields available for templating:

     | Field Name        | Description                                                    |
     |-------------------|----------------------------------------------------------------|
     | `attributes`      | (Optional) An object containing string key-value pairs, if any |
     | `message_id`      | The message ID assigned by GCP PubSub to this message          |
     | `ordering_key`    | (Optional) The message ordering key, if any                    |
     | `publishing_time` | Message timestamp as defined by GCP PubSub                     |
     | `topic`           | Originating GCP PubSub topic                                   |
     | `value`           | (Optional) The message payload, if present                     |

     The default value for **MQTT Payload Template** is `${.}`, which includes all available data encoded as a JSON object.  For example, choosing `${.}` as a template will produce the following for a GCP PubSub message containing all optional fields:

     ```json
     {
       "attributes": {"attribute_key": "attribute_value"},
       "message_id": "1679665968238",
       "ordering_key": "my-ordering-key",
       "topic": "my-pubsub-topic",
       "publishing_time": "2023-08-18T14:15:18.470Z",
       "value": "my payload"
     }
     ```

     Subfields from the GCP PubSub message may be accessed with dot notation. For example, `${.value}` will resolve to the GCP PubSub message value, and `${.attributes.h1}` will resolve to the value of the `h1` message attribute key if such a subfield exists.  Absent values will be replaced by empty strings.

     **Note**: Each GCP PubSub-to-MQTT topic mapping must contain a unique GCP PubSub topic name.  That is, the GCP PubSub topic must not be present in more than one mapping.

   - Leave other options as default.

   :::

   ::::

6. Before clicking **Create**, you can click **Test Connectivity** to test that the bridge can connect to the GCP PubSub server.

7. Click **Create** to finish the creation of the data bridge.

    A confirmation dialog will appear and ask if you like to create a rule using this data bridge, you can click **Create Rule** to continue creating rules to specify the data to be saved into GCP PubSub. You can also create rules by following the steps in [Create Rules for GCP PubSub Data Bridge](#create-rules-for-GCP-PubSub-data-bridge).

### Create a Rule for GCP PubSub Producer Data Bridge

You can continue to create rules to specify the data to be saved into GCP PubSub.

1. Go to EMQX Dashboard, click **Integration** -> **Rules**.

2. Click **Create** on the top right corner of the page.

3. Input `my_rule` as the rule ID.

3. Set the rules in the **SQL Editor**. Here we want to save the MQTT messages under topic `/devices/+/events`  to GCP PubSub, we can use the SQL syntax below.

   Note: If you want to specify your own SQL syntax, make sure that the `SELECT` part includes all fields required by the payload template in the data bridge.

   ```sql
   SELECT
     *
   FROM
     "/devices/+/events"
   ```

5. Click the **Add Action** button, select **Forwarding with Data Bridge** from the dropdown list, and then select the data bridge you just created under **Data Bridge**. Then click the **Add** button.

4. Click **Create** at the page bottom to finish the creation.

Now a rule to forward data to GCP PubSub via a GCP PubSub bridge is created. You can click **Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `/devices/+/events` are sent and saved to GCP PubSub after parsing by rule `my_rule`.

### Test the Data Bridge and Rule

1. Use MQTTX to send messages on the topic `/devices/+/events`.

   ```bash
   mqttx pub -i emqx_c -t /devices/+/events -m '{ "msg": "hello GCP PubSub" }'
   ```

2. Check the running status of the data bridge, there should be one new incoming and one new outgoing message.

3. Go to GCP **Pub/Sub** -> **Subscriptions**, click **MESSAGES** tab. You should see the message.
