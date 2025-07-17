# Ingest MQTT Data into BigQuery

[BigQuery](https://cloud.google.com/bigquery?hl=en) is an enterprise data warehouse for large amounts of relational structured data. It is optimized for large-scale, ad-hoc SQL-based analysis and reporting, which makes it best suited for gaining organizational insights. EMQX supports seamless integration with BigQuery for real-time extraction, processing, and analysis of MQTT data.

This page provides a comprehensive introduction to the data integration between EMQX and BigQuery with practical instructions on creating and validating the data integration.

## How It Works

BigQuery data integration is an out-of-the-box feature of EMQX designed to help users seamlessly integrate MQTT data streams with Google Cloud and leverage its rich services and capabilities for IoT application development.

EMQX forwards MQTT data to BigQuery through the rule engine and Sink. Taking the example of a BigQuery producer role, the complete process is as follows:

1. **IoT Devices Publish Messages**: Devices publish telemetry and status data through specific topics, triggering the rule engine.
2. **Rule Engine Processes Messages**: Using the built-in rule engine, MQTT messages from specific sources are processed based on topic matching. The rule engine matches corresponding rules and processes messages, such as converting data formats, filtering specific information, or enriching messages with contextual information.
3. **Bridging to BigQuery**: The rule triggers the action of forwarding messages to BigQuery, allowing easy configuration of data properties, ordering keys, and mapping of MQTT topics to BigQuery topics. This provides richer context information and order assurance for data integration, enabling flexible IoT data processing.

## Features and Benefits


## Before You Start

This section describes the preparations you need to complete before you start to create the BigQuery data integration.

### Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [Data Integration](./data-bridges.md)

### Create Service Account Key in GCP

You need to create a service account and a service account key to use the BigQuery service.

1. Create a [Service Account](https://developers.google.com/identity/protocols/oauth2/service-account#creatinganaccount) in your GCP account.  Ensure that the Service Account has permission to read and write the datasets and tables used in your Actions (e.g. "BigQuery Data Editor" for the datasets/tables involved, or at least read/write over their data).

2. Click the email address for the service account you created. Click the **Key** tab. In the **Add key** drop-down list, select **Create new key** to create a Service Account key for that account and download it in JSON format.

   ::: tip

   Store the Service Account key securely for later use.

   :::

   <img src="./assets/gcp_pubsub/service-account-key.png" alt="service-account-key" style="zoom:50%;" />

### Create and Manage Datasets and Tables in GCP

Before configuring the BigQuery data integration on EMQX, you need to create a topic and be familiar with the basic management operation in GCP.

1. In the Google Cloud console, go to the **BigQuery** -> **Studio** page. For detailed instructions, see [Load and query data](https://cloud.google.com/bigquery/docs/quickstarts/load-data-console) tutorial for some hints on getting started.

   ::: tip

   The Service Account must have permission to write to the table in the dataset.

   :::

2. In the **Explorer** pane, click the kebab icon (⋮), then Create Dataset.  Define a name for your dataset.  Click Create Dataset.

3. In the **Explorer** pane, click the recently created dataset, then (+) Create Table.  Set the source as "Empty Table", define a name for it, define a schema for it (for example, choosing to edit it as text, `clientid:string,payload:bytes,topic:string,publish_received_at:timestamp`).  Click Create Table.

3. Click the dataset, then click Share.  Add your Service Account Email as the principal, and assign it an a role that has read and write access to the table, and read access for the dataset.  For example, the principal may have "BigQuery Data Viewer" on the dataset, and "Editor" on the table that will be used.

4. Click the table, then Query.  You can query its data by using SQL statements.

```
SELECT * FROM `my_project.my_dataset.my_tab` LIMIT 1000
```

## Create a BigQuery Producer Connector

Before adding a BigQuery Producer Sink action, you need to create a BigQuery Producer connector to establish a connection between EMQX and BigQuery.

1. Go to the EMQX Dashboard and click **Integration** -> **Connector**.
2. Click **Create** in the top right corner of the page, select **BigQuery** on the connector selection page, and click **Next**.
3. Enter a name and description, such as `my_producer`. The name is used to associate the BigQuery Producer Sink with the connector and must be unique within the cluster.
4. In **GCP Service Account Credentials**, upload the Service Account credentials in JSON format you exported in [Create Service Account Key in GCP](#create-service-account-key-in-gcp).
5. Before clicking **Create**, you can click **Test Connectivity** to test if the connector can connect to the BigQuery server.
6. Click the **Create** button at the bottom to complete the creation of the connector. In the pop-up dialog, you can click **Back to Connector List** or click **Create Rule** to continue creating a rule with Sink to specify the data to be forwarded to BigQuery. For detailed steps, see [Create a Rule with BigQuery Producer Sink](#create-a-rule-with-bigquery-producer-sink).

## Create a Rule with BigQuery Producer Sink

This section demonstrates how to create a rule to specify the data to be saved into BigQuery.

1. Go to EMQX Dashboard, and click **Integration** -> **Rules**.

2. Click **Create** on the top right corner of the page.

3. Enter `my_rule` as the rule ID.

4. Set the rules in the **SQL Editor**. Here if you want to save the MQTT messages under topic `t/bq`  to BigQuery, you can use the SQL syntax below.

   Note: If you want to specify your own SQL syntax, make sure that the `SELECT` part includes all fields required by the payload template in the Sink.

   ```sql
   SELECT
     clientid,
     topic,
     payload,
     publish_received_at
   FROM
     "t/bq"
   ```

   ::: tip
   Be sure to select only the fields that are columns in your BigQuery table, otherwise BigQuery will not recognize unknown fields.
   :::

   Note: If you are a beginner user, click **SQL Examples** and **Enable Test** to learn and test the SQL rule.

5. Click the **+ Add Action** button to define an action that will be triggered by the rule. Select `BigQuery` from the **Type of Action** dropdown list so that EMQX will send the data processed by the rule to BigQuery.

6. Keep the **Action** dropdown box with the value `Create Action`. Or, you also can select a BigQuery Producer Sink previously created. In this demonstration, you create a new Sink and add it to the rule.

7. In the **Name** field, enter a name for the Sink. The name should be a combination of upper/lower case letters and numbers.

8. Select the `my_producer` just created from the **Connector** dropdown box. You can also create a new Connector by clicking the button next to the dropdown box. For the configuration parameters, see [Create a Connector](#create-a-connector).

9. In **Dataset** and **Table**, enter the dataset and table names you created in [Create and Manage Datasets and Tables in GCP](#create-and-manage-datasets-and-tables-in-gcp), respectively.

12. **Fallback Actions (Optional)**: If you want to improve reliability in case of message delivery failure, you can define one or more fallback actions. These actions will be triggered if the primary Sink fails to process a message. See [Fallback Actions](./data-bridges.md#fallback-actions) for more details.

13. **Advanced settings (optional)**:  For details, see [Features of Sink](./data-bridges.md#features-of-sink).

14. Before clicking **Create**, you can click **Test Connectivity** to test that the Connector can connect to the BigQuery server.

15. Click the **Create** button to complete the Sink configuration and you will see the new Sink appear under the **Action Outputs** tab.

16. Back on the **Create Rule** page, click **Create** to create the rule.

You have now successfully created the rule. You can see the newly created rule on the **Integration** -> **Rules** page. Click the **Actions(Sink)** tab and you can see the new Google PubSub Producer Sink.

You can also click **Integration** -> **Flow Designer** to view the topology and you can that the messages under topic `t/bq` are sent and saved to BigQuery after parsing by rule `my_rule`.

## Test the Producer Rule

1. Use MQTTX to send messages on the topic `t/bq`.

   ```bash
   mqttx pub -i emqx_c -t t/bq -m '{ "msg": "hello BigQuery" }'
   ```

2. Check the running status of the Sink, there should be one new incoming and one new outgoing message.

3. Go to GCP **BigQuery** -> **Studio**, click your table, then Query, and run a query. You should see the message.
