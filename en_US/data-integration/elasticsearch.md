# Ingest MQTT Data into Elasticsearch

{% emqxce %}

::: tip

EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.

:::

{% endemqxce %}

[Elasticsearch](https://www.elastic.co/elasticsearch/) is a distributed, RESTful style search and data analysis engine that offers full-text search, structured search, and analysis capabilities for diverse data types. By integrating with Elasticsearch, EMQX enables seamless incorporation of MQTT data into Elasticsearch for storage. This integration leverages the powerful scalability and analysis capabilities of Elasticsearch, providing efficient and scalable data storage and analysis solutions for IoT applications.

This page details the data integration between EMQX and Elasticsearch and provides practical guidance on rule and Sink creation.

## How It Works

Data integration with Elasticsearch is an out-of-the-box feature in EMQX, combining EMQX's device access, message transmission capabilities with Elasticsearch’s data storage and analysis capabilities. Seamless integration of MQTT data can be achieved through simple configuration.

EMQX and Elasticsearch provide a scalable IoT platform for efficiently collecting and analyzing real-time device data. In this architecture, EMQX acts as the IoT platform, responsible for device access, message transmission, and data routing, while Elasticsearch serves as the data storage and analysis platform, handling data storage, data search, and analysis.

EMQX forwards device data to Elasticsearch through its rule engine and Sink, where Elasticsearch utilizes its powerful search and analysis capabilities to generate reports, charts, and other data analysis results, displayed to users through Kibana’s visualization tools. The workflow is as follows:

1. **Device Message Publishing and Receiving**: IoT devices connect via the MQTT protocol and publish telemetry and status data to specific topics, which EMQX receives and compares in the rule engine.
2. **Rule Engine Processes Messages**: Using the built-in rule engine, MQTT messages from specific sources can be processed based on topic matching. The rule engine matches corresponding rules and processes messages, such as transforming data formats, filtering out specific information, or enriching messages with context information.
3. **Writing to Elasticsearch**: Rules defined in the rule engine trigger the operation of writing messages to Elasticsearch. Elasticsearch Sink provides flexible operation methods and document templates to construct documents in the desired format, writing specific fields from messages into corresponding indices in Elasticsearch.

Once device data is written to Elasticsearch, you can flexibly use Elasticsearch's search and analysis capabilities to process data, such as:

1. **Log Monitoring**: IoT devices generate a large amount of log data, which can be sent to Elasticsearch for storage and analysis. By connecting to visualization tools, such as Kibana, charts can be generated based on these log data, displaying real-time information on device status, operation records, and error messages. This helps developers or operators quickly locate and resolve potential issues.
2. **Geographical Data (Maps)**: IoT devices often generate geographic location data, which can be stored in Elasticsearch. Using Kibana’s Maps feature, device location information can be visualized on a map for tracking and analysis.
3. **Endpoint Security**: Security log data from IoT devices can be sent to Elasticsearch. By connecting to Elastic Security, security reports can be generated, monitoring the security status of devices in real time, detecting potential security threats, and responding accordingly.

## Features and Advantages

The Elasticsearch data integration offers the following features and advantages:



## Before you Start

This section introduces the preparatory work needed before creating Elasticsearch data integration in EMQX, including installing Elasticsearch and creating indices.

### Prerequisites

- Understand [rules](https://chat.openai.com/c/rules.md).
- Understand [data integration](https://chat.openai.com/c/data-bridges.md).

### Install Elasticsearch and Create Indices

EMQX supports integration with privately deployed Elasticsearch or with Elastic in the cloud. You can use Elastic Cloud or Docker to deploy an Elasticsearch instance.

1. If you don't have a Docker environment, [install Docker](https://docs.docker.com/install/).

2. Start the Elasticsearch container, setting the initial password to `public`.

   ```bash
   docker run -d --name elasticsearch \
       -p 9200:9200 \
       -p 9300:9300 \
       -e "discovery.type=single-node" \
       -e "ELASTIC_PASSWORD=public" \
       docker.elastic.co/elasticsearch/elasticsearch:7.10.1
   ```

3. Create the `device_data` index for storing messages published by devices.

   ```bash
   curl -X PUT "localhost:9200/device_data?pretty" -H 'Content-Type: application/json' -d'
   {
     "mappings": {
       "properties": {
         "ts": { "type": "date" },
         "clientid": { "type": "keyword" },
         "temperature": { "type": "float" },
         "humidity": { "type": "float" }
       }
     }
   }'
   ```

## Create a Connector

Before adding the Elasticsearch Sink, you need to create an Elasticsearch connector.

The following steps assume you are running EMQX and Elasticsearch on the same local machine. If you have EMQX and Elasticsearch running remotely, adjust the settings accordingly.

1. Go to the Dashboard **Integration** -> **Connectors** page.
2. Click **Create** in the upper right corner of the page.
3. Select **Elasticsearch** as the connector type and click next.
4. Enter the connector name, for example, `my-elasticsearch`. The name must combine uppercase and lowercase letters and numbers.
5. Enter Elasticsearch connection information according to your deployment method. If using Docker, <!-- TODO -->
6. Click the **Create** button at the bottom to complete the connector creation.

## Create a Rule for Elasticsearch Sink 

This section demonstrates how to create a rule in EMQX to process messages from the source MQTT topic `t/#` and write the processed results to the `device_data` index in Elasticsearch through the configured Sink.

1. Go to the Dashboard **Integration** -> **Rules** page.

2. Click **Create** in the upper right corner.

3. Enter rule ID `my_rule`, and in the SQL editor, enter the rule to store MQTT messages from the `t/#` topic in Elasticsearch. The rule SQL is as follows:

   ```sql
   SELECT
     clientid,
     ts,
     payload.temp as temp,
     payload.humidity as humidity
   FROM
       "t/#"
   ```

   ::: tip

   If you are new to SQL, you can click **SQL Examples** and **Enable Debugging** to learn and test the rule SQL results.

   :::

4. Click **Add Action**. Select `Elasticsearch` from the **Action Type** dropdown list. Keep the **Action** dropdown box as the default `Create Action` option. Or, you can select a previously created Elasticsearch action from the action dropdown box. This demonstration will create a new Sink and add it to the rule.

5. Enter the name and description of the Sink.

6. Select the `my-elasticsearch` connector you just created from the connector dropdown box. You can also click the button next to the dropdown box to create a new connector on the pop-up page. The required configuration parameters can be referred to [Create a Connector](#create-a-connector).

7. Configure the document template, inserting JSON-formatted data using the following template.

  <!-- TODO Waiting for development -->

8. Keep the rest of the parameters at their default values. 
9. Expand **Advanced Settings** and configure advanced settings options as needed (optional), for details refer to [Advanced Settings](https://chat.openai.com/c/49d81446-3d5c-437d-9dd0-f1097ff4ac64#advanced-settings).
10. Click the **Create** button to complete the creation of the Sink. The new Sink will be added to the **Action Outputs**.
11. Back on the Create Rule page, click the **Create** button to complete the entire rule creation.

Now you have successfully created the rule. You can see the newly created rule on the **Rules** page and the new Elasticsearch Sink under the **Actions (Sink)** tab.

You can also click **Integration** -> **Flow Designer** to view the topology. The topology visually shows that messages from the `t/#` topic are written to Elasticsearch after being parsed by the rule `my_rule`.

## Test the Rule

Use MQTTX to publish messages to the `t/1` topic, which will also trigger online/offline events:

```bash
mqttx pub -i emqx_c -t t/1 -m '{"temp":24,"humidity":30}'
```

Check the Sink operation statistics, both hit and successful send counts +1.

Use the `_search` API to view the document content in the index and check whether the data has been written to the `device_data` index:

```bash
curl -X GET "localhost:9200/device_data/_search?pretty"
```

<!-- TODO Waiting for development -->

## Advanced Settings

<!-- TODO -->
