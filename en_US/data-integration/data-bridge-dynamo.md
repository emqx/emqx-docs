# Ingest MQTT Data into DynamoDB

{% emqxce %}
:::tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}

[DynamoDB](https://www.amazonaws.cn/en/dynamodb/) is a fully managed, high-performance, serverless key-value store database service on AWS. It is designed for applications that require fast, scalable, and reliable data storage. EMQX supports integration with DynamoDB, enabling you to save MQTT messages and client events to DynamoDB, facilitating the registration and management of IoT devices, as well as the long-term storage and real-time analysis of device data. Through the DynamoDB data bridge, MQTT messages and client events can be stored in DynamoDB, and events can also trigger updates or deletions of data within DynamoDB, thereby enabling the recording of information such as device online status and connection history.

This page provides a comprehensive introduction to the data integration between EMQX and DynamoDB with practical instructions on creating and validating the data integration.

## How It Works

DynamoDB data integration is an out-of-the-box feature in EMQX that combines EMQX's device connectivity and message transmission capabilities with DynamoDB's powerful data storage capabilities. With a built-in [rule engine](./rules.md) component, the integration simplifies the process of ingesting data from EMQX to DynamoDB for storage and management, eliminating the need for complex coding. 

The diagram below illustrates a typical architecture of data integration between EMQX and DynamoDB:

![EMQX Integration DynamoDB](./assets/emqx-integration-dynamodb.png)

Ingesting MQTT data into DynamoDB works as follows:

1. **Message publication and reception**: IoT devices, whether they are part of connected vehicles, IIoT systems, or energy management platforms, establish successful connections to EMQX through the MQTT protocol and publish MQTT messages to specific topics. When EMQX receives these messages, it initiates the matching process within its rules engine.
2. **Message data processing:** When a message arrives, it passes through the rule engine and is then processed by the rule defined in EMQX. The rules, based on predefined criteria, determine which messages need to be routed to DynamoDB. If any rules specify payload transformations, those transformations are applied, such as converting data formats, filtering out specific information, or enriching the payload with additional context.
3. **Data ingestion into DynamoDB**: Once the rule engine identifies a message for DynamoDB storage, it triggers an action of forwarding the messages to DynamoDB. Processed data will be seamlessly written into the collection of the DynamoDB database.
4. **Data storage and utilization**: With the data now stored in DynamoDB, businesses can harness its querying power for various use cases. For instance, in the realm of connected vehicles, this stored data can inform fleet management systems about vehicle health, optimize route planning based on real-time metrics, or track assets. Similarly, in IIoT settings, the data might be used to monitor machinery health, forecast maintenance, or optimize production schedules.

## Features and Benefits

The data integration with DynamoDB offers a range of features and benefits tailored to ensure efficient data transmission, storage, and utilization:

- **Real-time Data Streaming**: EMQX is built for handling real-time data streams, ensuring efficient and reliable data transmission from source systems to DynamoDB. It enables organizations to capture and analyze data in real-time, making it ideal for use cases requiring immediate insights and actions.
- **Flexibility in Data Transformation:** EMQX provides a powerful SQL-based Rule Engine, allowing organizations to pre-process data before storing it in DynamoDB. It supports various data transformation mechanisms, such as filtering, routing, aggregation, and enrichment, enabling organizations to shape the data according to their needs.
- **Flexible Data Model**: DynamoDB uses key-value and document data models, suitable for storing and managing structured device events and message data, allowing for easy storage of different MQTT message structures.
- **Powerful Scalability**: EMQX offers cluster scalability, capable of seamless horizontal scaling based on device connections and message volume; DynamoDB, requiring no server or infrastructure management, automatically handles underlying resource management and scaling. The combination of both provides high-performance and highly reliable data storage and scalability.

## Before You Start

This section describes the preparations you need to complete before you start to create a DynamoDB data bridge, including how to install a DynamoDB server and create a data table.

### Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [Data Integration](./data-bridges.md)

### Install DynamoDB Local Server and Create Table

1. Prepare a docker-compose file, `dynamo.yaml`, to set up the Dynamodb local server.

```json
version: '3.8'
services:
  dynamo:
    command: "-jar DynamoDBLocal.jar -sharedDb"
    image: "amazon/dynamodb-local:latest"
    container_name: dynamo
    ports:
      - "8000:8000"
    environment:
      AWS_ACCESS_KEY_ID: root 
      AWS_SECRET_ACCESS_KEY: public
      AWS_DEFAULT_REGION: us-west-2
```

2. Start the server.

```bash
docker-compose -f dynamo.yaml up
```

3. Prepare a table definition and save it to your home directory as `mqtt_msg.json`.

```json
{
    "TableName": "mqtt_msg",
    "KeySchema": [
        { "AttributeName": "id", "KeyType": "HASH" }
    ],
    "AttributeDefinitions": [
        { "AttributeName": "id", "AttributeType": "S" }
    ],
    "ProvisionedThroughput": {
        "ReadCapacityUnits": 5,
        "WriteCapacityUnits": 5
    }
}

```

4. Create a new table via this file.

```bash
docker run --rm -v ${HOME}:/dynamo_data -e AWS_ACCESS_KEY_ID=root -e AWS_SECRET_ACCESS_KEY=public -e AWS_DEFAULT_REGION=us-west-2 amazon/aws-cli dynamodb create-table --cli-input-json file:///dynamo_data/mqtt_msg.json --endpoint-url http://host.docker.internal:8000
```

5. Check if the table was created successfully.

```bash
docker run --rm -e AWS_ACCESS_KEY_ID=root -e AWS_SECRET_ACCESS_KEY=public -e AWS_DEFAULT_REGION=us-west-2 amazon/aws-cli dynamodb list-tables --endpoint-url http://host.docker.internal:8000
```

The following JSON will be printed if the table was created successfully.
```json
{
    "TableNames": [
        "mqtt_msg"
    ]
}
```

## Create Connector

This section demonstrates how to create a DynamoDB data bridge in EMQX Dashboard. It assumes that you run both EMQX and DynamoDB on the local machine. If you have Dynamo and EMQX running remotely, adjust the settings accordingly.

1. Go to EMQX Dashboard, and click **Integration** -> **Connector**.

2. Click **Create** on the top right corner of the page.

3. In the **Create Connector** page, click to select **DynamoDB**, and then click **Next**.

4. Input a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.

5. Input the connection information:

   - **Database Url**: Input `http://127.0.0.1:8000`, or the actual URL if the DynamoDB server is running remotely.
   - **Table Name**: Input `mqtt_msg`.
   - **AWS Access Key ID**: Input `root`.
   - **AWS Secret Access Key**: Input `public`.

6. Leave the **Template** empty by default.

   ::: tip

   When this value is empty the whole message will be stored in the database. The actual value is JSON template data.

   :::

7. Advanced settings (optional):  Choose whether to use **sync** or **async** query mode as needed. For details, see [Data Integration](./data-bridges.md).

8. Before clicking **Create**, you can click **Test Connectivity** to test that the bridge can connect to the server.

9. Then click **Create** to finish the creation of the data bridge.

   A confirmation dialog will appear and ask if you like to create a rule using this data bridge, you can click **Create Rule** to continue creating rules to specify the data to be saved into DynamoDB. You can also create rules by following the steps in [Create Rules for DynamoDB Data Bridge](#create-rules-for-dynamodb-data-bridge).

Now the data bridge should appear in the data bridge list (**Integration** -> **Connector**) with **Resource Status** as **Connected**. 

### Create Connector

Now that you have successfully created the data bridge to DynamoDB, you can continue to create rules to specify the data to be saved into DynamoDB. You need to create two different rules for messages forward and event records. 

1. Go to EMQX Dashboard, and click **Integration** -> **Rules**.

2. Click **Create** on the top right corner of the page.

3. Input `my_rule` as the rule ID, and set the rules in the **SQL Editor** based on the feature to use:

   - To create a rule for message storage, input the following statement, which means the MQTT messages under topic `t/#`  will be saved to DynamoDB.

     Note: If you want to specify your own SQL syntax, make sure that you have included all fields required by the data bridge in the `SELECT` part.

     ```sql
     SELECT 
       *
     FROM
       "t/#"
     ```

   - To create a rule for online/offline status recording, input the following statement:

     ```sql
     SELECT
       str(event) + timestamp as id, *
     FROM 
       "$events/client_connected", "$events/client_disconnected"
     ```

     ::: tip

     For convenience, the `mqtt_msg` topic will be reused to receive online/offline events.

     :::

4. Click the **Add Action** button, select **Forwarding with Data Bridge** from the dropdown list, and then select the data bridge you just created under **Data Bridge**.  Click the **Add** button. 
5. Click the **Create** button to finish the setup. 

Now you have successfully created the data bridge to DynamoDB. You can click **Integration** -> **Flow Designer** to view the topology. It can be seen that the messages under topic `t/#`  are sent and saved to DynamoDB after parsing by rule `my_rule`. 

### Test Rule

Use MQTT X to send a message to topic `t/1` to trigger an online/offline event. 

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello DynamoDB" }'
```

Check the running status of the data bridge, there should be one new incoming and one new outgoing message. 

Check whether the data is written into the `mqtt_msg`  data table. 

```bash
docker run --rm -e AWS_ACCESS_KEY_ID=root -e AWS_SECRET_ACCESS_KEY=public -e AWS_DEFAULT_REGION=us-west-2 amazon/aws-cli dynamodb scan --table-name=mqtt_msg --endpoint-url http://host.docker.internal:8000
```

The output will be:
```json
{
    "Items": [
        {
            "metadata": {
                "S": "{\"rule_id\":\"90d98f59\"}"
            },
            "peerhost": {
                "S": "127.0.0.1"
            },
            "clientid": {
                "S": "emqx_c"
            },
            "flags": {
                "S": "{\"retain\":false,\"dup\":false}"
            },
            "node": {
                "S": "emqx@127.0.0.1"
            },
            "qos": {
                "N": "0"
            },
            "payload": {
                "S": "{ \"msg\": \"hello DynamoDB\" }"
            },
            "pub_props": {
                "S": "{\"User-Property\":{}}"
            },
            "publish_received_at": {
                "N": "1678263363503"
            },
            "topic": {
                "S": "t/1"
            },
            "id": {
                "S": "0005F65F239F03FEF44300000BB40002"
            },
            "event": {
                "S": "message.publish"
            },
            "username": {
                "S": "undefined"
            },
            "timestamp": {
                "N": "1678263363503"
            }
        },
        {
            "conn_props": {
                "S": "{\"User-Property\":{},\"Request-Problem-Information\":1}"
            },
            "peername": {
                "S": "127.0.0.1:59582"
            },
            "metadata": {
                "S": "{\"rule_id\":\"703890a5\"}"
            },
            "clientid": {
                "S": "emqx_c"
            },
            "is_bridge": {
                "S": "false"
            },
            "keepalive": {
                "N": "30"
            },
            "proto_ver": {
                "N": "5"
            },
            "proto_name": {
                "S": "MQTT"
            },
            "connected_at": {
                "N": "1678263363499"
            },
            "receive_maximum": {
                "N": "32"
            },
            "sockname": {
                "S": "127.0.0.1:1883"
            },
            "mountpoint": {
                "S": "undefined"
            },
            "node": {
                "S": "emqx@127.0.0.1"
            },
            "id": {
                "S": "client.connected1678263363499"
            },
            "expiry_interval": {
                "N": "0"
            },
            "event": {
                "S": "client.connected"
            },
            "username": {
                "S": "undefined"
            },
            "timestamp": {
                "N": "1678263363499"
            },
            "clean_start": {
                "S": "true"
            }
        },
        {
            "reason": {
                "S": "normal"
            },
            "peername": {
                "S": "127.0.0.1:59582"
            },
            "metadata": {
                "S": "{\"rule_id\":\"703890a5\"}"
            },
            "clientid": {
                "S": "emqx_c"
            },
            "proto_ver": {
                "N": "5"
            },
            "proto_name": {
                "S": "MQTT"
            },
            "sockname": {
                "S": "127.0.0.1:1883"
            },
            "disconn_props": {
                "S": "{\"User-Property\":{}}"
            },
            "node": {
                "S": "emqx@127.0.0.1"
            },
            "id": {
                "S": "client.disconnected1678263363503"
            },
            "event": {
                "S": "client.disconnected"
            },
            "disconnected_at": {
                "N": "1678263363503"
            },
            "username": {
                "S": "undefined"
            },
            "timestamp": {
                "N": "1678263363503"
            }
        }
    ],
    "Count": 3,
    "ScannedCount": 3,
    "ConsumedCapacity": null
}
```



