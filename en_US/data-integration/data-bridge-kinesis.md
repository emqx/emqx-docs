# Stream MQTT Data into Amazon Kinesis

EMQX supports seamless integration with [Amazon Kinesis Data Streams](https://aws.amazon.com/kinesis/data-streams/) which can be integrated to several other AWS Services for real-time extraction, processing and analysis of MQTT data.

{% emqxce %}
::: tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}

:::tip Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridge](./data-bridges.md)

:::

## Feature List

- [Connection pool](./data-bridges.md#connection-pool)
- [Batch mode](./data-bridges.md#batch-mode)
- [Buffer queue](./data-bridges.md#buffer-queue)

## Quick Start Tutorial

This section introduces how to configure the Amazon Kinesis data bridge, including how to set up the Kinesis service, create data bridges and rules for forwarding data to Kinesis and test the data bridges and rules.

### Create Stream in Amazon Kinesis Data Streams

Follow the steps below to create a Stream via the AWS Management Console (see [this tutorial](https://docs.aws.amazon.com/streams/latest/dev/how-do-i-create-a-stream.html) for more details).

1. Sign in to the AWS Management Console and open the [Kinesis console](https://console.aws.amazon.com/kinesis).

2. In the navigation bar, expand the Region selector and choose a Region.

3. Choose **Create data stream**.

4. On the **Create Kinesis stream** page, enter a name for your data stream and then choose the **On-demand** capacity mode.

### Emulate Amazon Kinesis Data Streams locally

To facilitate the development and test, you can emulate the Amazon Kinesis Data Streams service locally via [LocalStack](https://localstack.cloud/). With LocalStack, you can run your AWS applications entirely on your local machine without connecting to a remote cloud provider.

1. Install and run it using a Docker Image:

   ```bash
   # To start the LocalStack docker image locally
   docker run --name localstack -p '4566:4566' -e 'KINESIS_LATENCY=0' -d localstack/localstack:2.1
   
   # Access the container
   docker exec -it localstack bash
   ```

2. Create a stream named **my_stream** with only one shard:

   ```bash
   awslocal kinesis create-stream --stream-name "my_stream" --shard-count 1
   ```

### Create a Kinesis Data Bridge

1. Go to EMQX Dashboard, click **Integration** -> **Data Bridge**.
2. Click **Create** on the top right corner of the page.
3. In the **Create Data Bridge** page, click to select **Amazon Kinesis**, and then click **Next**.
4. Enter a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.
5. Enter the connection information:

   - **AWS Access Key ID**: Enter the [Access key ID](https://docs.aws.amazon.com/powershell/latest/userguide/pstools-appendix-sign-up.html). If using [LocalStack](#emulate-amazon-kinesis-data-streams-locally), enter any value.
   - **AWS Secret Access Key**: Enter the [secret access key](https://docs.aws.amazon.com/powershell/latest/userguide/pstools-appendix-sign-up.html). If using [LocalStack](#emulate-amazon-kinesis-data-streams-locally), enter any value.
   - **Amazon Kinesis Endpoint**: Enter the [Endpoint](https://docs.aws.amazon.com/general/latest/gr/ak.html) for the Kinesis service. If using [LocalStack](#emulate-amazon-kinesis-data-streams-locally), input `http://localhost:4566`.
   - **Amazon Kinesis Stream**: Enter the stream name you created in [Create Stream in Amazon Kinesis Data Streams](#create-stream-in-amazon-kinesis-data-streams).
   - **Partition Key**: Enter the Partition Key that shall be associated with records that are sent to this stream. Placeholders of the form `${variable_name}` are allowed (see next step for example on placeholders).
6. In the **Payload Template** field, leave it blank or define a template.

      -  If left blank, it will encode all visible inputs from the MQTT message using JSON format, such as clientid, topic, payload, etc.

      - If using the defined template, placeholders of the form `${variable_name}` will be filled with the corresponding value from the MQTT context. For example, `${topic}` will be replaced with `my/topic` if such is the MQTT message topic.


11. Advanced settings (optional): Choose whether to use buffer queue and batch mode as needed. For details, see [Data Integration](./data-bridges.md).

11. Before clicking **Create**, you can click **Test Connectivity** to test that the bridge can connect to the Amazon Kinesis server.

12. Click **Create** to finish the creation of the data bridge.

    A confirmation dialog will appear and ask if you like to create a rule using this data bridge, you can click **Create Rule** to continue creating rules to specify the data to be saved into Amazon Kinesis. You can also create rules by following the steps in [Create Rules for Amazon Kinesis Data Bridge](#create-a-rule-for-amazon-kinesis-data-bridge).

### Create a Rule for Amazon Kinesis Data Bridge

You can continue to create rules to specify the data to be saved into Amazon Kinesis.

1. Go to EMQX Dashboard, click **Integration** -> **Rules**.

2. Click **Create** on the top right corner of the page.

3. Input `my_rule` as the rule ID.

3. Set the rules in the **SQL Editor**. If you want to save the MQTT messages under topic `t/#` to Amazon Kinesis Data Streams, you can use the SQL syntax below.

   Note: If you want to specify your own SQL syntax, make sure that the `SELECT` part includes all fields required by the payload template in the data bridge.

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

5. Click the **Add Action** button, select **Forwarding with Data Bridge** from the dropdown list, and then select the data bridge you just created under **Data Bridge**. Then click the **Add** button.

4. Click **Create** at the page bottom to finish the creation.

Now a rule to forward data to Amazon Kinesis Data Streams via the Amazon Kinesis bridge is created. You can click **Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `t/#` are sent and saved to Amazon Kinesis Data Streams after parsing by rule `my_rule`.

### Test the Data Bridge and Rule

1. Use MQTTX to send messages on the topic `t/my_topic`.

   ```bash
   mqttx pub -i emqx_c -t t/my_topic -m '{ "msg": "hello Amazon Kinesis" }'
   ```

2. Check the running status of the data bridge, there should be one new incoming and one new outgoing message.

3. Go to [Amazon Kinesis Data Viewer](https://docs.aws.amazon.com/streams/latest/dev/data-viewer.html). You should see the message when getting records.

#### Use LocalStack to Check

If you use LocalStack, follow the steps below to check the received data.

1. Use the following command to get the *ShardIterator* before sending the message to the bridge.
   
   ```bash
   awslocal kinesis get-shard-iterator --stream-name my_stream --shard-id shardId-000000000000 --shard-iterator-type LATEST
   {
   "ShardIterator": "AAAAAAAAAAG3YjBK9sp0uSIFGTPIYBI17bJ1RsqX4uJmRllBAZmFRnjq1kPLrgcyn7RVigmH+WsGciWpImxjXYLJhmqI2QO/DrlLfp6d1IyJFixg1s+MhtKoM6IOH0Tb2CPW9NwPYoT809x03n1zL8HbkXg7hpZjWXPmsEvkXjn4UCBf5dBerq7NLKS3RtAmOiXVN6skPpk="
   }
   ```
   
2. Use MQTTX to send messages on the topic `t/my_topic`.

   ```bash
   mqttx pub -i emqx_c -t t/my_topic -m '{ "msg": "hello Amazon Kinesis" }'
   ```

3. Read the records and decode the received data.
   ```bash
   awslocal kinesis get-records --shard-iterator="AAAAAAAAAAG3YjBK9sp0uSIFGTPIYBI17bJ1RsqX4uJmRllBAZmFRnjq1kPLrgcyn7RVigmH+WsGciWpImxjXYLJhmqI2QO/DrlLfp6d1IyJFixg1s+MhtKoM6IOH0Tb2CPW9NwPYoT809x03n1zL8HbkXg7hpZjWXPmsEvkXjn4UCBf5dBerq7NLKS3RtAmOiXVN6skPpk="
   {
       "Records": [
           {
               "SequenceNumber": "49642650476690467334495639799144299020426020544120356866",
               "ApproximateArrivalTimestamp": 1689389148.261,
               "Data": "eyAibXNnIjogImhlbGxvIEFtYXpvbiBLaW5lc2lzIiB9",
               "PartitionKey": "key",
               "EncryptionType": "NONE"
           }
       ],
       "NextShardIterator": "AAAAAAAAAAFj5M3+6XUECflJAlkoSNHV/LBciTYY9If2z1iP+egC/PtdVI2t1HCf3L0S6efAxb01UtvI+3ZSh6BO02+L0BxP5ssB6ONBPfFgqvUIjbfu0GOmzUaPiHTqS8nNjoBtqk0fkYFDOiATdCCnMSqZDVqvARng5oiObgigmxq8InciH+xry2vce1dF9+RRFkKLBc0=",
       "MillisBehindLatest": 0
   }
   
   echo 'eyAibXNnIjogImhlbGxvIEFtYXpvbiBLaW5lc2lzIiB9' | base64 -d
   { "msg": "hello Amazon Kinesis" }
   ```
