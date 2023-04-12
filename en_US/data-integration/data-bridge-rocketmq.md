# Ingest Data into RocketMQ

EMQX supports integration with RocketMQ, so you can forward client messages and events to RocketMQ, for example, use events to trigger the update of data to record the online status or online/offline of clients.

{% emqxce %}
:::tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}


## Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridge](./data-bridges.md)

## Features List

- [Connection pool](./data-bridges.md)
- [Async mode](./data-bridges.md)
- [Batch mode](./data-bridges.md)
- [Buffer queue](./data-bridges.md)
- [SQL preprocessing](./data-bridges.md)

## Quick Start Tutorial

This section introduces how to configure the RocketMQ data bridge, covering topics like how to set up the RocketMQ server, create data bridges and rules for forwarding data to RocketMQ and test the data bridges and rules.

This tutorial assumes that you run both EMQX and RocketMQ on the local machine. If you have RocketMQ and EMQX running remotely, adjust the settings accordingly.

### Install RocketMQ 

1. Prepare a docker-compose file, `rocketmq.yaml`, to set up the RocketMQ.

```yaml
version: '3.9'

services:
  mqnamesrv:
    image: apache/rocketmq:4.9.4
    container_name: rocketmq_namesrv
    ports:
      - 9876:9876
    volumes:
      - ./rocketmq/logs:/opt/logs
      - ./rocketmq/store:/opt/store
    command: ./mqnamesrv

  mqbroker:
    image: apache/rocketmq:4.9.4
    container_name: rocketmq_broker
    ports:
      - 10909:10909
      - 10911:10911
    volumes:
      - ./rocketmq/logs:/opt/logs
      - ./rocketmq/store:/opt/store
      - ./rocketmq/conf/broker.conf:/etc/rocketmq/broker.conf
    environment:
        NAMESRV_ADDR: "rocketmq_namesrv:9876"
        JAVA_OPTS: " -Duser.home=/opt"
        JAVA_OPT_EXT: "-server -Xms1024m -Xmx1024m -Xmn1024m"
    command: ./mqbroker -c /etc/rocketmq/broker.conf
    depends_on:
      - mqnamesrv
```

2. Prepare the folders and configurations required for running RocetMQ.

```bash
mkdir rocketmq
mkdir rocketmq/logs
mkdir rocketmq/store
mkdir rocketmq/conf
```

3. Save the below content into `rocketmq/conf/broker.conf`.

```bash
brokerClusterName=DefaultCluster
brokerName=broker-a
brokerId=0

brokerIP1=change me to your real IP address

defaultTopicQueueNums=4
autoCreateTopicEnable=true
autoCreateSubscriptionGroup=true

listenPort=10911
deleteWhen=04

fileReservedTime=120
mapedFileSizeCommitLog=1073741824
mapedFileSizeConsumeQueue=300000
diskMaxUsedSpaceRatio=100
maxMessageSize=65536

brokerRole=ASYNC_MASTER

flushDiskType=ASYNC_FLUSH
```

4. Start the server.

```bash
docker-compose -f rocketmq.yaml up
```

5. Start a consumer.

```
docker run --rm -e NAMESRV_ADDR=host.docker.internal:9876 apache/rocketmq:4.9.4 ./tools.sh org.apache.rocketmq.example.quickstart.Consumer
```
::: tip

In Linux, you should change the `host.docker.internal` to your real IP address.

:::

### Create RocketMQ Data Bridge

1. Go to EMQX Dashboard, and click **Data Integration** -> **Data Bridge**.

2. Click **Create** on the top right corner of the page.

3. In the **Create Data Bridge** page, click to select **RocketMQ**, and then click **Next**.

4. Input a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.

5. Input the connection information. Input `127.0.0.1:9876` as the **Server**,  `TopicTest` as the **Topic**, and leave others as default.

6. Leave the **Template** empty by default.

   ::: tip

   When this value is empty the whole message will be stored in the database. The actual value is JSON template data.

   :::

7. Advanced settings (optional):  Choose whether to use **sync** or **async** query mode as needed.

8. Before clicking **Create**, you can click **Test Connectivity** to test that the bridge can connect to the MySQL server.

9. Then click **Create** to finish the creation of the data bridge.

   A confirmation dialog will appear and ask if you like to create a rule using this data bridge, you can click **Create Rule** to continue creating rules to specify the data to be saved into RocketMQ. You can also create rules by following the steps in [Create Rules for RocketMQ Data Bridge](#create-rules-for-rocketmq-data-bridge).

Now the RocketMQ data bridge should appear in the data bridge list (**Data Integration** -> **Data Bridge**) with **Resource Status** as **Connected**. 

### Create Rules for RocketMQ Data Bridge

Now that you have successfully created the data bridge to RocketMQ, you can continue to create rules to specify the data to be saved into RocketMQ. You need to create two different rules for messages forward and event records. 

1. Go to EMQX Dashboard, and click **Data Integration** -> **Rules**.

2. Click **Create** on the top right corner of the page.

3. Input `my_rule` as the rule ID, and set the rules in the **SQL Editor** based on the feature to use:

   - To create a rule for message storage, input the following statement, which means the MQTT messages under topic `t/#`  will be saved to RocketMQ.

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
       *
     FROM 
       "$events/client_connected", "$events/client_disconnected"
     ```

     ::: tip

     For convenience, the `TopicTest` topic will be reused to receive online/offline events.

     :::

4. Click the **Add Action** button, select **Forwarding with Data Bridge** from the dropdown list, and then select the data bridge you just created under **Data Bridge**. Click the **Add** button. 
6. Click the **Create** button to finish the setup. 

Now you have successfully created the data bridge to RocketMQ. You can click **Data Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `t/#`  are sent and saved to RocketMQ after parsing by rule `my_rule`. 

### Test the Data Bridge and Rule

Use MQTTX to send a message to topic `t/1` to trigger an online/offline event. 

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello RocketMQ" }'
```

Check the running status of the two data bridges, there should be one new incoming and one new outgoing message. 

Check whether the data is forwarded to the `TopicTest` topic. 

The following data will be printed by the consumer.
```bash
ConsumeMessageThread_please_rename_unique_group_name_4_1 Receive New Messages: [MessageExt [brokerName=broker-a, queueId=3, storeSize=581, queueOffset=0, sysFlag=0, bornTimestamp=1679037578889, bornHost=/172.26.83.106:43920, storeTimestamp=1679037578891, storeHost=/172.26.83.106:10911, msgId=AC1A536A00002A9F000000000000060E, commitLogOffset=1550, bodyCRC=7414108, reconsumeTimes=0, preparedTransactionOffset=0, toString()=Message{topic='TopicTest', flag=0, properties={MIN_OFFSET=0, MAX_OFFSET=8, CONSUME_START_TIME=1679037605342, CLUSTER=DefaultCluster}, body=[...], transactionId='null'}]]
ConsumeMessageThread_please_rename_unique_group_name_4_2 Receive New Messages: [MessageExt [brokerName=broker-a, queueId=3, storeSize=511, queueOffset=1, sysFlag=0, bornTimestamp=1679037580174, bornHost=/172.26.83.106:43920, storeTimestamp=1679037580176, storeHost=/172.26.83.106:10911, msgId=AC1A536A00002A9F0000000000000E61, commitLogOffset=3681, bodyCRC=1604860416, reconsumeTimes=0, preparedTransactionOffset=0, toString()=Message{topic='TopicTest', flag=0, properties={MIN_OFFSET=0, MAX_OFFSET=8, CONSUME_START_TIME=1679037605342, CLUSTER=DefaultCluster}, body=[...], transactionId='null'}]]
ConsumeMessageThread_please_rename_unique_group_name_4_3 Receive New Messages: [MessageExt [brokerName=broker-a, queueId=3, storeSize=458, queueOffset=2, sysFlag=0, bornTimestamp=1679037584933, bornHost=/172.26.83.106:43920, storeTimestamp=1679037584934, storeHost=/172.26.83.106:10911, msgId=AC1A536A00002A9F000000000000166E, commitLogOffset=5742, bodyCRC=383397630, reconsumeTimes=0, preparedTransactionOffset=0, toString()=Message{topic='TopicTest', flag=0, properties={MIN_OFFSET=0, MAX_OFFSET=8, CONSUME_START_TIME=1679037605342, CLUSTER=DefaultCluster}, body=[...], transactionId='null'}]]
```



