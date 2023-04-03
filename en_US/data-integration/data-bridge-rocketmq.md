v# RocketMQ

EMQX supports integration with RocketMQ, so you can forward client messages and events to RocketMQ, for example, use events to trigger the update of data to record the online status or online/offline of clients.

{% emqxce %}
:::tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}


## Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridge](./data-bridges.md)

## Features list

- [Connection pool](./data-bridges.md#Connection pool)
- [Async mode](./data-bridges.md#Async mode)
- [Batch mode](./data-bridges.md#Batch mode)
- [Buffer queue](./data-bridges.md#Buffer queue)
- [SQL preprocessing](./data-bridges.md#Prepared statement)

## [Configuration parameters](#Configuration)
<!-- TODO LIKN TO THE CONFIG doc。 -->

## Quick starts

### Install RocketMQ 

We need a docker compose file, `rocketmq.yaml`, to setup the RocketMQ
```json
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
    networks:
      - emqx_bridge

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
    networks:
      - emqx_bridge
```

Then, prepare the folders and configurations required for running RocetMQ

```bash
mkdir rocketmq
mkdir rocketmq/logs
mkdir rocketmq/store
mkdir rocketmq/conf
``` 

Save the below content into `rocketmq/conf/broker.conf`
```json
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

Start the server
```bash
docker-compose -f rocketmq.yaml up
```

Start a consumer
```
docker run --rm -e NAMESRV_ADDR=host.docker.internal:9876 apache/rocketmq:4.9.4 ./tools.sh org.apache.rocketmq.example.quickstart.Consumer
```
*Note*: in Linux you should change the `host.docker.internal` to your real IP address.

### Connect to RocketMQ

We will create a data bridge to RocketMQ with two rules for messages forward and event records respectively. 

#### [Messages storage](#Storage)

1. Go to EMQX Dashboard, click **Data Integration** -> **Data Bridge**.
2. Click **Create** on the top right corner of the page.
3. In the **Create Data Bridge** page, click to select **RocketMQ**, and then click **Next**.
4. Input a name for the data bridge. Note: It should be a combination of upper/lower case letters and numbers.
5. Input the connection information. Input **127.0.0.1:9876** as the **Server**,  **TopicTest** as the **Topic**, and leave others as default.
6. Advanced settings (optional):  Choose whether to use sync or async query mode as needed. For details, see [Configuration parameters](#Configuration).
7. Then click **Create** to finish the creation of the data bridge.

We have successfully created the data bridge to RocketMQ, now we can continue to create rules to specify the data to be forwarded to RocketMQ. 

1. Go to EMQX Dashboard, click **Data Integration** -> **Rules**.
2. Click **Create** on the top right corner of the page.
3. Input `my_rule` as the rule ID, and set the rules in the **SQL Editor**. Here we want to forward the MQTT messages under topic `t/#`  to RocketMQ, we can use the SQL syntax below. Note: If you are testing with your SQL, please ensure you have included all required fields in the `SELECT` part. 

  ```sql
  SELECT 
    *
  FROM
    "t/#"
  ```

4. Then click the **Add Action** button, select **Forwarding with Data Bridge** from the dropdown list and then select the data bridge we just created under **Data bridge**.  
5. Then, click the **Add** button. 
6. Then click the **Create** button to finish the setup. 

Now we have successfully created the data bridge to RocketMQ. You can click **Data Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `t/#`  are sent and saved to RocketMQ after parsing by rule `my_rule`. 

#### Online/Offline status recording

The operating steps are similar to those at the [Message storage](#Storage) part expect for the SQL template and SQL rules. 

For convenience, the `TopicTest` topic will be reused to receive online/offline events.

The SQL rule is as follows: 

```sql
SELECT
  *
FROM 
  "$events/client_connected", "$events/client_disconnected"
```

### Test

Use MQTTX to send a message to topic `t/1` to trigger an online/offline event. 

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello RocketMQ" }'
```

Check the running status of the two data bridges, there should be one new incoming and one new outgoing message. 

Check whether the data is forwarded to the `TopicTest` topic. 

We can see the below data will be printed by the consumer
```json
ConsumeMessageThread_please_rename_unique_group_name_4_1 Receive New Messages: [MessageExt [brokerName=broker-a, queueId=3, storeSize=581, queueOffset=0, sysFlag=0, bornTimestamp=1679037578889, bornHost=/172.26.83.106:43920, storeTimestamp=1679037578891, storeHost=/172.26.83.106:10911, msgId=AC1A536A00002A9F000000000000060E, commitLogOffset=1550, bodyCRC=7414108, reconsumeTimes=0, preparedTransactionOffset=0, toString()=Message{topic='TopicTest', flag=0, properties={MIN_OFFSET=0, MAX_OFFSET=8, CONSUME_START_TIME=1679037605342, CLUSTER=DefaultCluster}, body=[...], transactionId='null'}]]
ConsumeMessageThread_please_rename_unique_group_name_4_2 Receive New Messages: [MessageExt [brokerName=broker-a, queueId=3, storeSize=511, queueOffset=1, sysFlag=0, bornTimestamp=1679037580174, bornHost=/172.26.83.106:43920, storeTimestamp=1679037580176, storeHost=/172.26.83.106:10911, msgId=AC1A536A00002A9F0000000000000E61, commitLogOffset=3681, bodyCRC=1604860416, reconsumeTimes=0, preparedTransactionOffset=0, toString()=Message{topic='TopicTest', flag=0, properties={MIN_OFFSET=0, MAX_OFFSET=8, CONSUME_START_TIME=1679037605342, CLUSTER=DefaultCluster}, body=[...], transactionId='null'}]]
ConsumeMessageThread_please_rename_unique_group_name_4_3 Receive New Messages: [MessageExt [brokerName=broker-a, queueId=3, storeSize=458, queueOffset=2, sysFlag=0, bornTimestamp=1679037584933, bornHost=/172.26.83.106:43920, storeTimestamp=1679037584934, storeHost=/172.26.83.106:10911, msgId=AC1A536A00002A9F000000000000166E, commitLogOffset=5742, bodyCRC=383397630, reconsumeTimes=0, preparedTransactionOffset=0, toString()=Message{topic='TopicTest', flag=0, properties={MIN_OFFSET=0, MAX_OFFSET=8, CONSUME_START_TIME=1679037605342, CLUSTER=DefaultCluster}, body=[...], transactionId='null'}]]
```



