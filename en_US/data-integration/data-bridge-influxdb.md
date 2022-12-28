# InfluxDB

InfluxDB is a database for storing and analyzing time series data. Its powerful data throughput capability and stable performance make it very suitable to be applied in the field of Internet of Things (IoT).

EMQX now supports connection to mainstream versions of InfluxDB Cloud, InfluxDB OSS, or InfluxDB Enterprise.



## Prerequisites

- Knowledge about [InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/), as EMQX will follow this protocol when writing data into InfluxDB
- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridge](./data-bridges.md)

## Features supported

- [Connection pool](./data-bridges.md#连接池) <!-- TODO 确认改版后知否支持-->
- [Async mode](./data-bridges.md#异步请求模式)
- [Batch mode](./data-bridges.md#批量模式)
- [Buffer queue](./data-bridges.md#缓存队列)

## [Configuration parameters](#Configuration)
<!-- TODO 链接到配置手册对应配置章节。 -->

## Quick starts

### [Install InfluxDB](#install)

1. [Install InfluxDB](https://docs.influxdata.com/influxdb/v2.5/install/) via Docker, and then run the docker image. 

```bash
# TO start the InfluxDB docker image
docker run --name influxdb -p 8086:8086 influxdb:2.5.1
```

2. With InfluxDB running, visit [http://localhost:8086.]( http://localhost:8086) Set the **Username**, **Password**, **Organization Name**, and **Bucket Name**. 
3. In the InfluxDB UI, click **Load Data** -> **API Token** and then follow the instructions to [create all-access tokens](https://docs.influxdata.com/influxdb/v2.5/install/#create-all-access-tokens). 

### Connect to InfluxDB

1. Go to EMQX Dashboard, click **Data Integration** -> **Data Bridge**.
2. Click **Create** on the top right corner of the page.
3. In the **Create Data Bridge** page, click to select **InfluxDB**, and then click **Next**.
4. Input a name for the data bridge. Note: It should be a combination of upper/lower case letters and numbers.
5. Select the InfluxDB version as needed, by default v2 is selected.
6. Input the connection information.
   1. For **Server Host**, input **127.0.0.1:8086**. If you are creating a connection to InfluxDB Cloud, use 443 as the port No., that is, input **{url}:443** and enable TLS  connection. 
   2. Select **Token** as the **Auth Type**. Input the **Organization**, **Bucket**, and **Token** we set in the [Install InfluxDB](#install) step.
7. Define data parsing method, including **Measurement**, **Timestamp**, **Fields** and **Tags**. Note: All key values can be variables and you can also follow the [InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/) to set them. 
8. Advanced settings (optional): Choose whether to use sync or async query mode, and whether to enable queue or batch. For details, see [Configuration parameters](#Configuration).
9.  Then click **Create** to finish the setup. 

We have successfully created the data bridge to InfluxDB, now we can continue to create rules to specify the data to be saved into InfluxDB. 

1. Go to EMQX Dashboard, click **Data Integration** -> **Rules**.
2. Click **Create** on the top right corner of the page.
3. Input `my_rule` as the rule ID, and set the rules in the **SQL Editor**. Here we want to save the MQTT messages under topic `t/#`  to InfluxDB, we can use the SQL syntax below. Note: If you are testing with your SQL, please ensure you have included all required fields in the `SELECT` part. 

  ```sql
  SELECT 
    *
  FROM
    "t/#"
  ```

4. Then click the **Add Action** button, select **Forwarding with Data Bridge** from the dropdown list and then select the data bridge we just created under **Data bridge**.  
5. Click the **Add** button to finish the setup. 

Now we have successfully created the data bridge to InfluxDB. You can click **Data Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `t/#`  are sent and saved to InfluxDB after parsing by rule  `my_rule`. 

### Test

Use MQTTX  to send a message to topic  `t/1`  to trigger an online/offline event. 

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello InfluxDB" }'
```

Check the running status of the two data bridges, there should be one new incoming and one new outgoing message. 

In the InfluxDB UI, you can confirm whether the message is written into the InfluxDB via the **Data Explorer** window. 