# Ingest Data into TDengine

TDengine can be widely applied to IoT, Industrial Internet, Connected Vehicles, DevOps, energy, finance, and many other scenarios. 

## Setup TDengine and Create Tables

Deploy TDengine by [Huawei Cloud](https://marketplace.huaweicloud.com/product/OFFI454488918838128640) or Docker:

:::tip
To use TDengine v3.x, please use EMQX v4.4.12 and later.
:::

```bash
docker run --name TDengine -d -p 6030:6030 -p 6041:6041 -p 6043-6049:6043-6049 -p 6043-6049:6043-6049/udp tdengine/tdengine
```

Execute cmd in docker:

```bash
docker exec -it TDengine bash
taos
```

Create database `test`

```sql
create database test;
```

Create table `t_mqtt_msg`. For more details on TDengine data structures and SQL, see [TDengine SQL](https://docs.taosdata.com/taos-sql/):

```sql
USE test;
CREATE TABLE t_mqtt_msg (
  ts timestamp,
  msgid NCHAR(64),
  mqtt_topic NCHAR(255),
  qos TINYINT,
  payload BINARY(1024),
  arrived timestamp
);
```

## Create Rules

Open [EMQX Dashboard](http://127.0.0.1:18083/#/rules). Click `Rule`.

Write SQL:

```sql
SELECT

  *,
  now_timestamp('millisecond')  as ts

FROM

  "#"
```

![image](./assets/rule-engine/TDengine/td_new_reul.png)

The subsequent action creation operation can be selected flexibly depending on your EMQX version.

## Add an Action

Find `Action`, click `Add Action`.

Click `Data persist`, Choice `Data to TDengine`.

> Requires EMQX Enterprise 4.1.1 and later only.

Action parameters:

1. SQL template. In this example we insert a data to TDengine, note that we should specify the database name in the SQL and the character type should be enclosed in single quotes, the SQL template:

```sql
insert into test.t_mqtt_msg(ts, msgid, mqtt_topic, qos, payload, arrived) values (${ts}, '${id}', '${topic}', ${qos}, '${payload}', ${timestamp})
```

2. Now that the resource drop-down box is empty, you can create a TDengine resource by clicking on `Create` in the upper right corner:

Create new TDengine Resource:
</br>
TDEngine Username: root
</br>
TDEngine password: taosdata
</br>

<img src="./assets/rule-engine/TDengine/td_create_resource.png" alt="image" style="zoom:50%;" />

Click `Confirm`.

Return to the action screen and click "Confirm".

<img src="./assets/rule-engine/TDengine/td_creat_action.png" alt="image" style="zoom:50%;" />

Return to the rule screen and click "Create".

## Test the Rule

In the rule list, click on the Rule ID link to preview the rule you just created.

<img src="./assets/rule-engine/TDengine/td_rule.png" alt="image" style="zoom:50%;" />

The rule has been created, now send a data:

```bash
Topic: "t/a"
QoS: 1
Payload: {"msg": "hello"}
```

Query and check:

```sql
select * from t_mqtt_msg;
```

![image](./assets/rule-engine/TDengine/td_queryres.png)
