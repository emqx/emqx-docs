# Ingest Data into ClickHouse

### Install ClickHouse Server and Create Tables

Steup the ClickHouse database, and set username/password to default/public. Take CentOS as an example:

```bash
## install dependencies
sudo yum install -y epel-release

## download and run the installation script provided by packagecloud.io
curl -s https://packagecloud.io/install/repositories/altinity/clickhouse/script.rpm.sh | sudo bash

## install ClickHouse server and client
sudo yum install -y clickhouse-server clickhouse-client

## start the ClickHouse server
clickhouse-server

## start the ClickHouse client
clickhouse-client
```

Create the `test` database:
```sql
create database test;
```
create `t_mqtt_msg` table:

```sql
use test;
create table t_mqtt_msg (msgid Nullable(String), topic Nullable(String), clientid Nullable(String), payload Nullable(String)) engine = Log;
```

![](./assets/rule-engine/clickhouse_0.png)

## **Create the Rule**

Go to the [EMQX Dashboard](http://127.0.0.1:18083/#/rules), and type in the follwing SQL:

```sql
SELECT * FROM "#"
```

![image](./assets/rule-engine/clickhouse_1.png)

## **Add an Action**

Add an action and select "Data to ClickHouse" from the dropdown list.

![image](./assets/rule-engine/clickhouse_2.png)

Provide the arguments for the action:

1). The resource id. We create a new ClickHouse resource now:

Click "create" right to the resource Id text box, and then select "ClickHouse" and fill in the following paramenters:

![image](./assets/rule-engine/clickhouse_4.png)

Click the "Confirm" button.

2). The SQL template. In this example we insert an message to ClickHouse:

```sql
insert into test.t_mqtt_msg(msgid, clientid, topic, payload) values ('${id}', '${clientid}', '${topic}', '${payload}')
```

![image](./assets/rule-engine/clickhouse_5.png)

Keep all other arguments unchanged and confirm the action creation.

Then click "Create" to confirm the rule creation.

## Test the Rule

Now the rule has been created, we send a testing message to the broker:

```bash
Topic: "t/a"
QoS: 1
Payload: "hello"
```

And then we can verify if the message is inserted into the ClickHouse table:

![image](./assets/rule-engine/clickhouse_8.png)
