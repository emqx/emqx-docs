# Auto Subscribe with ClickHouse

## Set up the Environment

Set up the ClickHouse database and set the user name and password to root/public. Take MacOS X as an example:

```bash
## Install dependency
sudo yum install -y epel-release

## Download and run the installation shell script provided with packagecloud.io
curl -s https://packagecloud.io/install/repositories/altinity/clickhouse/script.rpm.sh | sudo bash

## Install the ClickHouse server and client
sudo yum install -y clickhouse-server clickhouse-client

## Start ClickHouse
clickhouse-server

## Start ClickHouse client
clickhouse-client
```

Create “mqtt” database:
```sql
create database mqtt;
```
Create the mqtt_sub table:

```sql
use mqtt;
create table mqtt_sub (
    clientid String,
    topic String,
    qos Nullable(Int8) DEFAULT 0
    ) engine = MergeTree() ORDER BY clientid;
```

:::tip

The table structure of the subscription relationship cannot be altered. Kindly utilize the provided SQL statement for creating the table.

:::

## Create Rules

Open [EMQX Dashboard](http://127.0.0.1:18083/#/rules) and select the "Rules" tab on the left.

Then fill in the rule SQL:

```sql
SELECT * FROM "$events/client_connected"
```

<img src="./assets/rule-engine/redis_sub_1.png" alt="image-20230523152321040" style="zoom:50%;" />



## Add an Action

Select "Add Action" on the "Response Action" interface, and then select "Get Subscription List from ClickHouse" in the "Add Action" drop-down box

<img src="./assets/rule-engine/redis_add_sub.png" alt="image-20230523152508102" style="zoom:50%;" />

Fill in the action parameters:

The action of "Get subscription list from ClickHouse" requires one parameter:

1). Associated resources. The resource drop-down box is empty now, and you can click "Create" in the upper right corner to create a ClickHouse resource, the "Create Resource" dialog box pops up。

<img src="./assets/rule-engine/clickhouse-resource.png" alt="image-20230524170941099" style="zoom:50%;" />



Fill in the resource configuration:

Fill in the ClickHouse server address and the values corresponding to other configurations, and then click the "Test" button to ensure that the connection test is successful.

Finally, click the "Confirm" button.

Return to the response action interface and click "Confirm".

Return to the rule creation interface and click "Create".



![image-20230524171220978](./assets/rule-engine/clickhouse-sub-rule.png)



## Test the Rule

The rule has been created, and you can insert a subscription relationship into ClickHouse with the command below:

```bash
insert into mqtt_sub(clientid, topic, qos) values('test', 't1', 1);
```

<img src="./assets/rule-engine/clickhouse_sub_7.png" style="zoom:50%;" />

Log in to the device (with clientid test) via Dashboard:

![image-20230523153725483](./assets/rule-engine/redis_sub_9.png)



Check the "Subscription" list, and you can see that the Broker obtains the subscription relationship from ClickHouse and subscribes as the agent device:

![image-20230523153908018](./assets/rule-engine/redis_sub_10.png)

