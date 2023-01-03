# MySQL

EMQX supports integration with MySQL so you can save client messages and events to MySQL, or use events to trigger the update or removal of data to record the online status or online/offline of clients. 

## Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridge](./data-bridges.md)

## Features supported

- [Connection pool](./data-bridges.md#连接池)
- [Async mode](./data-bridges.md#异步请求模式)
- [Batch mode](./data-bridges.md#批量模式)
- [Buffer mode](./data-bridges.md#缓存队列)
- [SQL preprocessing](./data-bridges.md#SQL-预处理)

## [Configuration parameters](#Configuration)
<!-- TODO 链接到配置手册对应配置章节。 -->

## Quick starts

### Install MySQL

Install MySQL via Docker, and then run the docker image. 

```bash
# To start the MySQL docker image and set the password as public
docker run --name mysql -e MYSQL_ROOT_PASSWORD=public -d mysql

# Access the container
docker exec -it mysql bash

# Locate the MySQL server in the container and input the preset password
mysql -u root -p

# Create and then select the database
CREATE DATABASE emqx_data CHARACTER SET utf8mb4;
use emqx_data;
```

### Connect to MySQL

We will create 2 data bridges to MySQL for messages storage and event records respectively. 

#### [Messages storage](#Storage)

1. Go to EMQX Dashboard, click **Data Integration** -> **Data Bridge**.
2. Click **Create** on the top right corner of the page.
3. In the **Create Data Bridge** page, click to select **MySQL**, and then click **Next**.
4. Input a name for the data bridge. Note: It should be a combination of upper/lower case letters and numbers.
5. Input the connection information. Input **127.0.0.1:3036** as the **Server Host**,  **emqx_data** as the **Database Name**, **root** as the **Username**, and **public** as the **Password**.
6. Configure the **SQL Template**. Use the SQL statements below to insert data. Note: This is a preprocessed SQL, so the fields should not be enclosed in quotation marks, and do not write a semicolon at the end of the statements. 

  ```sql
  INSERT INTO emqx_messages(clientid, topic, payload, created_at) VALUES(
    ${clientid},
    ${topic},
    ${payload},
    FROM_UNIXTIME(${timestamp}/1000)
  )
  ```

Before creating the above data bridge, please use the following SQL statements to create data table `emqx_messages` in MySQL database for storing the client ID, topic, payload, and creating time of every message. 

  ```sql
  CREATE TABLE emqx_messages (
    id INT AUTO_INCREMENT PRIMARY KEY,
    clientid VARCHAR(255),
    topic VARCHAR(255),
    payload BLOB,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  );
  ```

7. Advanced settings (optional):  Choose whether to use sync or async query mode as needed. For details, see [Configuration parameters](#Configuration).
8. Then click **Create** to finish the creation of the data bridge.

We have successfully created the data bridge to MySQL, now we can continue to create rules to specify the data to be saved into MySQL. 

1. Go to EMQX Dashboard, click **Data Integration** -> **Rules**.
2. Click **Create** on the top right corner of the page.
3. Input `my_rule` as the rule ID, and set the rules in the **SQL Editor**. Here we want to save the MQTT messages under topic `t/#`  to MySQL, we can use the SQL syntax below. Note: If you are testing with your SQL, please ensure you have included all required fields in the `SELECT` part. 

  ```sql
  SELECT 
    *
  FROM
    "t/#"
  ```
4. Then click the **Add Action** button, select **Forwarding with Data Bridge** from the dropdown list and then select the data bridge we just created under **Data bridge**.  
5. Click the **Add** button to finish the setup. 

Now we have successfully created the data bridge to MySQL. You can click **Data Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `t/#`  are sent and saved to InfluxDB after parsing by rule  `my_rule`. 

#### Online/Offline status recording

The operating steps are similar to those at the [Message storage](#Storage) part expect for the SQL template and SQL rules. 

The SQL template for online/offline status recording is as follows. Note: This is a preprocessed SQL, so the fields should not be enclosed in quotation marks, and do not write a semicolon at the end of the statements.

```sql
INSERT INTO emqx_client_events(clientid, event, created_at) VALUES (
  ${clientid},
  ${event},
  FROM_UNIXTIME(${timestamp}/1000)
)
```

Before creating this data bridge, please use the following SQL statements to create data table `emqx_client_events` in MySQL database for storing the client ID, event type, and creating time of every event. 

```sql
CREATE TABLE emqx_client_events (
  id INT AUTO_INCREMENT PRIMARY KEY,
  clientid VARCHAR(255),
  event VARCHAR(255),
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

The SQL rule is as follows: 

```sql
SELECT
  *
FROM 
  "$events/client_connected", "$events/client_disconnected"
```

### Test

Use MQTTX  to send a message to topic  `t/1`  to trigger an online/offline event. 

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello MySQL" }'
```

Check the running status of the two data bridges, there should be one new incoming and one new outgoing message. 

Check whether the data is written into the ` emqx_messages`  data table. 

```bash
mysql> select * from emqx_messages;
+----+----------+-------+--------------------------+---------------------+
| id | clientid | topic | payload                  | created_at          |
+----+----------+-------+--------------------------+---------------------+
|  1 | emqx_c   | t/1   | { "msg": "hello MySQL" } | 2022-12-09 08:44:07 |
+----+----------+-------+--------------------------+---------------------+
1 row in set (0.01 sec)
```

`emqx_client_events`  table:

```bash
mysql> select * from emqx_client_events;
+----+----------+---------------------+---------------------+
| id | clientid | event               | created_at          |
+----+----------+---------------------+---------------------+
|  1 | emqx_c   | client.connected    | 2022-12-09 08:44:07 |
|  2 | emqx_c   | client.disconnected | 2022-12-09 08:44:07 |
+----+----------+---------------------+---------------------+
2 rows in set (0.00 sec)
```