# Ingest Data into MySQL

EMQX supports integration with MySQL. You can save client messages and events to MySQL, or record the online status or online/offline of clients by using events to trigger the data update or removal. 

{% emqxce %}
:::tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}

::: tip Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)

- Knowledge about [data bridge](./data-bridges.md)


:::

## Feature List

- [Connection pool](./data-bridges.md)
- [Async mode](./data-bridges.md)
- [Batch mode](./data-bridges.md)
- [Buffer mode](./data-bridges.md)
- [SQL preprocessing](./data-bridges.md)

<!-- [Configuration parameters](#Configuration) TODO 链接到配置手册对应配置章节。 -->

## Quick Start Tutorial

This section introduces how to configure the MySQL data bridge, covering topics like how to set up the MySQL server, create data bridges and rules for forwarding data to MySQL and test the data bridges and rules.

This tutorial assumes that you run both EMQX and MySQL on the local machine. If you have MySQL and EMQX running remotely, adjust the settings accordingly.

### Install MySQL Server

Install MySQL server via Docker, and then run the docker image. 

```bash
# To start the MySQL docker image and set the password as public
docker run --name mysql -p 3306:3306 -e MYSQL_ROOT_PASSWORD=public -d mysql

# Access the container
docker exec -it mysql bash

# Locate the MySQL server in the container and input the preset password
mysql -u root -p

# Create and then select the database
CREATE DATABASE emqx_data CHARACTER SET utf8mb4;
use emqx_data;
```

### Create Data Tables

1. Use the following SQL statements to create data table `emqx_messages` in MySQL database for storing the client ID, topic, payload, and creation time of every message.

   ```sql
   CREATE TABLE emqx_messages (
     id INT AUTO_INCREMENT PRIMARY KEY,
     clientid VARCHAR(255),
     topic VARCHAR(255),
     payload BLOB,
     created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
   );
   ```

2. Use the following SQL statements to create data table `emqx_client_events` in MySQL database for storing the client ID, event type, and creation time of every event. 

   ```sql
   CREATE TABLE emqx_client_events (
     id INT AUTO_INCREMENT PRIMARY KEY,
     clientid VARCHAR(255),
     event VARCHAR(255),
     created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
   );
   ```

### Create MySQL Data Bridges

Data bridges for message storage and event recording require different SQL templates. Therefore, you need to create 2 different data bridges to MySQL for messages storage and event recording. 

1. Go to EMQX Dashboard, click **Data Integration** -> **Data Bridge**.

2. Click **Create** on the top right corner of the page.

3. In the **Create Data Bridge** page, click to select **MySQL**, and then click **Next**.

4. Input a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.

5. Input the connection information:

   - **Server Host**: Input `http://127.0.0.1:3036`, or the actual URL if the MySQL server is running remotely.
   - **Database Name**: Input `emqx_data`.
   - **Username**: Input `root`.
   - **Password**: Input `public`.

6. Configure the **SQL Template** based on the feature to use: 

   Note: This is a preprocessed SQL, so the fields should not be enclosed in quotation marks, and do not write a semicolon at the end of the statements.
   
   - To create a data bridge for message storage, use the statement below:
   
     ```sql
     INSERT INTO emqx_messages(clientid, topic, payload, created_at) VALUES(
       ${clientid},
       ${topic},
       ${payload},
       FROM_UNIXTIME(${timestamp}/1000)
     )
     ```
   
   - To create a data bridge for online/offline status recording, use the statement below:
   
     ```sql
     INSERT INTO emqx_client_events(clientid, event, created_at) VALUES (
       ${clientid},
       ${event},
       FROM_UNIXTIME(${timestamp}/1000)
     )
     ```

7. Advanced settings (optional):  Choose whether to use **sync** or **async** query mode as needed. For details, see [Configuration](./data-bridges.md).

8. Before clicking **Create**, you can click **Test Connectivity** to test that the bridge can connect to the TDengine.

9. Click **Create** to finish the creation of the data bridge.

   A confirmation dialog will appear and ask if you like to create a rule using this data bridge, you can click **Create Rule** to continue creating rules to specify the data to be saved into MySQL. You can also create rules by following the steps in [Create Rules for MySQL Data Bridge](#create-rules-for-mysql-data-bridge).

Now the MySQL data bridge should appear in the data bridge list (**Data Integration** -> **Data Bridge**) with **Resource Status** as **Connected**. 

### Create Rules for MySQL Data Bridge

After you have successfully created the data bridge to MySQL, you can continue to create rules to specify the data to be saved into MySQL and rules for the online/offline status recording. 

1. Go to EMQX Dashboard, click **Data Integration** -> **Rules**.

2. Click **Create** on the top right corner of the page.

3. Input `my_rule` as the rule ID, and set the rules in the **SQL Editor** based on the feature to use:

   - To create a rule for message storage, input the following statement, which means the MQTT messages under topic `t/#`  will be saved to MySQL.

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

4. Click the **Add Action** button, select **Forwarding with Data Bridge** from the dropdown list and then select the data bridge we just created under **Data Bridge**. Click the **Add** button.
5. Click the **Create** button to finish the setup. 

Now you have successfully created the rule for MySQL data bridge. You can click **Data Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `t/#`  are sent and saved to MySQL after parsing by rule  `my_rule`. 

### Test the Data Bridge and Rule

Use MQTT X  to send a message to topic  `t/1`  to trigger an online/offline event. 

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello MySQL" }'
```

Check the running status of the two data bridges, there should be one new incoming and one new outgoing message. 

Check whether the data is written into the ` emqx_messages` data table. 

```bash
mysql> select * from emqx_messages;
+----+----------+-------+--------------------------+---------------------+
| id | clientid | topic | payload                  | created_at          |
+----+----------+-------+--------------------------+---------------------+
|  1 | emqx_c   | t/1   | { "msg": "hello MySQL" } | 2022-12-09 08:44:07 |
+----+----------+-------+--------------------------+---------------------+
1 row in set (0.01 sec)
```

Check whether the data is written into the `emqx_client_events` table.

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