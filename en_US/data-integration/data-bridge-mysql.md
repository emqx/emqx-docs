# Ingest MQTT Data into MySQL

{% emqxce %}
:::tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}

[MySQL](https://www.mysql.com/) is a widely used relational database with high reliability and stability, and can be quickly installed, configured and used. MySQL data bridge can efficiently store MQTT messages in the MySQL database, and also supports real-time updating or deletion of data in MySQL through event triggering. With the help of MySQL data bridge, you can easily implement functions such as message storage, device online/offline status update, and device behavior recording to achieve flexible IoT data storage and device management functions.

This page introduces the data integration between EMQX and MySQL and provides practical rules and data bridge creation guidance.

## How It Works

MySQL data bridge is an out-of-the-box feature in EMQX, which enables complex business development through simple configuration. In a typical IoT application, EMQX, as the IoT platform, is responsible for device connection and transmitting messages. MySQL, as the data storage platform, is responsible for storing device status and metadata, as well as message data storage and data analysis.

<img src="./assets/emqx-integraion-mysql.jpg" alt="EMQX MySQL 数据集成" style="zoom:67%;" />

EMQX forwards device events and data to MySQL through the rule engine and data bridge. Applications can read the data in MySQL to sense the device status, obtain device online and offline records, and analyze device data. The specific workflow is as follows:

- **IoT devices connect to EMQX**: After IoT devices are successfully connected through the MQTT protocol, online events will be triggered. The events include information such as device ID, source IP address, and other attributes.
- **Message publication and reception**: The devices publish telemetry and status data to specific topics. When EMQX receives these messages, it initiates the matching process within its rules engine.
- **Rule Engine Processing Messages**: With the built-in rules engine, messages and events from specific sources can be processed based on topic matching. The rules engine matches the corresponding rules and processes messages and events, such as converting data formats, filtering out specific information, or enriching messages with contextual information.
- **Write to MySQL**: The rule triggers the writing of messages to MySQL. With the help of SQL templates, users can extract data from the rule processing results to construct SQL and send it to MySQL for execution, so that specific fields of the message can be written or updated into the corresponding tables and columns of the database.

After the event and message data are written to MySQL, you can connect to MySQL to read the data for flexible application development, such as:

- Connect to visualization tools, such as Grafana, to generate charts based on data and show data changes.
- Connect to the device management system, view the device list and status, detect abnormal device behavior, and eliminate potential problems in a timely manner.

## Features and Benefits

The data integration with MySQL can bring the following features and advantages to your business:

- **Flexible Event Handling**: Through the EMQX rules engine, MySQL can handle device lifecycle events, greatly facilitating the development of various management and monitoring tasks required for implementing IoT applications. By analyzing event data, you can promptly detect device failures, abnormal behavior, or trend changes to take appropriate measures.
- **Message Transformation**: Messages can undergo extensive processing and transformation through EMQX rules before being written to MySQL, making storage and usage more convenient.
- **Flexible Data Operations**: With SQL templates provided by MySQL data bridging, it's easy to write or update data from specific fields to the corresponding tables and columns in the MySQL database, enabling flexible data storage and management.
- **Integration of Business Processes**: MySQL data bridging allows you to integrate device data with MySQL's rich ecosystem applications, facilitating integration with systems like ERP, CRM, or other custom business systems to achieve advanced business processes and automation.
- **Runtime Metrics**: Support for viewing runtime metrics of each data bridge, such as total message count, success/failure counts, current rates, and more.

Through flexible event handling, extensive message transformation, flexible data operations, and real-time monitoring and analysis capabilities, you can build efficient, reliable, and scalable IoT applications, benefiting your business decisions and optimizations.

## Before You Start

This section describes the preparations you need to complete before you start to create the MySQL data bridges in EMQX Dashboard, including installing the MySQL server and creating data tables.

### Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)

- Knowledge about [data bridge](./data-bridges.md)

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

## Create Rule and MySQL Data Bridges

Data bridges for message storage and event recording require different SQL templates. Therefore, you need to create 2 different data bridges to MySQL for message storage and event recording. 

### Message Storage

This demonstration assumes that you run both EMQX and MySQL on the local machine. If you have MySQL and EMQX running remotely, adjust the settings accordingly.

1. Go to EMQX Dashboard, click **Integration** -> **Rules**.

2. Click **Create** on the top right corner of the page.

3. Enter `my_rule` as the rule ID, and set the rules in the **SQL Editor** with the following statement, which means the MQTT messages under topic `t/#`  will be saved to MySQL.

   Note: If you want to specify your own SQL syntax, make sure that you have included all fields required by the data bridge in the `SELECT` part.

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   If you are a beginner user, click **SQL Examples** and **Enable Test** to learn and test the SQL rule. 

   :::

4. Click the + **Add Action** button to define an action that will be triggered by the rule. Select **Forwarding with Data Bridge** from the dropdown list. With this action, EMQX sends the data processed by the rule to MySQL.

5. Click the **+** icon next to the **Data bridge** drop-down box to create a data bridge.

6. Select `MySQL` from the **Type of Data Bridge** drop-down list. 

7. Enter a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.

8. Enter the following information for connecting to MySQL server:

   - **Server Host**: Input `127.0.0.1:3306`, or the actual hostname if the MySQL server is running remotely.
   - **Database Name**: Input `emqx_data`.
   - **Username**: Input `root`.
   - **Password**: Input `public`.

9. Configure the **SQL Template** based on the feature to use:

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

   <!-- 
   <img src="./assets/MySQL_bridge.png" alt="MySQL_ bridge" style="zoom:67%;" />
   -->

10. Advanced settings (optional):  See [Advanced Configurations](#advanced-configurations).

11. Click the **Add** button to complete the data bridge configuration. You will be redirected back to the **Add Action** page. Select the MySQL Data Bridge you just created from the **Data bridge** drop-down list. Click the **Add** button at the bottom to include this action in the rule.

12. Back on the **Create Rule** page, verify the configured information. Click the **Create** button to generate the rule. The rule you created is shown in the rule list and the **status** should be connected.

Now you have successfully created the data bridge to MySQL. You can click **Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `t/#`  are sent and saved to MySQL after parsing by the rule `my_rule`. 

### Event Recording

The steps for creating the data bridge for event recording are the same as those for message storage, except for the configured rule and SQL template for inserting the data.

To create a rule for online/offline status recording, you can enter the following statement in the **SQL Editor**:

```sql
SELECT
  *
FROM
  "$events/client_connected", "$events/client_disconnected"
```

To insert the client events data to the data table, you can use the following SQL template:

```sql
INSERT INTO emqx_client_events(clientid, event, created_at) VALUES (
  ${clientid},
  ${event},
  FROM_UNIXTIME(${timestamp}/1000)
)
```

## Test Data Bridge and Rule

Use MQTTX  to send a message to topic  `t/1`  to trigger an online/offline event.

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello MySQL" }'
```

Check the running status of the two data bridges, there should be one new incoming and one new outgoing message.

Check whether the data is written into the `emqx_messages` data table.

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

## Advanced Configurations

This section delves deeper into the advanced configuration options available for the MySQL data bridge. When configuring the data bridge in the Dashboard, navigate to **Advanced Settings** to tailor the following parameters to meet your specific needs.

| **Fields**                | **Descriptions**                                             | **Recommended Value** |
| ------------------------- | ------------------------------------------------------------ | --------------------- |
| **Connection Pool Size**  | Specifies the number of concurrent connections that can be maintained in the connection pool when interfacing with the MySQL service. This option helps in managing the application's scalability and performance by limiting or increasing the number of active connections between EMQX and MySQL.<br/>**Note**: Setting an appropriate connection pool size depends on various factors such as system resources, network latency, and the specific workload of your application. Too large a pool size may lead to resource exhaustion, while too small a size may limit throughput. | `8`                   |
| **Start Timeout**         | Determines the maximum time interval, in seconds, that the EMQX data bridge will wait for an auto-started resource to reach a healthy state before responding to resource creation requests. This setting helps ensure that the data bridge does not proceed with operations until it verifies that the connected resource—such as a database instance in MySQL—is fully operational and ready to handle data transactions. | `5` second            |
| **Buffer Pool Size**      | Specifies the number of buffer worker processes that will be allocated for managing data flow in egress-type bridges between EMQX and MySQL. These worker processes are responsible for temporarily storing and handling data before it is sent to the target service. This setting is particularly relevant for optimizing performance and ensuring smooth data transmission in egress (outbound) scenarios. For bridges that only deal with ingress (inbound) data flow, this option can be set to "0" as it is not applicable. | `16`                  |
| **Request TTL**           | The "Request TTL" (Time To Live) configuration setting specifies the maximum duration, in seconds, that a request is considered valid once it enters the buffer. This timer starts ticking from the moment the request is buffered. If the request stays in the buffer for a period exceeding this TTL setting or if it is sent but does not receive a timely response or acknowledgment from MySQL, the request is deemed to have expired. | `45` second           |
| **Health Check Interval** | Specifies the time interval, in seconds, at which the data bridge will perform automated health checks on the connection to MySQL. | `15` second           |
| **Max Buffer Queue Size** | Specifies the maximum number of bytes that can be buffered by each buffer worker in the MySQL data bridge. Buffer workers temporarily store data before it is sent to MySQL, serving as an intermediary to handle data flow more efficiently. Adjust the value according to your system's performance and data transfer requirements. | `256` MB              |
| **Max Batch Size**        | Specifies the maximum size of data batches transmitted from EMQX to MySQL in a single transfer operation. By adjusting the size, you can fine-tune the efficiency and performance of data transfer between EMQX and MySQL.<br />If the "Max Batch Size" is set to "1," data records are sent individually, without being grouped into batches. | `1`                   |
| **Query Mode**            | Allows you to choose `asynchronous` or `synchronous` query modes to optimize message transmission based on different requirements. In asynchronous mode, writing to MySQL does not block the MQTT message publish process. However, this might result in clients receiving messages ahead of their arrival in MySQL. | `Async`               |
| **Inflight Window**       | An "in-flight query" refers to a query that has been initiated but has not yet received a response or acknowledgment. This setting controls the maximum number of in-flight queries that can exist simultaneously when the data bridge is communicating with MySQL.<br/>When the **Query Mode** is set to `async` (asynchronous), the "Inflight Window" parameter gains special importance. If it is crucial for messages from the same MQTT client to be processed in strict order, you should set this value to 1. | `100`                 |

## More Information

Check out the following link to learn more:

[MQTT Performance Benchmark Testing: EMQX-MySQL Integration](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-mysql-integration)
