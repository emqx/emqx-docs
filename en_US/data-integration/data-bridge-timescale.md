# Ingest MQTT Data into TimescaleDB

[TimescaleDB](https://www.timescale.com/) (Timescale) is a database specifically designed for storing and analyzing time-series data. Its exceptional data throughput and reliable performance make it an ideal choice for the Internet of Things (IoT) domain, providing efficient and scalable data storage and analysis solutions for IoT applications.

{% emqxce %}

::: tip

EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.

:::

{% endemqxce %}

::: tip Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridge](./data-bridges.md)

:::

## Features

- [Connection pool](./data-bridges.md#connection-pool)
- [Async mode](./data-bridges.md#async-mode)
- [Batch mode](./data-bridges.md#batch-mode)
- [Buffer queue](./data-bridges.md#buffer-queue)
- [SQL prepared statement](./data-bridges.md#prepared-statement)

## Quick Start Tutorial

This section introduces how to install Timescale and create a data table, create a rule and data bridge for forwarding data to Timescale, and test the rule and data bridge.

The instructions below assume that you run both EMQX and Timescale (if self-deployed) on the local machine. If you have Timescale and EMQX running remotely, adjust the settings accordingly.

### Install Timescale and Create Data Table

EMQX supports integration with self-deployed TimescaleDB or Timescale Service on the cloud. You can use Timescale Service as a cloud service or deploy a TimescaleDB instance using Docker.

:::: tabs 
::: tab Timescale Service

1. If you do not have a Timescale account, create an account by referring to [Create your Timescale account](https://docs.timescale.com/getting-started/latest/services/#create-your-timescale-account).

2. Log in to Timescale portal and [Create Timescale service](https://docs.timescale.com/getting-started/latest/services/#create-your-first-service). Save the password for your service.

3. Get the connection information from the service overview page. The fields required by EMQX include **Database name**, **Host**, **Port,** and **Username**.

4. [Connect to service](https://docs.timescale.com/getting-started/latest/services/#connect-to-your-service) with `psql client`.

   ```bash
   # Connect to service by service URL
   psql "postgres://tsdbadmin@xxxxx.xxxxx.tsdb.cloud.timescale.com:32541/tsdb?sslmode=require"
   # Use password in you saved in previous step
   Password for user tsdbadmin:
   ```

5. Create a table `sensor_data` to save the data in the message from the client.

   ```sql
   CREATE TABLE sensor_data (
       time        TIMESTAMPTZ       NOT NULL,
       location    TEXT              NOT NULL,
       temperature DOUBLE PRECISION  NULL,
       humidity    DOUBLE PRECISION  NULL
   );
   
   SELECT create_hypertable('sensor_data', 'time');
   ```

After the table is successfully created, you can view the information of the table `sensor_data` under the **Explorer** tab in Services.

![Timescale Explorer table](./assets/timescale-explorer-table.png)

:::

::: tab TimescaleDB Docker

1. If you do not Docker environment, refer to [Install Docker](https://docs.docker.com/install/).

2. Create a TimescaleDB container with Docker, and set the password of the database by `POSTGRES_PASSWORD` environment variable.

   ```bash
   docker run -d --name timescaledb \
       -p 5432:5432 \
       -e POSTGRES_PASSWORD=public \
       timescale/timescaledb:latest-pg13
   ```

3. Create a database to save the client data.

   ```bash
   docker exec -it timescaledb psql -U postgres
   
   ## create tsdb database
   > CREATE database tsdb;
   
   > \c tsdb;
   ```

4. Create a table `sensor_data` to save the data in the message from the client.

   ```sql
   CREATE TABLE sensor_data (
       time        TIMESTAMPTZ       NOT NULL,
       location    TEXT              NOT NULL,
       temperature DOUBLE PRECISION  NULL,
       humidity    DOUBLE PRECISION  NULL
   );
   
   SELECT create_hypertable('sensor_data', 'time');
   ```

:::
::::

### Create Rule and Data Bridge

1. Go to EMQX Dashboard and click **Integration** -> **Rules** from the left navigation menu.

2. Click **+ Create** on the top right corner of the page.

3. Input a rule ID `my_rule`. Input the following SQL rule in **SQL Editor** to save the MQTT message with the topic `t/#` to TimescaleDB：

   ```sql
   SELECT
     payload.temp as temp,
     payload.humidity as humidity,
     payload.location as location
   FROM
       "t/#"
   ```

4. Click **+ Add Action**. Select `Forwarding with Data Bridge` from the **Action** drop-down list. Click **+** beside the **Data bridge** dropdown to enter the **Create Data Bridge** pop-up page. 

5. Select `Timescale` from the **Type of Data Bridge** drop-down list. Input a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.

6. Input the connection information according to how the TimescaleDB is deployed. If it is deployed using Docker, input `127.0.0.1:5432` as **Server Host**, `tsdb` as **Database Name**,  `postgres` as **Username,** and `public` as **Password**.

7. Configure the **SQL Template** using the following SQL statement for data inserting.

   Note: This is a preprocessed SQL, so the fields should not be enclosed in quotation marks, and do not write a semicolon at the end of the statements.

   ```sql
     INSERT INTO
    sensor_data (time, location, temperature, humidity)
     VALUES
      (NOW(), ${location}, ${temp}, ${humidity})
   ```

8. Advanced settings (optional):  Choose whether to use **sync** or **async** query mode as needed.

9. Click **Add** to finish the data bridge creation and return to the **Add Actions** page. Click **+ Add** to add the Timescale data bridge to the rule action. 

10. Click **Create** to finish the rule creation.

Now you have successfully created the data bridge to Timescale. You can click **Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `t/#`  are sent and saved to Timescale after parsing by the rule `my_rule`. 


### Test Data Bridge and Rule

Use MQTTX to send a message to topic `t/1` and trigger an online/offline event at the same time：

```bash
mqttx pub -i emqx_c -t t/1 -m '{"temp":24,"humidity":30,"location":"hangzhou"}'
```

Check the running status of the data bridge, there should be one new Matched and one Sent Successfully message.

Verify the Timescale table `sensor_data`. New records should be inserted:

```bash
tsdb=# select * from sensor_data;
             time              | location | temperature | humidity 
-------------------------------+----------+-------------+----------
 2023-07-10 08:28:48.813988+00 | hangzhou |          24 |       30
 2023-07-10 08:28:57.737768+00 | hangzhou |          24 |       30
 2023-07-10 08:28:58.599537+00 | hangzhou |          24 |       30
(3 rows)
```