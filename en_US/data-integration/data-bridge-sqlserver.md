# Ingest MQTT Data into Microsoft SQL Server

{% emqxce %}
::: tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}

[SQL Server](https://www.microsoft.com/en-us/sql-server/) is one of the leading relational commercial database solutions, widely used in enterprises and organizations of various sizes and types. EMQX supports integration with SQL Server, enabling you to save MQTT messages and client events to SQL Server. This facilitates the construction of complex data pipelines and analytical processes for data management and analysis, or for managing device connections and integrating with other enterprise systems such as ERP, CRM, and BI.

This page provides a comprehensive introduction to the data integration between EMQX and Microsoft SQL Server with practical instructions on creating a rule and data bridge.

{% emqxee %}

::: tip

The data integration with Microsoft SQL Server is supported in EMQX Enterprise 5.0.3 and above.

:::

{% endemqxee %}

## How It Works

Microsoft SQL Server data integration is an out-of-the-box feature in EMQX, combining EMQX's device connectivity and message transmission capabilities with the powerful data storage capabilities of Microsoft SQL Server. Through the built-in [rule engine](./rules.md) component and data bridge, you can store MQTT messages and client events in Microsoft SQL Server. Additionally, events can trigger updates or deletions of data within Microsoft SQL Server, enabling the recording of information such as device online status and connection history. This integration simplifies the process of ingesting data from EMQX to SQL Server for storage and management, eliminating the need for complex coding.

The diagram below illustrates a typical architecture of data integration between EMQX and SQL Server:

![EMQX Integration SQL Server](./assets/emqx-integration-sql_server.png)

Ingesting MQTT data into Microsoft SQL Server works as follows:

1. **Message publication and reception**: Industrial IoT devices establish successful connections to EMQX through the MQTT protocol and publish real-time MQTT data from machines, sensors, and product lines based on their operational states, readings, or triggered events to EMQX. When EMQX receives these messages, it initiates the matching process within its rules engine.  
2. **Message data processing:** When a message arrives, it passes through the rule engine and is then processed by the rule defined in EMQX. The rules, based on predefined criteria, determine which messages need to be routed to Microsoft SQL Server. If any rules specify payload transformations, those transformations are applied, such as converting data formats, filtering out specific information, or enriching the payload with additional context.
3. **Data ingestion into SQL Server**: The rule triggers the writing of messages to Microsoft SQL Server. With the help of SQL templates, users can extract data from the rule processing results to construct SQL and send it to SQL Server for execution, so that specific fields of the message can be written or updated into the corresponding tables and columns of the database.
4. **Data Storage and Utilization**: With the data now stored in Microsoft SQL Server, businesses can harness its querying power for various use cases.

## Features and Benefits

The data integration with Microsoft SQL Server offers a range of features and benefits tailored to ensure efficient data transmission, storage, and utilization:

- **Real-time Data Streaming**: EMQX is built for handling real-time data streams, ensuring efficient and reliable data transmission from source systems to Microsoft SQL Server. It enables organizations to capture and analyze data in real-time, making it ideal for use cases requiring immediate insights and actions.
- **High Performance and Scalability**: Both EMQX and Microsoft SQL Server feature expandability and reliability, suitable for handling large-scale IoT data. They can undergo uninterrupted horizontal and vertical expansion as demands grow, ensuring the continuity and reliability of IoT applications.
- **Flexibility in Data Transformation:** EMQX provides a powerful SQL-based Rule Engine, allowing organizations to pre-process data before storing it in Microsoft SQL Server. It supports various data transformation mechanisms, such as filtering, routing, aggregation, and enrichment, enabling organizations to shape the data according to their needs.
- **Advanced Analytics:** Microsoft SQL Server offers powerful analytical capabilities, such as building multi-dimensional data models through Analysis Services to support complex data analysis and data mining. It also enables the creation and publication of reports through Reporting Services, presenting insights and analysis results of IoT data to stakeholders.

## Before You Start

This section describes the preparations you need to complete before you start to create the Microsoft SQL Server data bridges, including how to install and connect to the Microsoft SQL Server, create database and data tables, and install and configure the ODBC driver.

### Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [Data Integration](./data-bridges.md)

### Install and Connect to Microsoft SQL Server

This section describes how to start Microsoft SQL Server 2019 on Linux/MacOS using Docker images and use `sqlcmd` to connect to Microsoft SQL Server. For other installation methods of Microsoft SQL Server, please refer to [Microsoft SQL Server Installation Guide](https://learn.microsoft.com/en-us/sql/database-engine/install-windows/install-sql-server?view=sql-server-ver16).

1. Install Microsoft SQL Server via Docker, and then start the docker image with the command below. Use `mqtt_public1` as the password. For the password policy of Microsoft SQL Server, see [Password Complexity](https://learn.microsoft.com/en-us/sql/relational-databases/security/password-policy?view=sql-server-ver16#password-complexity).

   Note: By starting a Docker container with the environment variable `ACCEPT_EULA=Y` you agree to the terms of Microsoft EULA, see also [MICROSOFT SOFTWARE LICENSE TERMS MICROSOFT SQL SERVER 2019 STANDARD(EN_US)](https://www.microsoft.com/en-us/Useterms/Retail/SQLServerStandard/2019/Useterms_Retail_SQLServerStandard_2019_English.htm).

   ```bash
   # To start the Microsoft SQL Server docker image and set the password as `mqtt_public1`
   $ docker run --name sqlserver -p 1433:1433 -e ACCEPT_EULA=Y -e MSSQL_SA_PASSWORD=mqtt_public1 -d mcr.microsoft.com/mssql/server:2019-CU19-ubuntu-20.04
   ```

2. Access the container.

   ```bash
   docker exec -it sqlserver bash
   ```

3. Enter the preset password to connect to the server in the container. The characters are not echoed when entering the password. Click `Enter` directly after entering the password.

   ```bash
   $ /opt/mssql-tools/bin/sqlcmd -S 127.0.0.1 -U sa
   $ Password:
   1>
   ```

   ::: tip

   The `mssql-tools` have been installed in the Microsoft SQL Server container provided by Microsoft, but the executable file is not in `$PATH`. Therefore, you need to specify the executable file path for `mssql-tools` before proceeding. As for the Docker deployment in this example, the file path should be `opt`.

   For more information on how to use `mssql-tools`, see [sqlcmd-utility](https://learn.microsoft.com/en-us/sql/tools/sqlcmd/sqlcmd-utility?view=sql-server-ver16).

   :::

So far, the Microsoft SQL Server 2019 instance has been deployed and can be connected.

### Create Database and Data Tables

This section describes how to create a database and data table in Microsoft SQL Server.

1. Create a database `mqtt` in Microsoft SQL Server using the connection created from the previous section.

   ```bash
   ...
   Password:
   1> USE master
   2> GO
   Changed database context to 'master'.
   1> IF NOT EXISTS(SELECT name FROM sys.databases WHERE name = 'mqtt') BEGIN CREATE DATABASE mqtt END
   2> GO
   ```

2. Use the following SQL statements to create a data table.

   - Create the following data table for storing the MQTT message, including the message ID, topic, QoS, payload, and publish time of each message.

     ```sql
     CREATE TABLE mqtt.dbo.t_mqtt_msg (id int PRIMARY KEY IDENTITY(1000000001,1) NOT NULL,
                                       msgid   VARCHAR(64) NULL,
                                       topic   VARCHAR(100) NULL,
                                       qos     tinyint NOT NULL DEFAULT 0,
                                       payload VARCHAR(100) NULL,
                                       arrived DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP);
     GO
     ```

   - Create the following data table for recording the online/offline status of clients.

     ```sql
     CREATE TABLE mqtt.dbo.t_mqtt_events (id int PRIMARY KEY IDENTITY(1000000001,1) NOT NULL,
                                          clientid VARCHAR(255) NULL,
                                          event_type VARCHAR(255) NULL,
                                          event_time DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP);
     GO
     ```

### Install and Configure ODBC Driver

You need to configure the ODBC driver to be able to access the Microsoft SQL Server database. You can use either FreeTDS or the msodbcsql17 driver provided by Microsoft as the ODBC driver (The connection properties for msodbcsql18 have not been adapted yet).

EMQX uses the DSN Name specified in the `odbcinst.ini` configuration to determine the path to the driver dynamic library. In the examples below, the DSN Name is `ms-sql`. For more information, refer to [Connection Properties](https://learn.microsoft.com/en-us/sql/connect/odbc/linux-mac/connection-string-keywords-and-data-source-names-dsns?view=sql-server-ver16#connection-properties).

::: tip Note:

You can choose your own DSN name according to your preference, but it is recommended to use only English letters. Additionally, the DSN Name is case-sensitive.

:::

#### Install and Configure msodbcsql17 Driver as ODBC Driver

<!-- TODO: update tag version in command and dockerfile -->

If you need to use msodbcsql17 driver as the ODBC driver, refer to the Microsoft instructions:

- [Install the Microsoft ODBC driver for SQL Server (Linux)](https://learn.microsoft.com/en-us/sql/connect/odbc/linux-mac/installing-the-microsoft-odbc-driver-for-sql-server?view=sql-server-ver16&tabs=alpine18-install%2Calpine17-install%2Cdebian8-install%2Credhat7-13-install%2Crhel7-offline)
- [Install the Microsoft ODBC driver for SQL Server (macOS)](https://learn.microsoft.com/en-us/sql/connect/odbc/linux-mac/install-microsoft-odbc-driver-sql-server-macos?view=sql-server-ver16)

Restricted by [Microsoft EULA terms](https://www.microsoft.com/en-us/Useterms/Retail/SQLServerStandard/2019/Useterms_Retail_SQLServerStandard_2019_English.htm), the Docker image provided by EMQX does not include the msodbcsql17 driver. To use it in Docker or Kubernetes, you need to create a new image installed with ODBC driver based on the image provided by [EMQX-Enterprise](https://hub.docker.com/r/emqx/emqx-enterprise) to access the Microsoft SQL Server database. Using the new image means that you agree to the [Microsoft SQL Server EULA](https://www.microsoft.com/en-us/Useterms/Retail/SQLServerStandard/2019/Useterms_Retail_SQLServerStandard_2019_English.htm).

Follow the instructions below to build a new image:

1. Get the corresponding EMQX [Dockerfile](https://github.com/emqx/emqx/blob/master/deploy/docker/Dockerfile.msodbc). You can save the file locally.

   The image version in this example is `emqx/emqx-enterprise:5.0.3-alpha.2`. You can build the image based on the EMQX-Enterprise version you need, or use the latest version image `emqx/emqx-enterprise:latest`.

   ```docker
   # FROM emqx/emqx-enterprise:latest
   FROM emqx/emqx-enterprise:5.0.3-alpha.2

   USER root

   RUN apt-get update \
       && apt-get install -y gnupg2 curl apt-utils \
       && curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add - \
       && curl https://packages.microsoft.com/config/debian/11/prod.list > /etc/apt/sources.list.d/mssql-release.list \
       && apt-get update \
       && ACCEPT_EULA=Y apt-get install -y msodbcsql17 unixodbc-dev \
       && sed -i 's/ODBC Driver 17 for SQL Server/ms-sql/g' /etc/odbcinst.ini \
       && apt-get clean \
       && rm -rf /var/lib/apt/lists/*

   USER emqx
   ```

2. Build a new image using the command `docker build -f=Dockerfile.msodbc -t emqx-enterprise-with-msodbc:5.0.3-alpha.2 .`

3. After building, you can use `docker image ls` to obtain a list of local images. You can also upload or save the image for later use.

::: tip

Check that the DSN Name in `odbcinst.ini` should be `ms-sql` if you install the msodbcsql17 driver using this example. You can change the DSN Name according to your needs.

:::

#### Install and Configure FreeTDS as ODBC driver

This section introduces how to install and configure FreeTDS as an ODBC driver on some of the mainstream distributions.

Install and configure FreeTDS ODBC driver on MacOS:
```bash
$ brew install unixodbc freetds
$ vim /usr/local/etc/odbcinst.ini
# add the following lines
[ms-sql]
Description = ODBC for FreeTDS
Driver      = /usr/local/lib/libtdsodbc.so
Setup       = /usr/local/lib/libtdsodbc.so
FileUsage   = 1
```

Install and configure FreeTDS ODBC driver on Centos:
```bash
$ yum install unixODBC unixODBC-devel freetds freetds-devel perl-DBD-ODBC perl-local-lib
$ vim /etc/odbcinst.ini
# add the following lines
[ms-sql]
Description = ODBC for FreeTDS
Driver      = /usr/lib64/libtdsodbc.so
Setup       = /usr/lib64/libtdsS.so.2
Driver64    = /usr/lib64/libtdsodbc.so
Setup64     = /usr/lib64/libtdsS.so.2
FileUsage   = 1
```

Install and configure FreeTDS ODBC driver on Ubuntu (Take Ubuntu20.04 as an example, for other versions, please refer to the official ODBC documentation):
```bash
$ apt-get install unixodbc unixodbc-dev tdsodbc freetds-bin freetds-common freetds-dev libdbd-odbc-perl liblocal-lib-perl
$ vim /etc/odbcinst.ini
# add the following lines
[ms-sql]
Description = ODBC for FreeTDS
Driver      = /usr/lib/x86_64-linux-gnu/odbc/libtdsodbc.so
Setup       = /usr/lib/x86_64-linux-gnu/odbc/libtdsS.so
FileUsage   = 1
```

## Create Microsoft SQL Server Data Bridge

This section demonstrates how to create Microsoft SQL Server data bridge for message storage and events recording. It assumes that you run both EMQX and Microsoft SQL Server on the local machine. If you have Microsoft SQL Server and EMQX running remotely, adjust the settings accordingly.

1. Go to EMQX Dashboard, and click **Integration** -> **Connector**.

2. Click **Create** on the top right corner of the page.

3. In the **Create Connector** page, click to select **Microsoft SQL Server**, and then click **Next**.

4. Input a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.

5. Input the connection information:

   - **Server Host**: Input **127.0.0.1:1433**, or the URL if the Microsoft SQL Server is running remotely.
   - **Database Name**: Input `mqtt`.
   - **Username**: Input `sa`.
   - **Password**: Input preset password `mqtt_public1`, or use the actual password.
   - **SQL Server Driver Name**: Input `ms-sql`, as the DSN Name configured in `odbcinst.ini`

6. Configure the **SQL Template** based on the feature to use:

   - To configure the SQL template for message storage, use the following SQL statement:

     Note: This is a preprocessed SQL, so the fields should not be enclosed in quotation marks, and do not write a semicolon at the end of the statements.

     ```sql
     insert into t_mqtt_msg(msgid, topic, qos, payload) values ( ${id}, ${topic}, ${qos}, ${payload} )
     ```

   - To configure the SQL template for online/offline status recording, use the following SQL statement:

     ```sql
     insert into t_mqtt_events(clientid, event_type, event_time) values ( ${clientid}, ${event}, DATEADD(MS, ${ms_shift}, DATEADD(S, ${s_shift}, '19700101 00:00:00:000') ) )
     ```

7. Advanced settings (optional):  Choose whether to use **sync** or **async** query mode as needed. For details, see [Configuration](./data-bridges.md).

8. Before clicking **Create**, you can click **Test Connectivity** to test that the bridge can connect to the Microsoft SQL Server.

9. Click **Create** to finish the creation of the data bridge.

   A confirmation dialog will appear and ask if you like to create a rule using this data bridge, you can click **Create Rule** to continue creating rules to specify the data to be saved into Microsoft SQL Server. You can also create rules by following the steps in [Create Rules for Microsoft SQL Server Data Bridge](#create-rules-for-sqlserver-data-bridge).

Now that you have created the data bridge, and the Microsoft SQL Server data bridge should appear in the data bridge list (**Integration** -> **Data Bridge**) with **Resource Status** as **Connected**.


## Create Rules for Microsoft SQL Server Data Bridge

After you have successfully created the data bridge to Microsoft SQL Server, you can continue to create rules to specify the data to be saved into Microsoft SQL Server and rules for the online/offline status recording.

1. Go to EMQX Dashboard, click **Integration** -> **Rules**.

2. Click **Create** on the top right corner of the page.

3. Input `my_rule` as the rule ID, and set the rules in the **SQL Editor** based on the feature to use:

   - To create a rule for message storage, input the following statement, which means the MQTT messages under topic `t/#`  will be saved to Microsoft SQL Server.

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
       *,
       floor(timestamp / 1000) as s_shift,
       timestamp div 1000 as ms_shift
     FROM
       "$events/client_connected", "$events/client_disconnected"
     ```

4. Click the **Add Action** button, select **Forwarding with Data Bridge** from the dropdown list, and then select the data bridge we just created under **Data Bridge**. Click the **Add** button.

5. Click the **Create** button to finish the setup.

Now you have successfully created the rule for Microsoft SQL Server data bridge. You can click **Integration** -> **Flow Designer** to view the topology. It can be seen that the messages under topic `t/#`  are sent and saved to Microsoft SQL Server after parsing by rule  `my_rule`.

## Test Data Bridge and Rule

Use MQTT X  to send a message to topic  `t/1`  to trigger an online/offline event.

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello SQL Server" }'
```

Check the running statistics of the Microsoft SQL Server data bridges.

- For the data bridge used to store messages, there should be one new matching and one new outgoing message. Check whether the data is written into the `mqtt.dbo.t_mqtt_msg` data table.

```bash
1> SELECT * from mqtt.dbo.t_mqtt_msg
2> GO
id          msgid                                                            topic                                                                                                qos payload                                                                                              arrived
----------- ---------------------------------------------------------------- ---------------------------------------------------------------------------------------------------- --- ---------------------------------------------------------------------------------------------------- -----------------------
 1000000001 0005F995096D9466F442000010520002                                 t/1                                                                                                    0 { "msg": "Hello SQL Server" }                                                                        2023-04-18 04:49:47.170

(1 rows affected)
1>
```

- For the data bridge used to record online/offline status, there should be two new events recorded: client connected and client disconnected. Check whether the status recording is written into the `mqtt.dbo.t_mqtt_events` data table.

```bash
1> SELECT * from mqtt.dbo.t_mqtt_events
2> GO
id          clientid                                                         event_type                                                                                                                                                                                                    event_time
----------- ---------------------------------------------------------------- ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- -----------------------
 1000000001 emqx_c                                                           client.connected                                                                                                                                                                                              2023-04-18 04:49:47.140
 1000000002 emqx_c                                                           client.disconnected                                                                                                                                                                                           2023-04-18 04:49:47.180

(2 rows affected)
1>
```
