# Ingest Data into SQL Server

EMQX supports integration with SQL Server. You can save client messages and events to SQL Server, or record the online status or online/offline of clients by using events to trigger the data update or removal.

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
- [Batch mode](./data-bridges.md)
- [Buffer mode](./data-bridges.md)

<!-- [Configuration parameters](#Configuration) TODO 链接到配置手册对应配置章节。 -->

## Quick Start Tutorial

This section introduces how to configure the SQL Server data bridge, covering topics like how to set up the SQL Server server, create data bridges and rules for forwarding data to SQL Server and test the data bridges and rules.

This tutorial assumes that you run both EMQX and SQL Server on the local machine. If you have SQL Server and EMQX running remotely, adjust the settings accordingly.

### 1. Install SQL Server Server

This section describes how to start SQL Server 2019 on Linux/MacOS installation using Docker images.
And use `sqlcmd` to connect to SQL Server and create databases and data tables.
For other installation methods of SQL Server, please refer to [SQL Server Installation Guide](https://learn.microsoft.com/en-us/sql/database-engine/install-windows/install-sql-server?view=sql-server-ver16).

#### 2. Install SQL Server server via Docker, and then run the docker image.

SQL Server requires using complex passwords, see also [Password Complexity](https://learn.microsoft.com/en-us/sql/relational-databases/security/password-policy?view=sql-server-ver16#password-complexity)</br>
By starting a Docker container with the environment variable `ACCEPT_EULA=Y` you agree to the terms of Microsoft's EULA, see also
[MICROSOFT SOFTWARE LICENSE TERMS MICROSOFT SQL SERVER 2019 STANDARD(EN_US)](https://www.microsoft.com/en-us/Useterms/Retail/SQLServerStandard/2019/Useterms_Retail_SQLServerStandard_2019_English.htm)
```bash
# To start the SQL Server docker image and set the password as `mqtt_public1`
$ docker run --name sqlserver -p 1433:1433 -e ACCEPT_EULA=Y -e SQLSERVER_ROOT_PASSWORD=mqtt_public1 -d mcr.microsoft.com/mssql/server:2019-CU19-ubuntu-20.04
```

#### 3. Access the container
```
docker exec -it sqlserver bash
```

#### 4. To connect to the SQL Server server in the container, you need to enter the preset password.
`mssql-tools` has been installed in the SQL Server container provided by Microsoft, but the executable file is not in `$PATH`, and the executable file path needs to be specified.
For security reasons, the characters are not echoed when entering the password. Please enter the password and directly type `Enter`.
For more usage of `mssql-tools`, please refer to related documents provided by Microsoft: [sqlcmd-utility](https://learn.microsoft.com/en-us/sql/tools/sqlcmd/sqlcmd-utility?view=sql-server-ver16).
```
$ /opt/mssql-tools/bin/sqlcmd -S 127.0.0.1 -U sa
$ Password:
1>
```

So far, the SQL Server 2019 instance has been deployed and can be connected.

### Use EMQX Data Bridge connect to SQL Server and write mqtt message

In this section we will create a SQL Server data bridge to store client-published messages.

#### 1. First create the database `mqtt` in SQL Server using this connection from the previous section.
```
...
Password:
1> USE master
2> GO
Changed database context to 'master'.
1> IF NOT EXISTS(SELECT name FROM sys.databases WHERE name = 'mqtt') BEGIN CREATE DATABASE mqtt END
2> GO
```

#### 2. Use the follow sql statement to create a table in database `mqtt` to stroe the mqtt message.
This table stores the MsgID, Subject, QoS, Payload, and publish time of each message:
```sql
CREATE TABLE mqtt.dbo.t_mqtt_msg (id int PRIMARY KEY IDENTITY(1000000001,1) NOT NULL,
                                  msgid   VARCHAR(64) NULL,
                                  topic   VARCHAR(100) NULL,
                                  qos     tinyint NOT NULL DEFAULT 0,
                                  payload VARCHAR(100) NULL,
                                  arrived DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP);
GO
```

#### 3. Store MQTT Message

<!-- Data bridges for message storage and event recording require different SQL templates. Therefore, you need to create 2 different data bridges to SQL Server for messages storage and event recording. -->

1. Go to EMQX Dashboard, click **Data Integration** -> **Data Bridge**.

2. Click **Create** on the top right corner of the page.

3. In the **Create Data Bridge** page, click to select **SQL Server**, and then click **Next**.

4. Input a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.

5. Input the connection information:

   - **Server Host**: Input **127.0.0.1:1433**, or the actual URL if the SQL Server server is running remotely.
   - **Database Name**: Input `mqtt`.
   - **Username**: Input `sa`.
   - **Password**: Input preset password `mqtt_public1`, or use the actual password.

6. Configure the **SQL Template** based on the feature to use:

   Note: This is a preprocessed SQL, so the fields should not be enclosed in quotation marks, and do not write a semicolon at the end of the statements.
   ```sql
   insert into t_mqtt_msg(msgid, topic, qos, payload) values ( ${id}, ${topic}, ${qos}, ${payload} )
   ```

7. Configure odbc driver
You can use freetds or msodbcsql17 released by Microsoft as odbc driver.
To use the msodbcsql17 driver, please refer to [Install the Microsoft ODBC driver for SQL Server (Linux)](https://learn.microsoft.com/en-us/sql/connect/odbc/linux-mac/installing-the-microsoft-odbc-driver-for-sql-server?view=sql-server-ver16&tabs=alpine18-install%2Calpine17-install%2Cdebian8-install%2Credhat7-13-install%2Crhel7-offline)

Configure FreeTDS odbc driver on MacOS:
```
$ brew install unixodbc freetds
$ vim /usr/local/etc/odbcinst.ini
# add the following lines
[ms-sql]
Description = ODBC for FreeTDS
Driver      = /usr/local/lib/libtdsodbc.so
Setup       = /usr/local/lib/libtdsodbc.so
FileUsage   = 1
```

Configure FreeTDS odbc driver configuration on Centos:
```
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

Configure FreeTDS odbc driver configuration on Ubuntu (Take Ubuntu20.04 as an example, for other versions, please refer to the official odbc documentation):
```
$ apt-get install unixodbc unixodbc-dev tdsodbc freetds-bin freetds-common freetds-dev libdbd-odbc-perl liblocal-lib-perl
$ vim /etc/odbcinst.ini
# add the following lines
[ms-sql]
Description = ODBC for FreeTDS
Driver      = /usr/lib/x86_64-linux-gnu/odbc/libtdsodbc.so
Setup       = /usr/lib/x86_64-linux-gnu/odbc/libtdsS.so
FileUsage   = 1
```

8. Advanced settings (optional):  Choose whether to use **sync** or **async** query mode as needed. For details, see [Configuration](./data-bridges.md).

9. Before clicking **Create**, you can click **Test Connectivity** to test that the bridge can connect to the SQL Server.

10. Click **Create** to finish the creation of the data bridge.

   A confirmation dialog will appear and ask if you like to create a rule using this data bridge, you can click **Create Rule** to continue creating rules to specify the data to be saved into SQL Server. You can also create rules by following the steps in [Create Rules for SQL Server Data Bridge](#create-rules-for-sqlserver-data-bridge).

Now that you have created the data bridge, and the SQL Server data bridge should appear in the data bridge list (**Data Integration** -> **Data Bridge**) with **Resource Status** as **Connected**.

You will continue to create a rule to specify the data that needs to be written:


### Create Rules for SQL Server Data Bridge

After you have successfully created the data bridge to SQL Server, you can continue to create rules to specify the data to be saved into SQL Server and rules for the online/offline status recording.

1. Go to EMQX Dashboard, click **Data Integration** -> **Rules**.

2. Click **Create** on the top right corner of the page.

3. Input `my_rule` as the rule ID, and set the rules in the **SQL Editor** based on the feature to use:

   - To create a rule for message storage, input the following statement, which means the MQTT messages under topic `t/#`  will be saved to SQL Server.

     Note: If you want to specify your own SQL syntax, make sure that you have included all fields required by the data bridge in the `SELECT` part.

     ```sql
     SELECT
       *
     FROM
       "t/#"
     ```

4. Click the **Add Action** button, select **Forwarding with Data Bridge** from the dropdown list and then select the data bridge we just created under **Data Bridge**. Click the **Add** button.

5. Click the **Create** button to finish the setup.

Now you have successfully created the rule for SQL Server data bridge. You can click **Data Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `t/#`  are sent and saved to SQL Server after parsing by rule  `my_rule`.

### Test the Data Bridge and Rule

Use MQTT X  to send a message to topic  `t/1`  to trigger an online/offline event.

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello SQL Server" }'
```

Check the running status of the SQL Server data bridges, there should be one new matching and one new outgoing message.

Check whether the data is written into the `mqtt.dbo.t_mqtt_msg` data table.

```bash
1> SELECT * from mqtt.dbo.t_mqtt_msg
2> GO
id          msgid                                                            topic                                                                                                qos payload                                                                                              arrived
----------- ---------------------------------------------------------------- ---------------------------------------------------------------------------------------------------- --- ---------------------------------------------------------------------------------------------------- -----------------------
 1000000002 0005F995096D9466F442000010520002                                 t/1                                                                                                    0 { "msg": "Hello SQL Server" }                                                                        2023-04-18 04:49:47.170

(1 rows affected)
1>
```
