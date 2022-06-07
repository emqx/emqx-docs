# Save data to SQLServer

Set up a SQLServer database and set the user name and password to sa/mqtt_public. Take MacOS X as an example:

```bash
docker run -e 'ACCEPT_EULA=Y' -e 'SA_PASSWORD=mqtt_public' -p 1433:1433 -d mcr.microsoft.com/mssql/server:2017-latest
```

Enter the SQLServer container and initialize the SQLServer table:

```bash
$ /opt/mssql-tools/bin/sqlcmd -S localhost -U sa -P mqtt_public -d master
$ mysql -u root -h localhost -ppublic
```

Create the "test" database:
```bash
CREATE DATABASE test;
go;
```
Create the t_mqtt_msg table:

```sql
USE test;
go;
CREATE TABLE t_mqtt_msg (id int PRIMARY KEY IDENTITY(1000000001,1) NOT NULL,
                         msgid   VARCHAR(64) NULL,
                         topic   VARCHAR(100) NULL,
                         qos     tinyint NOT NULL DEFAULT 0,
                         payload NVARCHAR(100) NULL,
                         arrived DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP);
go;
```

Configure odbc driver in Mac:
```
$ brew install unixodbc freetds
$ vim /usr/local/etc/odbcinst.ini
[ms-sql]
Description = ODBC for FreeTDS
Driver      = /usr/local/lib/libtdsodbc.so
Setup       = /usr/local/lib/libtdsodbc.so
FileUsage   = 1
```

Configure odbc driver in CentOS:
```
$ yum install unixODBC unixODBC-devel freetds freetds-devel perl-DBD-ODBC perl-local-lib
$ vim /etc/odbcinst.ini
# add as below
[ms-sql]
Description = ODBC for FreeTDS
Driver      = /usr/lib64/libtdsodbc.so
Setup       = /usr/lib64/libtdsS.so.2
Driver64    = /usr/lib64/libtdsodbc.so
Setup64     = /usr/lib64/libtdsS.so.2
FileUsage   = 1
```

Configure odbc dirver in Ubuntu (Take Ubuntu 20.04 as an example, for other versions please refer to the odbc official documentation):
```
$ apt-get install unixodbc unixodbc-dev tdsodbc freetds-bin freetds-common freetds-dev libdbd-odbc-perl liblocal-lib-perl
$ vim /etc/odbcinst.ini
# add as below
[ms-sql]
Description = ODBC for FreeTDS
Driver      = /usr/lib/x86_64-linux-gnu/odbc/libtdsodbc.so
Setup       = /usr/lib/x86_64-linux-gnu/odbc/libtdsS.so
FileUsage   = 1
```

Create rules:

Open [EMQX Dashboard](http://127.0.0.1:18083/#/rules) and select the "Rules" tab on the left.

Fill in the rule SQL:

```sql
SELECT * FROM "t/#"
```

![image](./assets/rule-engine/sqlserver1.png)

Related actions:

On the "Response Action" interface, select "Add", and then select "Save Data to SQLServer" in the "Action" drop-down box.

![image](./assets/rule-engine/sqlserver2.png)

Fill in the action parameters:

The "Save data to SQLServer" action requires two parameters:
1). SQL template. In this example, we insert a piece of data into SQLServer, and the SQL template is:

```sql
insert into t_mqtt_msg(msgid, topic, qos, payload) values ('${id}', '${topic}', ${qos}, '${payload}')
```

![image](./assets/rule-engine/sqlserver4.png)

2). The ID of the associated resource. Now the resource drop-down box is empty, and you can click "New Resource" in the upper right corner to create a SQLServer resource:

Fill in the resource configuration:
Fill in “mqtt” for database name, “sa” for user name, and “mqtt_public” for password

![image](./assets/rule-engine/sqlserver3.png)

Click the "New" button.

Return to the response action interface and click "OK".

![image](./assets/rule-engine/sqlserver5.png)

Return to the rule creation interface and click "Create".

![image](./assets/rule-engine/sqlserver6.png)

In the rule list, click the "View" button or the rule ID connection to preview the rule you just created:

![image](./assets/rule-engine/sqlserver7.png)

The rule has been created. Now, send a piece of data:

```bash
Topic: "t/a"
QoS: 1
Payload: "hello"
```

Then check the SQLServer table to see whether the new record is added successfully:

![image](./assets/rule-engine/sqlserver8.png)
