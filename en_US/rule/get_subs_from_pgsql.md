---
enterprise: true
---
# Get subscription relationship from PostgreSQL 

Set up the PostgreSQL database, and take MacOS X as an example:
```bash
$ brew install postgresql
$ brew services start postgresql
```

Create the mqtt database:

```
# Create a database named 'mqtt' with the username postgres
$ createdb -U postgres mqtt

$ psql -U postgres mqtt

mqtt=> \dn;
List of schemas
Name  | Owner
--------+-------
public | postgres
(1 row)
```

Create the mqtt_sub table:

```sql
$ psql -U postgres mqtt
CREATE TABLE mqtt_sub(
  id SERIAL8 primary key,
  clientid character varying(64),
  topic character varying(255),
  qos integer,
  UNIQUE (clientid, topic)
);
```

::: danger

The subscription relationship table structure cannot be modified. Please use the above SQL statement to create

:::

Create rules:

Open [EMQ X Dashboard](http://127.0.0.1:18083/#/rules) and select the "Rules" tab on the left.

Then fill in the rule SQL:

```bash
SELECT * FROM "$events/client_connected"
```

![](./assets/rule-engine/pg_sub_01.png)

Related actions:

Select "Add Action" on the "Response Action" interface, and then select "Get Subscription List from PostgreSQL" in the "Add Action" drop-down box

![](./assets/rule-engine/pg_sub_02.png)

Fill in the action parameters:

The action of "Get subscription list from PostgreSQL" requires one parameter:

1). Associated resources. The resource drop-down box is empty now, and you can click "New" in the upper right corner to create a PostgreSQL resource:

![](./assets/rule-engine/pg_sub_03.png)

The "Create Resource" dialog box pops up

![](./assets/rule-engine/pg_sub_04.png)

Fill in the resource configuration:

Fill in the real PostgreSQL server address and the values corresponding to other configurations, and then click the "Test Connection" button to ensure that the connection test is successful.

Finally click the "OK" button.

![](./assets/rule-engine/pg_sub_05.png)

Return to the response action interface and click "OK".

![](./assets/rule-engine/pg_sub_06.png)

Return to the rule creation interface and click "Create".

![](./assets/rule-engine/pg_sub_07.png)

The rule has been created, and you can insert a subscription relationship into PostgreSQL through "psql":

```
insert into mqtt_sub(clientid, topic, qos) values('test', 't1', 1)'
```

![](./assets/rule-engine/pg_sub_08.png)

Log in to the device whose clientid is test via Dashboard:

![](./assets/rule-engine/pg_sub_09.png)

查看“订阅”列表，可以看到 Broker 从 PostgreSQL 里面获取到订阅关系，并代理设备订阅:

Check the "Subscription" list, and you can see that the Broker obtains the subscription relationship from PostgreSQL and subscribes as the agent device:

![](./assets/rule-engine/pg_sub_10.png)