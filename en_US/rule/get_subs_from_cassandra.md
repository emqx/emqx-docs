# Get subscription relationship from Cassandra

Set up the Cassandra database and set the user name and password to root/public. Take MacOS X as an example:
```bash
$ brew install cassandra
## Modify the configuration and disable anonymous authentication
$  vim /usr/local/etc/cassandra/cassandra.yaml

    authenticator: PasswordAuthenticator
    authorizer: CassandraAuthorizer

$ brew services start cassandra

## Create root user
$ cqlsh -ucassandra -pcassandra

create user root with password 'public' superuser;
```

Create the "mqtt" tablespace:
```bash
$ cqlsh -uroot -ppublic

CREATE KEYSPACE mqtt WITH replication = {'class': 'SimpleStrategy', 'replication_factor': '1'}  AND durable_writes = true;
```

Create the mqtt_sub table:

```sql

CREATE TABLE mqtt_sub (
    clientid text,
    topic text,
    qos int,
    PRIMARY KEY (clientid, topic)
) WITH CLUSTERING ORDER BY (topic ASC)
    AND bloom_filter_fp_chance = 0.01
    AND caching = {'keys': 'ALL', 'rows_per_partition': 'NONE'}
    AND comment = ''
    AND compaction = {'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4'}
    AND compression = {'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor'}
    AND crc_check_chance = 1.0
    AND dclocal_read_repair_chance = 0.1
    AND default_time_to_live = 0
    AND gc_grace_seconds = 864000
    AND max_index_interval = 2048
    AND memtable_flush_period_in_ms = 0
    AND min_index_interval = 128
    AND read_repair_chance = 0.0
    AND speculative_retry = '99PERCENTILE';

```

::: tip

The subscription relationship table structure cannot be modified. Please use the above SQL statement to create

:::

Create rules:

Open [EMQ X Dashboard](http://127.0.0.1:18083/#/rules) and select the "Rules" tab on the left.

Then fill in the rule SQL:

```bash
SELECT * FROM "$events/client_connected"
```

![](./assets/rule-engine/cass_sub_01.png)

Related actions:

Select "Add Action" on the "Response Action" interface, and then select "Get Subscription List from Cassandra" in the "Add Action" drop-down box

![](./assets/rule-engine/cass_sub_02.png)

Fill in the action parameters:

The action of "Get subscription list from Cassandra" requires one parameter:

1). Associated resources. The resource drop-down box is empty now, and you can click "New" in the upper right corner to create a Cassandra resource:

![](./assets/rule-engine/cass_sub_03.png)

The "Create Resource" dialog box pops up

![](./assets/rule-engine/cass_sub_04.png)

Fill in the resource configuration:

Fill in the real Cassandra server address and the values corresponding to other configurations, and then click the "Test Connection" button to ensure that the connection test is successful.

Finally click the "OK" button.

![](./assets/rule-engine/cass_sub_05.png)

Return to the response action interface and click "OK".

![](./assets/rule-engine/cass_sub_06.png)

Return to the rule creation interface and click "Create".

![](./assets/rule-engine/cass_sub_07.png)

The rule has been created, and you can insert a subscription relationship into Cassandra through "cqlsh":

```
insert into mqtt_sub(clientid, topic, qos) values('test', 't1', 1);
```

![](./assets/rule-engine/cass_sub_08.png)

Log in to the device whose clientid is test via Dashboard:

![](./assets/rule-engine/cass_sub_09.png)

Check the "Subscription" list, and you can see that the Broker obtains the subscription relationship from Cassandra and subscribes as the agent device:

![](./assets/rule-engine/cass_sub_10.png)
