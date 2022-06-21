# Save data to TimescaleDB

Setup a TimescaleDB database, taking MacOS X for instance:

```bash
$ docker pull timescale/timescaledb

$ docker run -d --name timescaledb -p 5432:5432 -e POSTGRES_PASSWORD=password timescale/timescaledb:latest-pg11

$ docker exec -it timescaledb psql -U postgres

## create tutorial database
> CREATE database tutorial;

> \c tutorial

> CREATE EXTENSION IF NOT EXISTS timescaledb CASCADE;
```

Initiate the TimescaleDB table:

```bash
$ docker exec -it timescaledb psql -U postgres -d tutorial

CREATE TABLE conditions (
    time        TIMESTAMPTZ       NOT NULL,
    location    TEXT              NOT NULL,
    temperature DOUBLE PRECISION  NULL,
    humidity    DOUBLE PRECISION  NULL
);

SELECT create_hypertable('conditions', 'time');

```

Create a rule:

Go to [EMQX Dashboard](http://127.0.0.1:18083/#/rules), select the
"rule" tab on the menu to the left.

Select "message.publish", then type in the following SQL:

```sql
SELECT
    payload.temp as temp,
    payload.humidity as humidity,
    payload.location as location
FROM
    "message.publish"
```

![image](./assets/rule-engine/timescaledb_sql_1.png)

Bind an action:

Click on the "+ Add" button under "Action Handler", and then select
"Data to TimescaleDB" in the pop-up dialog window.

![image](./assets/rule-engine/timescaledb_action_0.png)

Fill in the parameters required by the action:

Two parameters is required by action "Data to TimescaleDB":

1). SQL template. SQL template is the sql command you'd like to run
when the action is triggered. In this example we'll insert a message
into TimescaleDB, so type in the following sql
template:

```sql
insert into conditions(time, location, temperature, humidity) values (NOW(), ${location}, ${temp}, ${humidity})
```

Before data is inserted into the table, placeholders like \${key} will
be replaced by the corresponding values.

![image](./assets/rule-engine/timescaledb_action_1.png)

2). Bind a resource to the action. Since the dropdown list "Resource"
is empty for now, we create a new resource by clicking on the "New
Resource" to the top right, and then select "TimescaleDB":

![image](./assets/rule-engine/timescaledb_action_1.png)

Configure the resource:

Set "TimescaleDB Database" to "tutorial", "TimescaleDB User" to
"postgres", "TimescaleDB Password" to "password", and keep all other
configs as default, and click on the "Testing Connection" button to
make sure the connection can be created successfully, and then click
on the "Create" button.

![image](./assets/rule-engine/timescaledb_resource_0.png)

Back to the "Actions" dialog, and then click on the "Confirm" button.

![image](./assets/rule-engine/timescaledb_action_3.png)

Back to the creating rule page, then click on "Create" button. The
    rule we created will be show in the rule list:

![image](./assets/rule-engine/timescaledb_rule_overview_0.png)

We have finished, testing the rule by sending an MQTT message to
    emqx:

```bash
> Topic: "t/1"
>
> QoS: 0
>
> Retained: false
>
> Payload: {"temp":24,"humidity":30,"location":"hangzhou"}
```

Then inspect the TimescaleDB table, verify a new record has been
inserted:

 tutorial=\# SELECT \* FROM conditions LIMIT 100;

The output data could look like

 this:
```bash
    time              | location | temperature | humidity

 \------------------------------+----------+-------------+----------2019-06-27
 01:41:08.752103+00 | hangzhou | 24 | 30

```

And from the rule list, verify that the "Matched" column has increased
to 1:

![image](./assets/rule-engine/timescaledb_rule_overview_1.png)
