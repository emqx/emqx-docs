# Ingest Data into MySQL

Setup a MySQL database, and changes the username/password to root/public, taking Mac OSX for instance:

```bash
$ brew install mysql

$ brew services start mysql

$ mysql -u root -h localhost -p

ALTER USER 'root'@'localhost' IDENTIFIED BY 'public';
```

Initiate MySQL table:

```bash
$ mysql -u root -h localhost -ppublic

create "test" database:

CREATE DATABASE test;

create "t_mqtt_msg" table:

USE test;
```

```sql
CREATE TABLE `t_mqtt_msg` (
    `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
    `msgid` varchar(64) DEFAULT NULL,
    `topic` varchar(255) NOT NULL,
    `qos` tinyint(1) NOT NULL DEFAULT '0',
    `payload` blob,
    `arrived` datetime NOT NULL,
    PRIMARY KEY (`id`),
    INDEX topic_index(`topic`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8MB4;
```

![image](./assets/rule-engine/mysql_init_1.png)


Create a rule:

Go to [EMQX Dashboard](http://127.0.0.1:18083/#/rules), select the "rule" tab on the menu to the left.

Select "message.publish", then type in the following SQL:

```sql
SELECT * FROM "message.publish"
```

![image](./assets/rule-engine/mysql_sql_1.png)

Bind an action:

Click on the "+ Add" button under "Action Handler", and then select
"Data to MySQL" in the pop-up dialog window.

![image](./assets/rule-engine/mysql_action_1.png)

Fill in the parameters required by the action:

Two parameters is required by action "Data to MySQL":

1). SQL template. SQL template is the sql command you'd like to runwhen the action is triggered. In this example we'll insert a message into mysql, so type in the following sql template:

```sql
insert into t_mqtt_msg(msgid, topic, qos, payload, arrived) values (${id}, ${topic}, ${qos}, ${payload}, FROM_UNIXTIME(${timestamp}/1000))
```

Before data is inserted into the table, placeholders like \${id} will be replaced by the corresponding values.

If a placeholder variable is undefined, you can use the **Insert undefined value as Null** option to define the rule engine behavior:

- `false` (default): The rule engine can insert the string `undefined` into the database.
- `true`: Allow the rule engine to insert `NULL` into the database when a variable is undefined.

![image](./assets/rule-engine/mysql_action_2.png)

2). Bind a resource to the action. Since the dropdown list "Resource" is empty for now, we create a new resource by clicking on the "Create" to the top right, and then select "MySQL":

<img src="./assets/rule-engine/mysql_action_3.png" alt="image" style="zoom:67%;" />

Configure the resource:

Set "MySQL Database" to "test", "MySQL Username" to "root", "MySQL Password" to "public", and "Description" to "MySQL resource to 127.0.0.1:3306 db=test".

Click the "Test" button to make sure the connection can be created successfully, and then click the "Confirm" button.

<img src="./assets/rule-engine/mysql_resource_1.png" alt="image" style="zoom:67%;" />

Back to the "Actions" dialog, and then click on the "Confirm" button.

Back to the creating rule page, then click on "Create" button. The rule we created will be shown in the rule list:

![image](./assets/rule-engine/mysql_rule_overview_1.png)

We have finished, testing the rule by sending an MQTT message to emqx:

```bash
> Topic: "t/a"
>
> QoS: 1
>
> Payload: "hello"
```

Then inspect the MySQL table, verify a new record has been inserted:

![image](./assets/rule-engine/mysql_result_1.png)
