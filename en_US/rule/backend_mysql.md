# Ingest Data into MySQL

## Set up MySQL Database and Create Table

Set up a MySQL database, and changes the username/password to root/public, taking Mac OSX for instance:

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
    INDEX topic_index(`id`, `topic`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8MB4;
```

<img src="./assets/rule-engine/mysql_init_1.png" alt="image" style="zoom:50%;" />

## Create a Rule

Go to [EMQX Dashboard](http://127.0.0.1:18083/#/rules), select the "rule" tab on the menu to the left.

Select "message.publish", then type in the following SQL:

```sql
SELECT * FROM "message.publish"
```

<img src="./assets/rule-engine/mysql_sql_1.png" alt="image" style="zoom:50%;" />

## Add an Action

Click on the "+ Add" button under "Action Handler", and then select
"Data to MySQL" in the pop-up dialog window.

![image](./assets/rule-engine/mysql_action_1.png)

Fill in the parameters required by the action:

Two parameters are required by action "Data to MySQL":

1). SQL template. SQL template is the SQL command you'd like to run
when the action is triggered. In this example, we'll insert a message
into MySQL, so type in the following SQL
template:

```sql
insert into t_mqtt_msg(msgid, topic, qos, payload, arrived) values (${id}, ${topic}, ${qos}, ${payload}, FROM_UNIXTIME(${timestamp}/1000))
```

Before data is inserted into the table, placeholders like \${key} will
be replaced by the corresponding values.

<img src="./assets/rule-engine/mysql_action_2.png" alt="image" style="zoom:50%;" />

2). Bind a resource to the action. Since the dropdown list "Resource"
is empty for now, we create a new resource by clicking on the "New
Resource" to the top right, and then select "MySQL":

<img src="./assets/rule-engine/mysql_action_3.png" alt="image" style="zoom:50%;" />

Configure the resource:

Set "MySQL Database" to "test", "MySQL Username" to "root", "MySQL
Password" to "public", and "Description" to "MySQL resource to
127.0.0.1:3306 db=test", and click on the "Testing Connection" button
to make sure the connection can be created successfully, and then
click on the "Create" button.

<img src="./assets/rule-engine/mysql_resource_1.png" alt="image" style="zoom:50%;" />

Back to the "Actions" dialog, and then click on the "Confirm" button.

<img src="./assets/rule-engine/mysql_action_4.png" alt="image" style="zoom:50%;" />

Back to the creating rule page, then click on "Create" button. The rule we created will be show in the rule list:

![image](./assets/rule-engine/mysql_rule_overview_1.png)

## Test the Rule

We have finished creating the rule, test the rule by sending an MQTT message to emqx:

```bash
> Topic: "t/a"
>
> QoS: 1
>
> Payload: "hello"
```

Then inspect the MySQL table, verify a new record has been inserted:

<img src="./assets/rule-engine/mysql_result_1.png" alt="image" style="zoom:50%;" />
