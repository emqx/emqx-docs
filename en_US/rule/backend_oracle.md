# Ingest Data into Oracle DB

## Set up Oracle Database and Create Table

Create t_mqtt_msg table:

```sql
CREATE TABLE t_mqtt_msg (msgid VARCHAR2(64),topic VARCHAR2(255), qos NUMBER(1), payload NCLOB)
```

<img src="./assets/rule-engine/oracle_action_1.png" alt="image" style="zoom:50%;" />

## Create Rules

Open [EMQX Dashboard](http://127.0.0.1:18083/#/rules) and select the "Rules" tab on the left.

Fill in the rule SQL:

```sql
SELECT * FROM "t/#"
```

## Add an Action

In the "Response Action" interface, select "Add", and then select "Save Data to Oracle Database" in the "Action" drop-down box.

<img src="./assets/rule-engine/oracle_action_2.png" alt="image-20230522182757372" style="zoom:50%;" />

Fill in the action parameters:

The "Save data to Oracle Database" action requires two parameters:

1). SQL template. In this example, we insert a piece of data into Oracle Database. The SQL template is:

```sql
INSERT INTO T_MQTT_MSG (MSGID, TOPIC, QOS, PAYLOAD) values ('${id}', '${topic}', '${qos}', '${payload}');
```

2). The ID of the associated resource. Now that the resource drop-down box is empty, and you can click "New Resource" in the upper right corner to create an Oracle Database resource:

Fill in the resource configuration:

![image](./assets/rule-engine/oracle_action_3.png)

Note: for the connection to Oracle Real Applications Cluster (RAC) to work properly, the "Service Name" field must be filled in correctly.  If left empty, it defaults to the Database SID.

Click the "New" button.

Return to the response action interface and click "OK". Then return to the rule creation interface and click "Create".

In the rule list, you can click the "View" button or the rule ID connection to preview the rule just created. 

## Test the Rule

The rule has been created. Now, send a piece of data:

```bash
Topic: "t/a"
QoS: 1
Payload: "hello"
```

View the number of rule hits, it should now be 1. 
