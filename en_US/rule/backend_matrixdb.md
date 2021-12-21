# Sava data to MatrixDB

Build the MatrixDB database, set the user name and password to root/public, and create a database named mqtt.

Access MatrixDB through the command line tool psql and create the `t_mqtt_msg` table:

```bash
$ psql -h localhost -U root mqtt
```

```sql
CREATE TABLE t_mqtt_msg (
    id SERIAL primary key,
    msgid character varying(64),
    sender character varying(64),
    topic character varying(255),
    qos integer,
    payload text,
    arrived timestamp without time zone
);
```

Create a rule:

Open [EMQ X Dashboard](http://127.0.0.1:18083/#/rules) and select the "Rules" tab on the menu to the left.

Fill in the rule SQL:

```bash
SELECT * FROM "t/#"
```

![image-20211201154727219](D:/emqx/emqx-docs/zh_CN/rule/assets/rule-engine/matrixdb_data_to_store1.png)

Related action:

Click on the "Add Action" button under "Action Handler", and then select "Data to MatrixDB" in the pop-up dialog window.

![image-20211201154826879](D:/emqx/emqx-docs/zh_CN/rule/assets/rule-engine/matrixdb_data_to_store2.png)

Fill in the parameters required by the action:

The following parameters are required by action "Data to MatrixDB":

1). **Use resource**, which is the resource ID. Now the resource drop-down box is empty. You need to create an available MatrixDB resource instance first.

![image-20211201155055729](D:/emqx/emqx-docs/zh_CN/rule/assets/rule-engine/matrixdb_data_to_store3.png)

Click the **New** button on the right side of **Use Resource** to enter the **Create Resource** page. The MatrixDB resource requires the following configurable items:

**Server**, the server address of MatrixDB.

**Database name**, MatrixDB database name.

**User name, password**, authentication credentials.

**Whether to reconnect**, whether to enable automatic reconnection.

**Connection pool size**, connection process pool size, which will help obtain the best performance with reasonable configuration.

**Enable SSL connection**, whether to enable TLS connection.

After the configuration is complete, click **OK** to complete the creation.

![image-20211201163555899](D:/emqx/emqx-docs/zh_CN/rule/assets/rule-engine/matrixdb_data_to_store4.png)

After the resource is successfully created, we will return to the **Add Action** page, and **Use Resource** is also automatically filled in with the resource ID of the Matrix resource just created.

![image-20211201160356380](D:/emqx/emqx-docs/zh_CN/rule/assets/rule-engine/matrixdb_data_to_store5.png)

2). **Enable batch insert**, whether to enable batch insert. In high concurrency scenarios, it can significantly improve write performance.

3). **Maximum batch number**, the largest INSERT SQL entry that can be sent in a single batch request.

4). **Maximum batch interval**, the maximum waiting interval between two batch requests.

5). **Synchronous or Asynchronous Insertion**, which determines to make a synchronous or asynchronous call.

6). **Call timeout.** It  refers to the timeout for executing actions in synchronous mode. This option is only valid for synchronous insertion.

7). **SQL template**. It contains a placeholder SQL template for inserting or updating data to the database. In this example, we use the following SQL:

```
INSERT INTO t_mqtt_msg (msgid, topic, qos, payload, arrived)
VALUES (${id}, ${topic}, ${qos}, ${payload}, to_timestamp(${timestamp}::double precision / 1000))
```

Here we use the placeholder of ${id}, which will be replaced with runtime data when the action is executed.

![image-20211201163937652](D:/emqx/emqx-docs/zh_CN/rule/assets/rule-engine/matrixdb_data_to_store6.png)

After the configuration is complete, click **OK** to complete the addition of the action. Then click the **Create** button at the bottom of the rule page to complete the rule creation.

After creating MatrixDB resources and rules, we will conduct the test and verification. We directly use the MQTT client tool in Dashboard to publish a message. In this example, we change the message topic to `t/1` to hit the rules we set. The Payload and QoS remain unchanged. Then, click **publish**.

![image-20211201164831324](D:/emqx/emqx-docs/zh_CN/rule/assets/rule-engine/matrixdb_data_to_store7.png)

After the message is successfully published, we will be able to see the newly written data in the `t_mqtt_msg` table:

![image-20211201165210566](./assets/rule-engine/matrixdb_data_to_store8.png)
