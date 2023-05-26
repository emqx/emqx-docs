# Save Offline Messages to PostgreSQL

## Set up Environment

Set up the PostgreSQL database, and take MacOS X as an example:

```bash
$ brew install postgresql
$ brew services start postgresql
```

Create the mqtt database:

```
# Create a database named'mqtt' with the username postgres
$ createdb -U postgres mqtt

$ psql -U postgres mqtt

mqtt=> \dn;
List of schemas
Name  | Owner
--------+-------
public | postgres
(1 row)
```

Create the mqtt_msg table:

```sql
$ psql -U postgres mqtt

CREATE TABLE mqtt_msg (
  id SERIAL8 primary key,
  msgid character varying(64),
  sender character varying(64),
  topic character varying(255),
  qos integer,
  retain integer,
  payload text,
  arrived timestamp without time zone
);
```

:::tip

The message table structure cannot be modified. Please use the above SQL statement to create

:::

## Create Rules

Open [EMQX Dashboard](http://127.0.0.1:18083/#/rules) and select the "Rules" tab on the left.

Then fill in the rule SQL:

FROM description

​	**t/#**: The publisher publishes a message to trigger the action of saving of offline messages to PostgreSQL

​	**$events/session_subscribed**: The subscriber subscribes to topics to trigger  the action of getting offline messages

​	**$events/message_acked**: The subscriber replies to the message ACK to trigger the action of deleting the offline message that has been received

```sql
SELECT * FROM "t/#", "$events/session_subscribed", "$events/message_acked" WHERE topic =~ 't/#'
```

<img src="./assets/rule-engine/ofline-rules.png" alt="image-20230525151209609" style="zoom:50%;" />

## Add an Action

Select "Add Action" on the "Response Action" interface, and then select "Save offline messages to PostgreSQL" in the "Add Action" drop-down box.

<img src="./assets/rule-engine/offline-msg.png" alt="image-20230525135721993" style="zoom:50%;" />

Now that the resource drop-down box is empty, and you can click "Create" in the upper right corner to create a PostgreSQL resource:

The "Create Resource" dialog box pops up

<img src="./assets/rule-engine/postgre-offline-resource.png" alt="image-20230525151714685" style="zoom:50%;" />

Fill in the resource configuration:

Fill in the real PostgreSQL server address and the values corresponding to other configurations, and then click the "Test" button to ensure that the connection test is successful.

Finally, click the "Confirm" button.

Return to the response action interface and click "Confirm".

Return to the rule creation interface and click "Create".

<img src="./assets/rule-engine/postgre-offline-rule.png" alt="image-20230525151911291" style="zoom:50%;" />

## Test the Rule

The rule has been created, and you can send a piece of data through the WebSocket client of Dashboard **(The QoS of the published message must be greater than 0):**

<img src="./assets/rule-engine/offline-message-received.png" alt="image-20230525152023575" style="zoom:50%;" />

After the message is sent, you can see the message is saved in PostgreSQL through PostgreSQL:

<img src="./assets/rule-engine/pg_offline_msg_09.png" style="zoom:50%;" />

Use another client to subscribe to the topic "t/1" (the QoS of the subscribed topic must be greater than 0, otherwise the message will be received repeatedly):

<img src="./assets/rule-engine/pg_offline_msg_10.png" style="zoom:50%;" />

After subscribing, you will receive the offline message saved in PostgreSQL immediately:

<img src="./assets/rule-engine/pg_offline_msg_11.png" style="zoom:40%;" />

Offline messages will be deleted in PostgreSQL after being received:

<img src="./assets/rule-engine/pg_offline_msg_12.png" style="zoom:50%;" />
