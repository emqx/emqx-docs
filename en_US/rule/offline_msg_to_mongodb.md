# Save offline messages to MongoDB

Set up the MongoDB database and set the user name and password to root/public. Take MacOS X as an example:

```bash
$ brew install mongodb
$ brew services start mongodb

## Add root/public user
$ use mqtt;
$ db.createUser({user: "root", pwd: "public", roles: [{role: "readWrite", db: "mqtt"}]});

## Modify the configuration and disable anonymous authentication
$ vi /usr/local/etc/mongod.conf

    security:
    authorization: enabled

$ brew services restart mongodb
```

Create the mqtt_msg table:
```bash
$ mongo 127.0.0.1/mqtt -uroot -ppublic
db.createCollection("mqtt_msg");
```

Create rules:

Open [EMQX Dashboard](http://127.0.0.1:18083/#/rules) and select the "Rules" tab on the left.

Then fill in the rule SQL:

FROM description

​	**t/#**: The publisher publishes a message to trigger the action of saving of offline messages to MongoDB

​	**$events/session_subscribed**: The subscriber subscribes to topics to trigger  the action of getting offline messages

​	**$events/message_acked**: The subscriber replies to the message ACK to trigger the action of deleting the offline message that has been received

```sql
SELECT * FROM "t/#", "$events/session_subscribed", "$events/message_acked" WHERE topic =~ 't/#'
```

![image-20211214151125535](./assets/rule-engine/mongo_offline_msg_01.png)

Related actions:

Select "Add Action" on the "Action" interface, and then select "Offline messages" and "Offline Msg to MongoDB" in the "Action Type" drop-down box

![image-20211214151154689](./assets/rule-engine/mongo_offline_msg_02.png)

Now that the resource drop-down box is empty, and you can click "Create" in the upper right corner to create a MongoDB resource:

![image-20211214151834553](./assets/rule-engine/mongo_offline_msg_03.png)

The "Create" dialog box pops up:

![image-20211214151326446](./assets/rule-engine/mongo_offline_msg_04.png)

Fill in the resource configuration:

Fill in the real MongoDB server address and the values corresponding to other configurations, and then click the "Test Connection" button to ensure that the connection test is successful.

Finally click the "Confirm" button.

![image-20211214151354243](./assets/rule-engine/mongo_offline_msg_05.png)

Return to the action interface and click "Confirm".

![image-20211214151425343](./assets/rule-engine/mongo_offline_msg_06.png)

Return to the rule creation interface and click "Create".

![image-20211214151455755](./assets/rule-engine/mongo_offline_msg_07.png)

The rule has been created, and you can send a piece of data through the WebSocket client of Dashboard **(The QoS of the published message must be greater than 0):**

![image-20211214151531162](./assets/rule-engine/mongo_offline_msg_08.png)

After the message is sent, you can see the message is saved in MongoDB through mongo:

```
db.mqtt_msg.find()
```

![](./assets/rule-engine/mongo_offline_msg_09.png)

Use another client to subscribe to the topic "t/1" (the QoS of the subscribed topic must be greater than 0, otherwise the message will be received repeatedly):

![](./assets/rule-engine/mongo_offline_msg_10.png)

After subscribing, you will receive the offline message saved in MongoDB immediately:

![](./assets/rule-engine/mongo_offline_msg_11.png)

Offline messages will be deleted in MongoDB after being received:

![](./assets/rule-engine/mongo_offline_msg_12.png)
