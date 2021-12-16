# Get subscription relationship from MongoDB

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

Create the mqtt_sub table:
```sql
$ mongo 127.0.0.1/mqtt -uroot -ppublic
db.createCollection("mqtt_sub");
```

Create rules:

Open [EMQ X Dashboard](http://127.0.0.1:18083/#/rules) and select the "Rules" tab on the left.

Then fill in the rule SQL:

```bash
SELECT * FROM "$events/client_connected"
```

![image-20211214142714391](./assets/rule-engine/mongo_sub_01.png)

Related actions:

Select "Add Action" on the "Response Action" interface, and then select "Get Subscription List from MongoDB" in the "Add Action" drop-down box

![image-20211214142813615](./assets/rule-engine/mongo_sub_02.png)

Fill in the action parameters:

The action of "Get subscription list from MongoDB" requires one parameter:

1). Associated resources. The resource drop-down box is empty now, and you can click "New" in the upper right corner to create a MongoDB  resource:

![image-20211214142840878](./assets/rule-engine/mongo_sub_03.png)

The "Create Resource" dialog box pops up

![image-20211214142951416](./assets/rule-engine/mongo_sub_04.png)

Fill in the resource configuration:

Fill in the real MongoDB  server address and the values corresponding to other configurations, and then click the "Test Connection" button to ensure that the connection test is successful.

Finally click the "OK" button.

![image-20211214143043056](./assets/rule-engine/mongo_sub_05.png)

Return to the response action interface and click "OK".

![image-20211214143347354](./assets/rule-engine/mongo_sub_06.png)

Return to the rule creation interface and click "Create".

![image-20211214143412422](./assets/rule-engine/mongo_sub_07.png)

The rule has been created, and you can insert a subscription relationship into MongoDB through "mongo":

```
db.mqtt_sub.insert({clientid: "test", topic: "t1", qos: 1})
```

![](./assets/rule-engine/mongo_sub_08.png)

Log in to the device whose clientid is test via Dashboard:

![image-20211214143449950](./assets/rule-engine/mongo_sub_09.png)

Check the "Subscription" list, and you can see that the Broker obtains the subscription relationship from MongoDB and subscribes as the agent device:

![image-20211214143528939](./assets/rule-engine/mongo_sub_10.png)
