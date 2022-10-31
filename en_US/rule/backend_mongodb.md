# Save data to MongoDB
Setup a MongoDB database, and changes the username/password to root/public, taking Mac OSX for instance:

```bash
$ brew install mongodb
$ brew services start mongodb

## add user root
$ use mqtt;
$ db.createUser({user: "root", pwd: "public", roles: [{role: "readWrite", db: "mqtt"}]});

## change the config file to enable authentication
$ vim /usr/local/etc/mongod.conf

    security:
    authorization: enabled

$ brew services restart mongodb
```

Initiate the MongoDB table:

```bash
$ mongo 127.0.0.1/mqtt -uroot -ppublic

db.createCollection("t_mqtt_msg");
```

Create a rule:

Go to [EMQX Dashboard](http://127.0.0.1:18083/#/rules), select the "rule" tab on the menu to the left. Then type in the following SQL:

```sql
SELECT id as msgid, topic, qos, payload, publish_received_at as arrived FROM "t/#"
```

![image](./assets/rule-engine/mongo_sql_1.png)

Bind an action:

Click on the "+ Add action" button under the "Action" tab, and then select "Data persist" -> "Data to MongoDB" in the pop-up dialog windows.

![image](./assets/rule-engine/mongo_action_0.png)

Fill in the parameters required by the action:

Two parameters is required by action "Data to MongoDB":

1). The mongodb collection. Set it to "t_mqtt_msg" we just created.

::: tip
From EMQX Enterprise 4.4.11 and 4.3.17, we can use placeholders in `${var}` format for the collections.
:::

2). Payload template. Payload template is the keys and values you'd
like to insert into mongodb when the action is triggered. In this
example we'll insert all the available fields we got from the rule SQL in JSON format, so we just leave the payload template as empty.

::: warning
MongoDB requires a JSON string when writing, so please ensure your template is a valid JSON format after all the placeholders are placed with the values. For example, you could write this in your template:

```
{"client": "${clientid}"}
```
:::

3). Bind a resource to the action. Since the dropdown list "Resource"
is empty for now, we create a new resource by clicking on the "New
Resource" to the top right, and then select "MongoDB Single Mode":

Configure the resource:

Set "Database Name" to "mqtt", "Username" to "root", "Password" to
"public", "Auth Source" to "mqtt", and keep all other configs as
default, and click on the "Testing Connection" button to make sure the
connection can be created successfully, and then click on the "Create"
button..

![image](./assets/rule-engine/mongo_resource_0.png)

Back to the "Actions" dialog, and then click on the "Confirm" button.

Back to the creating rule page, then click on "Create" button. The rule we created will be show in the rule list.

We have finished, testing the rule by sending an MQTT message to emqx:

```bash
Topic: "t/mongo"
QoS: 1
Payload: "hello"
```

Then inspect the MongoDB table, verify a new record has been inserted:

![image](./assets/rule-engine/mongo_result.png)

And from the rule list, verify that the "Matched" column has increased
to 1:

![image](./assets/rule-engine/mongo_rule_overview_1.png)
