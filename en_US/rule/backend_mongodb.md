# Save data to MongoDB

In order to facilitate the demonstration of some functions, the follow-up content will be introduced based on MongoDB Cloud. However, users who deploy MongoDB in other ways still can use this document to learn how to use MongoDB resources and actions.

We first deploy a cluster instance named Cluster0 on MongoDB Cloud as a replica set :

![image-20211126152843438](./assets/rule-engine/mongo_data_to_store1.png)

    security:
    authorization: enabled

![image-20211126153431253](./assets/rule-engine/mongo_data_to_store2.png)

Before entering the next step, we also need to configure the user password and IP access whitelist on the `Database Access` and `Network Access` pages to ensure normal access.

## Create resources

After completing the above work, we will create MongoDB resources and rules in EMQX Dashboard.

First, open the EMQX Dashboard, enter the resource page of the rule engine, click the **Create** button in the upper left corner, and the **Create Resource** form will pop up. In the **Resource Type** drop-down box in the form, we can see three resource types of **MongoDB Single node mode**, **MongoDB Replica Set mode** and **MongoDB Sharded mode**, which correspond to the three deployment methods of MongoDB.

Go to [EMQX Dashboard](http://127.0.0.1:18083/#/rules), select the "rule" tab on the menu to the left. Then type in the following SQL:

```sql
SELECT id as msgid, topic, qos, payload, publish_received_at as arrived FROM "t/#"
```

![image](./assets/rule-engine/mongo_sql_1.png)

![image-20211126201826084](./assets/rule-engine/mongo_data_to_store4.png)

Click on the "+ Add action" button under the "Action" tab, and then select "Data persist" -> "Data to MongoDB" in the pop-up dialog windows.

### Enable SRV Record

By default, MongoDB Cloud provides a domain name with SRV and TXT records added for connection.

We can click the Connect button of the Cluster0 instance on the Databases page of MongoDB Cloud, and select one of the three connection methods. Then, we can see the connection string to be used by the current instance. The selected part of the cursor is the content of the **MongoDB Server**  field that we need to configure to the MongoDB resource of the EMQX rule engine later.

![image-20211129104759799](./assets/rule-engine/mongo_data_to_store5.png)

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

![image-20211129113336183](./assets/rule-engine/mongo_data_to_store7.png)

Configure the resource:

In order to quickly obtain the configuration information, we can use the `nslookup` command to query DNS records:

```
$ nslookup
> set type=SRV 
> _mongodb._tcp.cluster0.j0ehi.mongodb.net
Server:         26.26.26.53
Address:        26.26.26.53#53

Non-authoritative answer:
_mongodb._tcp.cluster0.j0ehi.mongodb.net        service = 0 0 27017 cluster0-shard-00-01.j0ehi.mongodb.net.
_mongodb._tcp.cluster0.j0ehi.mongodb.net        service = 0 0 27017 cluster0-shard-00-02.j0ehi.mongodb.net.
_mongodb._tcp.cluster0.j0ehi.mongodb.net        service = 0 0 27017 cluster0-shard-00-00.j0ehi.mongodb.net.

Authoritative answers can be found from:
> set type=TXT 
> cluster0.j0ehi.mongodb.net
Server:         26.26.26.53
Address:        26.26.26.53#53

Non-authoritative answer:
cluster0.j0ehi.mongodb.net      text = "authSource=admin&replicaSet=atlas-r36spx-shard-0"
```

Then fill in the queried server list in the **MongoDB Server** option in the format of `host[:port][,...hostN[:portN]]`, and configure **Auth Source**  and **Replica Set** according to the queried TXT record content:

![image-20211129143723391](./assets/rule-engine/mongo_data_to_store8.png)

Back to the creating rule page, then click on "Create" button. The rule we created will be show in the rule list.

After the resource is created, we need to create the corresponding rules. Click the **Create** button at the upper left corner of the rule page to enter the **Create Rule** page, and enter the following SQL:

```bash
Topic: "t/mongo"
QoS: 1
Payload: "hello"
```

```
SELECT
	id as msgid,
	topic,
	qos,
	payload,
	publish_received_at as arrived
FROM
	"t/#"
```

This SQL means that all messages that match the topic filter `t/#` will trigger this rule, such as `t/1`, `t/1/2`, etc. The filtered data, such as msgid and topic, can be used to perform subsequent actions.

![image-20211129150342611](./assets/rule-engine/mongo_data_to_store9.png)

### 2. Add response action

Click the **Add Action** button. For **Action Type**, select data persistence and data to MongoDB. Then, select a resource we just created in the **Use Resource** drop-down list. Configure **Collection** on demand, here I configure it as demo. **Message content template** remains empty, which means that the data filtered by SQL is converted into Json data in the form of a Key-Value list and is written to MongoDB. Multiple response actions can be added to each rule. Here we only need one response action. Therefore, after adding the following actions, we can click the **Create** button at the bottom of the page to complete the creation of the rule.

![image-20211129151712310](./assets/rule-engine/mongo_data_to_store10.png)

## Test and Verification

We directly use the MQTT client tool in Dashboard to publish a message. In this example, we change the message topic to `t/1` to hit the rules we set. The Payload and QoS remain unchanged. Then, click **publish**.

![image-20211129155548290](./assets/rule-engine/mongo_data_to_store11.png)

After the message is successfully published, we can see the data just written on the Collections page of the Cluster0 cluster instance of MongoDB Cloud:

![image-20211129160418285](./assets/rule-engine/mongo_data_to_store12.png)
