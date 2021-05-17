# Save data to DynamoDB

Setup a DynamoDB database, taking Mac OSX for instance:

```bash
$ brew install dynamodb-local

$ dynamodb-local
```

Initiate the DynamoDB table:

1). Create table definition file "mqtt_msg.json" for DynamoDB:

```json
{
    "TableName": "mqtt_msg",
    "KeySchema": [
        { "AttributeName": "msgid", "KeyType": "HASH" }
    ],
    "AttributeDefinitions": [
        { "AttributeName": "msgid", "AttributeType": "S" }
    ],
    "ProvisionedThroughput": {
        "ReadCapacityUnits": 5,
        "WriteCapacityUnits": 5
    }
}
```

2). Create the DynamoDB
table:

```bash
$aws dynamodb create-table --cli-input-json file://mqtt_msg.json --endpoint-url http://localhost:8000
```

Create a rule:

Go to [EMQ X Dashboard](http://127.0.0.1:18083/#/rules), select the
"rule" tab on the menu to the left.

Select "message.publish", then type in the following SQL:

```sql
SELECT
     msgid as id, topic, payload
    FROM
      "message.pubish"
```


![image](./assets/rule-engine/mysql_sql_1.png)

Bind an action:

Click on the "+ Add" button under "Action Handler", and then select
"Data to DynamoDB" in the pop-up dialog window.

![image](./assets/rule-engine/dynamo_action_0.png)

Fill in the parameters required by the action:

Four parameters is required by action "Data to DynamoDB":

1). DynamoDB Table. Here set it to "mqtt_msg".

2). Hash Key. Here set it to "msgid".

3). DynamoDB Range Key. Leave it empty as we didn't define a range key
in the dynamodb definition file.

![image](./assets/rule-engine/dynamo_action_1.png)

4). Bind a resource to the action. Since the dropdown list "Resource"
is empty for now, we create a new resource by clicking on the "New
Resource" to the top right, and then select "DynamoDB":

![image](./assets/rule-engine/dynamo_action_2.png)

Configure the resource:

Fill in the configs as following:

- DynamoDB Region: us-west-2
- DynamoDB Server: <http://localhost:8000>
- AWS Access Key Id: "AKIAU5IM2XOC7AQWG7HK"
- AWS Secret Access Key: "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"

And then click on the "Create" button.

![image](./assets/rule-engine/dynamo_resource_0.png)

Back to the "Actions" dialog, and then click on the "Confirm" button.

![image](./assets/rule-engine/dynamo_action_3.png)

Back to the creating rule page, then click on "Create" button. The rule we created will be show in the rule list:

![image](./assets/rule-engine/dynamo_rule_overview_0.png)

We have finished, testing the rule by sending an MQTT message to emqx:

```bash
> Topic: "t/a"
>
> QoS: 1
>
> Payload: "hello"
```

Then inspect the DynamoDB table, verify a new record has been
inserted:

![image](./assets/rule-engine/dynamo_result.png)

And from the rule list, verify that the "Matched" column has increased
to 1:

![image](./assets/rule-engine/dynamo_rule_overview_1.png)
