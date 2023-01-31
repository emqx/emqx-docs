# Bridge data to EMQX

Create a rule

Go to [EMQX Dashboard](http://127.0.0.1:18083/#/rules), select the "rule" tab on the menu to the left.

Select "message.publish", then type in the following SQL:

```sql
SELECT
    *
FROM
    "message.publish"
```

![image](./assets/rule-engine/mysql_sql_1.png)

Bind an action:

Click on the "+ Add" button under "Action Handler", and then select
"Data bridge to MQTT Broker" in the pop-up dialog window.

![image](./assets/rule-engine/mqtt_action_0.png)

Bind a resource to the action. Since the dropdown list "Resource" is
    empty for now, we create a new resource by clicking on the "New
    Resource" to the top right, and then select "MQTT Bridge":

![image](./assets/rule-engine/mqtt_action_1.png)

Configure the resource:
```
Set "EMQX Node Name" to the node name of the remote name, and keep
all other configs as default, and click on the "Testing Connection"
button to make sure the connection can be created successfully, and
then click on the "Create" button.
```
![image](./assets/rule-engine/rpc_resource_0.png)

Back to the "Actions" dialog, and then click on the "Confirm"
    button.

![image](./assets/rule-engine/rpc_action_2.png)

Back to the creating rule page, then click on "Create" button. The
    rule we created will be show in the rule list:

![image](./assets/rule-engine/rpc_rule_overview_0.png)

We have finished, testing the rule by sending an MQTT message to
    emqx:

```bash
Topic: "t/1"

QoS: 0

Retained: false

Payload: "Hello, World\!"
```

Then verify a message has been published to the other emqx:

![image](./assets/rule-engine/rpc_result.png)

And from the rule list, verify that the "Matched" column has increased
to 1:

![image](./assets/rule-engine/rpc_rule_overview_1.png)

