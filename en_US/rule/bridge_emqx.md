# Bridge Data into Multiple MQTT Brokers

## Set up MQTT Brokers

Set up another MQTT broker using EMQX, taking Mac OSX for instance:

```bash
$ brew tap emqx/emqx/emqx

$ brew install emqx

# start emqx
$ emqx console
```

## Create a Rule

Go to [EMQX Dashboard](http://127.0.0.1:18083/#/rules), select the
"rule" tab on the menu to the left.

Select "message.publish", then type in the following SQL:

```sql
SELECT
    *
FROM
    "message.publish"
```

<img src="./assets/rule-engine/mysql_sql_1.png" alt="image" style="zoom:50%;" />

## Add an Action

Click on the "+ Add" button under "Action Handler", and then select
"Data bridge to MQTT Broker" in the pop-up dialog window.

<img src="./assets/rule-engine/mqtt_action_0.png" alt="image" style="zoom:50%;" />

Bind a resource to the action. Since the dropdown list "Resource" is
    empty for now, we create a new resource by clicking on the "New
    Resource" to the top right, and then select "MQTT Bridge":

<img src="./assets/rule-engine/mqtt_action_1.png" alt="image" style="zoom:50%;" />

### Configure the Resource

Set "EMQX Node Name" to the node name of the remote name, and keep
all other configs as default, and click on the "Testing Connection"
button to make sure the connection can be created successfully, and
then click on the "Create" button.

<img src="./assets/rule-engine/rpc_resource_0.png" alt="image" style="zoom:50%;" />

Back to the "Actions" dialog, and then click on the "Confirm"
    button.

<img src="./assets/rule-engine/rpc_action_2.png" alt="image" style="zoom:50%;" />

Back to the creating rule page, then click on the "Create" button. The
    rule we created will be shown in the rule list:

<img src="./assets/rule-engine/rpc_rule_overview_0.png" alt="image" style="zoom:50%;" />

## Test the Rule

We have finished creating the rule, test the rule by sending an MQTT message to EMQX:

```bash
Topic: "t/1"

QoS: 0

Retained: false

Payload: "Hello, World\!"
```

Then verify a message has been published to the other EMQX:

<img src="./assets/rule-engine/rpc_result.png" alt="image" style="zoom:50%;" />

And from the rule list, verify that the "Matched" column has increased
to 1:

![image](./assets/rule-engine/rpc_rule_overview_1.png)

