# Bridge Data into MQTT Broker

Setup another MQTT broker using mosquitto, and change the port to 2883.
Take Mac OSX for instance:

```bash
$ brew install mosquitto

$ vim /usr/local/etc/mosquitto/mosquitto.conf

port 2883

# start mosquitto
$ brew services start mosquitto
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

### Configure the resource:

Set "Broker Address" to the address of mosquitto, here is
127.0.0.1:2883, and keep all other configs as default, and click on
the "Testing Connection" button to make sure the connection can be
created successfully, and then click on the "Create" button.

<img src="./assets/rule-engine/mqtt_resource_0.png" alt="image" style="zoom:50%;" />

Back to the "Actions" dialog, and then click on the "Confirm" button.

<img src="./assets/rule-engine/mqtt_action_2.png" alt="image" style="zoom:50%;" />

Back to the creating rule page, then click on the "Create" button. The
    rule we created will be shown in the rule list:

<img src="./assets/rule-engine/mqtt_rule_overview_0.png" alt="image" style="zoom:50%;" />

## Test the Rule

We have finished creating the rule, test the rule by sending an MQTT message to EMQX:

```bash
Topic: "t/1"

QoS: 0

Retained: false

Payload: "Hello, World\!"
```

Then verify a message has been published to mosquitto:

![image](./assets/rule-engine/rpc_result.png)

And from the rule list, verify that the "Matched" column has increased
to 1:

![image](./assets/rule-engine/mqtt_rule_overview_0.png)
