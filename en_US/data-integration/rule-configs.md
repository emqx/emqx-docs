# Use Rules with Config files

By modifying the configuration file `emqx.conf`, you can configure rules before EMQX starts.

## Structure of Rule Configs

Under the `rule_engine` engine namespace, one or more rules can be created using the `rules` configuration item:

```js
rule_engine {
    rules.my_rule_id {
        sql = "SELECT * FROM \"t/#\""
        actions = []
    }
}
```

Where `my_rule_id` is the ID of the rule. It must be a combination of alphanumeric characters or underscores and cannot start with a number.
In the definition of each rule, the `sql` field is used to define the SQL statement.
The `actions` field is an array that can define one or more actions.

### Configure Built-in Actions

For built-in actions, you need to configure an object containing a `function` field in the `actions` field:

```js{1-4,15-17}
rule_engine {
    rules.my_rule_id {
        sql = "SELECT * FROM \"t/#\""
        actions = [
            {
                function = console
            },
            {
                function = republish
                args = {
                    topic = "a/1"
                    payload = "${payload}"
                }
            }
        ]
    }
}
```

The `function` field is the name of the action. The `args` field is the parameter list of the action.
The `republish` action needs to specify parameters such as message subject and message content, while the `console` action does not need any parameters.

In the action, you can use the form of `${some_key}` to reference variables in the SQL result.
Because the SQL statement uses `SELECT *` to output all the available fields, we can use `${payload}` to obtain the message content in the original message, or `${clientid}` to obtain the client ID of the message, etc.

For the event types supported by the rule and the available fields, see: [events and fields](./rule-sql-events-and-fields.md)

For details of the built-in actions, see: [actions](./rule-actions.md)

### Reference Data Bridges as Actions

To use data bridging as an action, you need to configure the ID of data bridge in the `actions` field:

```js{1-3,5-6}
rule_engine {
    rules.my_rule_id {
        sql = "SELECT * FROM \"t/#\""
        actions = ["mqtt:my_egress_mqtt_bridge"]
    }
}
```

Where `mqtt:my_egress_mqtt_bridge` is a data bridge ID, whose type is `mqtt` and name is `my_egress_mqtt_bridge`.

Before using data bridge, you need to create it in advance:

```js
bridges.mqtt.my_egress_mqtt_bridge {
    connector = {
        server = "broker.EMQX.io:1883"
        username = "username1"
        password = ""
        ssl.enable = false
    }

    direction = egress

    remote_topic = "from_emqx/${topic}"
    remote_qos = "${qos}"
    payload = "${payload}"
    retain = false
}
```

The above configuration is slightly more complicated than the previous rule configuration, but it just to create an MQTT bridge.

We use the `bridges` namespace to create data bridges, `bridges.mqtt.my_egress_mqtt_bridge` creates a data bridge with type `mqtt` and name `my_egress_mqtt_bridge`. 
The bridge id is of format `<type>:<name>`, i.e. `mqtt:my_egress_mqtt_bridge`.
This ID is referenced as an action by the previous rule.

We use the `connector` field to configure the MQTT connection related information, such as the server address, port, username and password, etc.

For details of data bridges, see [data bridges](./data-bridges.md)

## Configure Rules to Handle Messages

We can use rules to handle messages with specific topics. 
An example of a rule for processing mqtt messages is given below.

Add the following configs to the end of the `emqx.conf` file, 

```js
rule_engine {
    rules.my_republish_rule {
        sql = "SELECT qos, payload.x as y FROM \"t/a\""
        actions = [
            {
                function = republish
                args = {
                    topic = "t/b"
                    qos = "${qos}"
                    payload = "y: ${y}"
                }
            }
        ]
    }
}
```

This configuration creates a rule named "my_republish_rule". This rule filters the `x` field from the message content with the subject `t/a` and assigns it to `y`.

The `actions` parameter defines the action that will be triggered when the rule is successfully executed: republish.

Now, if you send a message (JSON format) with a payload of `{"x": 1}` and a QoS of 0 to the topic `t/a`, the rule will be matched.

The rule first executes the SQL statement, selects the `qos` field from the message context, extracts the `x` field in the `payload` and assigns it to the variable `y`, and finally triggers the `republish` action to send a message to topic `t/b`, with payload of string `y: 1` (because `payload.x = 1`), and the QoS is 0.

## Configure Rules to Handle Events

The rule uses **event-topics** starting with **$events/** to process EMQX events. An example of a rule that handles the "client connected" event is given below.

Add the following configs to the end of the `emqx.conf` file, 

```js
rule_engine {
    rules.client_connected_debug {
        sql = "SELECT clientid, connected_at FROM \"$events/client_connected\" WHERE username = 'EMQX'"
        actions = [{function = console}]
    }
}
```

This configuration defines a rule with the ID "client_connected_debug".
This rule selects the "ClientID" and "connected_at" fields from the event context, and then calls the `console` action to print the SQL output to the EMQX console.

In addition to client-side online and offline events, rules also support subscription and unsubscribe, message delivery and other events.

For the event types supported by the rule and the available fields, see: [events and fields](./rule-sql-events-and-fields.md)

## Configure Rules that use Data Bridges as Data Sources

Some data bridges (such as the MQTT bridge) can also be used as the data source of rules. Rules use **data bridging topics** starting with **$bridges/** to handle events triggered by data bridging. An example of a rule that uses "MQTT bridge" as a data source is given below.

Add the following configs to the end of the `emqx.conf` file, 

```js
rule_engine {
    rules.receive_msgs_from_remote_mqtt_broker {
        sql = "SELECT * FROM \"$bridges/mqtt:my_mqtt_source\" WHERE username = 'EMQX'"
        actions = [{function = console}]
    }
}
```

This example create a rule with ID `receive_msgs_from_remote_mqtt_broker`.
A data bridge topic is specified in the SQL statement: `$bridges/mqtt:my_mqtt_source`.
Where `mqtt:my_mqtt_source` is an ID of a data bridge, its type is `mqtt`, and name is `my_mqtt_source`.

This MQTT bridge needs to be created in advance:

```js
bridges.mqtt.my_mqtt_source {
    connector = {
        server = "192.168.2.100:1883"
        username = "username1"
        password = ""
        ssl.enable = false
    }
    direction = ingress
    remote_topic = "aws/#"
    remote_qos = 1
}
```

We use the `bridges` namespace to create data bridges.

The `bridges.mqtt.my_mqtt_source` creates a data bridge with type `mqtt` and name `my_mqtt_source`。So
its ID is `mqtt:my_mqtt_source`.

The `connector` field configures MQTT connection related configurations, such as the server address, username and password.

For details of data bridges, see [data bridges](./data-bridges.md)


The rule calls the `console` action at the end. This is a debugging action, which will print all the fields filtered out by the SQL statement to the EMQX console.

Because the SQL statement uses `SELECT *` to output all the available fields, it will print all the fields related to the message received from the remote MQTT broker, such as payload, QoS, topic, etc.
