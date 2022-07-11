# Actions

Actions are components used to process the output results of rules and determine the final destination of data.

The current rule supports the following two actions:

- Built-in Actions: Currently, there are only two supported built-in actions: `republish` and `console`.

- Data Bridges: A data bridge is a channel to the external data system. Rules can directly use the ID of data bridge as the action, sending the output of the rule to the data bridge for further processing. For details of data bridge, see [data bridges](./data-bridges.md).

## The "republish" Action 

The "republish" action is used to publish a new MQTT message, which is applicable to the scenarios where downlink messages need to be sent to the device.

::: tip
The "republish" action does not prevent the delivery of the original message. For example, if a "a/1" message triggers a "republish" action through a rule and sends a new message "a/2", then the "a/1" message will still be delivered to the client subscribed to the topic.
:::

In the "republish" action, you can customize the payload, topic, QoS and other parameters of the message,
and fields in the rule output can be referenced as values of these parameters in the form of `${field name}`.

For an example of creating a message republish action, see: [Get Started](./rule-get-started.md#add-a-republish-action)

## The "console" Action 

The console output action is used to view the output results of rules. The results will be printed to the console in the form of logs.

If the emqx is started with `emqx console`, the results will be printed to the foreground.
If the emqx is started with `emqx start`, the results will be printed to `erlang log.*` under the log dir of EMQX.

### Output Format

In the first line it prints the `[rule action]` header followed by the rule ID.
The outputs from the second line is divided into two parts:

- The `Action Data` section is the output result of the rule. The fields contained in `Action Data` can be referenced in the form of `${field name}` in the action parameters.

- The `Envs` is the environment variable information available for the action. The environment variable information includes all available fields of the data source and other internal information related to the execution of this action.

Example of the outputs:

```
[rule action] rule_id1
    Action Data: #{key1 => val1}
    Envs: #{key1 => val1, key2 => val2}
```

::: tip
The "console output" action is only used for debugging. If it is used in the production environment, it will cause performance degradation.
:::
