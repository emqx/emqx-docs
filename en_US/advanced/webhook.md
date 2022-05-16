# WebHook

WebHook is a plugin provided by the [emqx_web_hook](https://github.com/emqx/emqx-web-hook) plugin with the function of notifying a web service of hook events in EMQX Broker.

The internal implementation of WebHook is based on  [hooks](./hooks.md), but it is closer to the top level. It obtains various events in EMQX Broker through the callback function mounted on the hook, and forwards them to the web server configured in emqx_web_hook.

Taking the client.connected event as an example, the event delivery process is as follows:

```bash
    Client      |    EMQX     |  emqx_web_hook |   HTTP       +------------+
  =============>| - - - - - - -> - - - - - - - ->===========>  | Web Server |
                |    Broker    |                |  Request     +------------+
```

::: tip
WebHook processes events in one-way pattern. It only supports pushing events in EMQX Broker to Web services, and does not care about the return of Web services.
With the help of Webhooks, many services such as device going online, online and offline recording, subscription and message storage, and message delivery confirmation can be completed.

:::

## Configuration item

The webhook configuration file is located in: `etc/plugins/emqx_web_hook.conf`, the detailed description of configuration items can be found in [Configuration Item](../admin/file.md).

## Trigger rule

Trigger rules can be configured in `etc/plugins/emqx_web_hook.conf`. The configuration format is as follows:

```bash
## Format example
web.hook.rule.<Event>.<Number> = <Rule>

## Example
web.hook.rule.message.publish.1 = {"action": "on_message_publish", "topic": "a/b/c"}
web.hook.rule.message.publish.2 = {"action": "on_message_publish", "topic": "foo/#"}
```

### Trigger event

The following events are currently supported:


| Name                 | Description                   | Execution timing                                             |
| -------------------- | ----------------------------- | ------------------------------------------------------------ |
| client.connect       | Processing connection packets | When the server receives the client's connection packet      |
| client.connack       | Issue connection acknowledge  | When the server is ready to send connack packet              |
| client.connected     | connected                     | After the client authentication is completed and successfully connected to the system |
| client.disconnected  | disconnected                  | When the client connection layer is about to close           |
| client.subscribe     | subscribe                     | After receiving the subscription message,and before executing `client.check_acl` authentication |
| client.unsubscribe   | unsubscribe                   | After receiving the unsubscription message                   |
| session.subscribed   | Session subscribed            | After completing the subscription operation                  |
| session.unsubscribed | session unsubscribed          | After completing the unsubscription operation                |
| message.publish      | message published             | Before the server rpublishes (routes) the message            |
| message.delivered    | message deliveried            | Before the message is ready to be delivered to the client    |
| message.acked        | message acknowledged          | After the server received the message ACK from the client    |
| message.dropped      | message dropped               | After the published message is dropped                       |

### Number

Multiple trigger rules can be configured for the same event, and events with the same configuration should be incremented in sequence.

### Rule

The trigger rule's 'value is a JSON string, and the available Keys are:

- action: string, taking a fixed value
- topic: a string, indicating a topic filter, the operation topic can only trigger the forwarding of the event if it matches the topic

For example, we only forward messages matching the topics of `a/b/c` and `foo/#` to the web server, and the configuration should be:

```bash
web.hook.rule.message.publish.1 = {"action": "on_message_publish", "topic": "a/b/c"}
web.hook.rule.message.publish.2 = {"action": "on_message_publish", "topic": "foo/#"}
```

In this way, Webhook will only forward messages matching the topics of  `a/b/c` and `foo/#`, such as `foo/bar`, etc., instead of forwarding `a/b/d` or `fo/bar`.


## Webhook event parameters

When the event is triggered, Webhook will group each event into an HTTP request and sent it to the web server configured by url according to the configuration. The request format is:

```bash
URL: <url>      # From the url field in the configuration
Method: POST        # Fixed as POST method

Body: <JSON>        # Body is a JSON format string
```

For different events, the content of the request body is different. The following table lists the parameters of the body in each event:

**client.connect**

| Key        |  Type  | Description |
| ---------- | ------- | ----- |
| action     | string  | event name<br>fixed at："client_connect" |
| clientid   | string | client ClientId                                             |
| username   | string  | client Username, When not existed, the value is "undefined" |
| ipaddress  | string  | client source IP address |
| keepalive  | integer | Heartbeat keepalive time applied by client |
| proto_ver  | integer | Protocol version number |


**client.connack**

| Key        | Type    | Description                                                 |
| ---------- | ------- | ----- |
| action     | string  | event name<br/>fixed at: "client_connack" |
| clientid   | string  | client ClientId |
| username   | string  | client Username, When not existed, the value is "undefined" |
| ipaddress  | string  | client source IP address |
| keepalive  | integer | Heartbeat keepalive time applied by client |
| proto_ver  | integer | Protocol version number |
| conn_ack   | string  | "success" means success, other means failure |


**client.connected**

| Key         | Type    | Description                                                 |
| ----------- | ------- | ----- |
| action      | string  | event name<br>fixed at:"client_connected" |
| clientid    | string  | client ClientId |
| username    | string  | client Username, When not existed, the value is "undefined" |
| ipaddress   | string  | client source IP address |
| keepalive   | integer | Heartbeat keepalive time applied by client |
| proto_ver   | integer | Protocol version number |
| connected_at| integer | Timestamp (second) |


**client.disconnected**

| Key         | Type   | Description                                                 |
| ----------- | ------- | ----- |
| action      | string  | event name<br>fixed at: "client_disconnected"               |
| clientid    | string  | client ClientId |
| username    | string  | client Username, When not existed, the value is "undefined" |
| reason      | string  | error reason |


**client.subscribe**

| Key         | Type   | Description                                                 |
| ----------- | ------- | ----- |
| action      | string  | event name<br/>fixed at: "client_subscribe" |
| clientid    | string  | Client ClientId |
| username    | string  | Client Username, When not existed, the value is "undefined" |
| topic       | string  | Topics to be subscribed |
| opts        | json    | Subscription parameters |

opts includes

| Key  | Type | Description                                      |
| ---- | ---- | ---- |
| qos  | enum | QoS level, and the optional value is `0` `1` `2` |

**client.unsubscribe**

| Key         | Type   | Description                                                 |
| ----------- | ------- | ----- |
| action      | string  | event name<br/>fixed at:"client_unsubscribe" |
| clientid    | string  | client ClientId |
| username    | string  | client Username, When not existed, the value is "undefined" |
| topic       | string  | unsubscribed topic |

**session.subscribed**: same as `client.subscribe`，action is `session_subscribed`

**session.unsubscribed**: same as `client.unsubscribe`，action is `session_unsubscribe`

**session.terminated**: same as `client.disconnected`，action is `session_terminated`

**message.publish**

| Key            | Type    | Description                                                  |
| -------------- | ------- | ----- |
| action         | string  | event name<br/>fixed at: "message_publish" |
| from_client_id | string  | Publisher's ClientId |
| from_username  | string  | Publisher's Username, When not existed, the value is "undefined" |
| topic          | string  | Unsubscribed topic |
| qos            | enum    | QoS level, and the optional value is `0` `1` `2` |
| retain         | bool    | Whether it is a Retain message |
| payload        | string  | Message Payload |
| ts             | integer | Timestamp (second) |


**message.delivered**

| Key            | Type    | Description                                                  |
| -------------- | ------- | ----- |
| action         | string  | event name<br/>fixed at: "message_delivered" |
| clientid       | string  | Receiver's ClientId |
| username       | string  | Receiver's Username, When not existed, the value is "undefined" |
| from_client_id | string  | Publisher's ClientId |
| from_username  | string  | Publisher's Username, When not existed, the value is "undefined" |
| topic          | string  | Unsubscribed topic |
| qos            | enum    | QoS level, and the optional value is `0` `1` `2` |
| retain         | bool    | Whether it is a Retain message |
| payload        | string  | Message Payload |
| ts             | integer | Timestamp (second) |


**message.acked**

| Key            |  Type   | Description  |
| -------------- | ------- | ----- |
| action         | string  | event name<br/>fixed at: "message_acked" |
| clientid       | string  | Receiver's ClientId |
| from_client_id | string  | Publisher's ClientId |
| from_username  | string  | Publisher's Username, When not existed, the value is "undefined" |
| topic          | string  | Unsubscribed topic |
| qos            | enum    | QoS level, and the optional value is `0` `1` `2` |
| retain         | bool    | Whether it is a Retain message |
| payload        | string  | Message Payload |
| ts             | integer | Timestamp (second) |
