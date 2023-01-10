# Will Message
Will message refers to the "Will" message EMQX sends to relevant subscribers if a client is accidentally disconnected, to inform the subscriber the client is offline and update the client status. 

Accidental disconnection refers to that a client being disconnected without sending the `DISCONNECT` message, for example:

- The client does not send any messages within the defined Keep alive period for network failure, and EMQX closes the connection;
- The client is accidentally powered off;
- The client initiated an unauthorized operation and is disconnected by EMQX, for example, subscribing to topics that it is not allowed to.

::: tip
In some cases, EMQX may use [Rules](../data-integration/rules.md) instead of Will message for more flexibility. 
:::

The Will message can be set when the MQTT client sends a `CONNECT` message, and you can customize the Will message with the following fields (optional):

- Will Message flag
- Topic
- Payload
- Properties

Note: There will be a delay when EMQX publishes the Will message, as EMQX has to wait till the specified Keep Alive period before it can confirm the disconnection of the client. The Will Delay Interval <!--是不是插入超链接？--> of MQTT 5.0 may also affect the publishing time. 

::: tip

For more information on the Will mechanism, see blog [*Use of MQTT Will Message*](https://www.emqx.com/en/blog/use-of-mqtt-will-message).
:::

