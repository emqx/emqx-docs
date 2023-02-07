# Explore MQTT

To meet the needs of various IoT application scenarios， EMQX provides full support to a complete set of MQTT messaging features, for example, Will messages, Reserved messages, and Shared Subscriptions. All these features can greatly reduce development and managing difficulties.

This chapter will introduce how to use these features and how to verify them with [MQTT X CLI](https://mqttx.app/cli), to help you get familiar with the MQTT messaging services.

:::tip Prerequisites:

- Knowledge about [Publish and subscribe](./mqtt-publish-and-subscribe.md)
  :::

## Will message

**Operating steps:**

1. Enable Will Message at the broker side (enabled by default on EMQX). 
2. Set the topic and payload of the will message when the client initiates the connect request.
3. Subscribe to the topic of the will message with another client, and get ready to receive the will message. 
4. When the client is disconnected or closed, the broker will first send the preset will message and then forward it to online clients that subscribe to this topic.

**Use MQTT X CLI to verify:**

1. Initiate a connection request with one client, set the topic to `t/1` and payload to `A will message from MQTTX CLI`:

   ```bash
   $ mqttx conn -h 'localhost' -p 1883 --will-topic 't/1' --will-message 'A will message from MQTTX CLI'
   Connected
   ```

2. Subscribe to topic `t/1` with another client for receiving the will messages:

   ```bash
   mqttx sub -t 't/1' -h 'localhost' -p 1883 -v
   ```

3. Disconnect the client specified in Step 1, then the client specified in Step 2 will receive the will message:

   ```bash
   topic:  t/1
   payload:  A will message from MQTTX CLI
   ```

## Retained message

Operating steps:

1. Enable Retained message at the broker side (enabled by default on EMQX). 
2. Set `retain = true` when publishing a message, this message will be sent to the subscriber first, and remain as a retained message under the corresponding topic.
3. When any client subscribes to the **same topic**, the retained message will be sent to the subscriber.
4. When any client publishes an empty **retained message**  to the **same topic**, the retained message under the topic will be cleared.

Use MQTT X CLI to verify:

1. Initiate a connection request with one client, set the topic to `t/1`, payload to `A retained message from MQTTX CLI`,  and `retain = true`：

   ```bash
   mqttx pub -t 't/1' -m 'A retained message from MQTTX CLI' --retain true -h 'localhost' -p 1883
   ```

2. Resubscribe to topic `t/1` with another client and it will receive the retained message, repeat this step and it will continuously receive the retained message:

   ```bash
   $ mqttx sub -t 't/1' -h 'localhost' -p 1883 -v
   topic:  t/1
   payload:  A retained message from MQTTX CLI
   ```

3. Publish an empty message to clear the retained message:

   ```bash
   mqttx pub -t 't/1' -m '' --retain true -h 'localhost' -p 1883
   ```

4. Repeat Step 2 and no retained messages are received, indicating the retained message is cleared. 

## Shared Subscription

**Operating steps:**

1. Enable Shared Subscription at the broker side (enabled by default on EMQX). 
2. When multiple subscribers subscribe to the same topic (original topic), they need to add a prefix to indicate shared subscribe group `$share/{group}` (shared subscribe topic), for example,
   - If the original topic is `t/1`, the shared subscription topic is `$share/my_group/t/1` (with `my_group` as the customized group name)
   - If the original ancestor is `/t/1`, the shared subscription topic is `$share/my_group//t/1`.
3. Use any client to publish multiple pieces of messages to the original topic sequentially, the messages will be forwarded to the subscribers following the preset shared subscribe rule. Each subscriber of the group will only receive one message at a time.

**Use MQTT X CLI to verify:**

1. Four subscribers are divided into 2 groups to subscribe to topic  `t/1`:

   ```bash
   # Client A and B subscribe to topic `$share/my_group1/t/1`
   mqttx sub -t '$share/my_group1/t/1' -h 'localhost' -p 1883
   
   ## Client C and D subscribe to topic  `$share/my_group2/t/1`
   mqttx sub -t '$share/my_group2/t/1' -h 'localhost' -p 1883
   ```

2. Use a new client to publish 4 messages with payloads `1`, `2`, `3`, and `4` to the original topic `t/1`:

   ```bash
   mqttx pub -t 't/1' -m '1' -h 'localhost' -p 1883
   mqttx pub -t 't/1' -m '2' -h 'localhost' -p 1883
   mqttx pub -t 't/1' -m '3' -h 'localhost' -p 1883
   mqttx pub -t 't/1' -m '4' -h 'localhost' -p 1883
   ```

3. Check the message received by the clients within each subscription group:

   - Subscription group (A and B) and Subscription group (C and D) simultaneously receive the messages.
   - One message will only be received once within each group.
