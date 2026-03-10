# Advanced MQTT

::: tip Note

Advanced MQTT features are only available in the EMQX Enterprise edition.

:::

## Topic Rewrite

The Topic Rewrite feature allows modification of device business topics. By setting up rules in EMQX, it can rewrite the original topic to a new target topic during subscription or publishing. This page enables users to add topic rewriting rules via the Dashboard without modifying configuration files. For detailed topic rewriting rules, refer to [Topic Rewrite](../messaging/mqtt-topic-rewrite.md).

## Auto-Subscription

Auto-subscription is an MQTT extension feature supported by EMQX. It allows EMQX to automatically subscribe to specified topics for devices based on predefined rules after they successfully connect, without requiring additional subscription requests. This page allows users to configure the auto-subscription feature via the Dashboard. For detailed configuration guidance, refer to [Auto-Subscription](../messaging/mqtt-auto-subscription.md).

## Delayed Publish

Delayed publish is an MQTT extension feature supported by EMQX. When a client publishes a message using a special topic prefix `$delayed/{DelayInterval}`, the delayed publish feature is triggered, allowing the message to be published after a user-configured delay interval. This page enables users to configure the delayed publish feature via the Dashboard. For detailed configuration guidance, refer to [Configure Delayed Publish via Dashboard](../messaging/mqtt-delayed-publish.md#configure-delayed-publish-via-dashboard).

## File Transfer

File transfer based on MQTT is an advanced feature of EMQX Enterprise edition. EMQX extends the MQTT protocol to allow client devices to transfer, manage, and store offline file data, such as audio, video, images, and diagnostic logs, in addition to transmitting real-time structured data like sensor data and control instructions. You can configure this feature on the file transfer page. For detailed configuration guidance, refer to [Enabling and Configuring File Transfer via Dashboard](../file-transfer/broker.md#enable-and-configure-file-transfer-via-dashboard).