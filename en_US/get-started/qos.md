---
description: "Compare MQTT QoS levels and understand what delivery acknowledgments do and do not guarantee for application processing."
---

# Understand QoS

Choose a Quality of Service (QoS) level for each message exchange. [Publish and Subscribe](publish-subscribe.md) used QoS 1 for both clients; use the comparison below to understand that choice.

<!-- include: ../develop/mqtt/basics.md#quality-of-service-qos -->

## Read the Result Correctly

In the earlier task, receiving the exact payload was the success check. Keep that check when choosing another QoS: a publisher acknowledgment alone does not show that the receiving application processed the message.

## Continue Learning

Next, [authenticate with a username and password](password-authentication.md) so that your local broker can identify clients.

<!-- Migration follow-up: Update the messaging overview link after its rebuilt destination is migrated. -->

For delivery behavior, session settings, and related MQTT features, see [Messaging Features](messaging/introduction.md).

When you finish using the test broker, follow [Quick Start: Clean Up](quick-start.md#clean-up).
