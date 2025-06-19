# Developer Guide

The Developer Guide is designed to provide developers with the information to get started with EMQX and build applications on top of it.

This chapter explores the core concepts of MQTT, MQTT-specific features, and some extended features in EMQX. We will also explain how to configure these features in the EMQX Dashboard and test them using client tools. The following features are covered in the guide:

- [MQTT Core Concepts](../messaging/mqtt-concepts.md)
- [Test with MQTT Clients](../messaging/publish-and-subscribe.md)
- [Shared Subscription](../messaging/mqtt-shared-subscription.md)
- [Retained Message](../messaging/mqtt-retained-message.md)
- [Will Message](../messaging/mqtt-will-message.md)
- [Wildcard Subscription](../messaging/mqtt-wildcard-subscription.md)
- [Exclusive Subscription](../messaging/mqtt-exclusive-subscription.md)
- [Delayed Publish](../messaging/mqtt-delayed-publish.md)
- [Auto Subscribe](../messaging/mqtt-auto-subscription.md)
- [Topic Rewrite](../messaging/mqtt-topic-rewrite.md)

Additionally, this guide introduces EMQX's [MQTT Durable Session](../durability/durability_introduction.md) feature and provides steps for quickly experiencing this feature.

EMQX's support of the MQTT protocol makes it compatible with most MQTT client libraries and SDKs. The developer guide provides [step-by-step instructions and code samples](./introduction.md) to help developers start building their MQTT projects quickly. For a complete list of MQTT client SDKs and their comparison, see [MQTT client SDKs](https://www.emqx.com/en/mqtt-client-sdk).

::: tip

Not all SDKs are displayed on the document.

:::

EMQX also provides API documents to facilitate your development. [REST API](../admin/api.md) guides you on quickly getting started with the HTTP management API exposed by EMQX.