# Subscription Filters

The Subscription Filter feature introduced in EMQX 6.2 extends the MQTT 5.0 publish/subscribe model with content-based filtering at the subscription level. It allows clients to receive only the subset of messages that match both a topic filter and an additional filter expression, reducing unnecessary message delivery and network overhead.

This page provides a complete overview of Subscription Filters in EMQX, covering design motivation, key concepts, filter expression syntax, behavioral semantics, and real-world use cases.

## What Is a Subscription Filter?

A Subscription Filter is an optional filter condition attached to an MQTT subscription. When a published message matches the subscription's topic filter, EMQX evaluates the filter expression against the message's MQTT 5.0 User Properties before forwarding. Only messages that satisfy both the topic filter and the filter expression are delivered to the subscriber.

Standard MQTT routing forwards every topic-matched message to the subscriber:

```
Publisher --> Topic -- (Filter) --> Subscription --> Subscriber
```

Subscription Filters introduce a second level of filtering in the message routing path:

```
Publisher --> Topic -- (Filter) --> Subscription -- (Filter) --> Subscriber
```

This two-level filtering mechanism allows for both topic-based and content-based filtering, enabling clients to precisely specify the messages they wish to receive.

## Why Use Subscription Filters?

Standard MQTT 5.0 subscriptions route messages based solely on topic matching. All messages published to a matching topic are delivered to every subscriber, regardless of message content. This can be limiting in scenarios where:

- Subscribers are only interested in messages from a specific region, device group, or category.
- High-frequency topics carry mixed data that different consumers need to partition independently.
- Delivering all messages to clients increases network usage and processing load unnecessarily.

Subscription Filters address these limitations by allowing precise, content-aware delivery rules to be declared at subscription time. No changes to publishers, topic structures, or separate topics per data dimension are required.

## Key Concepts

- **Topic Filter**: The standard MQTT topic filter portion of the subscription (the part before `?`). Determines which messages enter the routing stage.

- **Filter Expression**: The content-based filter condition (the part after `?`). Evaluated against the MQTT 5.0 User Properties of each message that passes the topic filter.

- **User Properties**: Key-value metadata attached to an MQTT 5.0 message. Publishers include these to provide additional context that subscribers can filter on, such as `location`, `device_type`, or `region`.

- **Two-Stage Delivery**: The combined evaluation of the topic filter followed by the filter expression before a message is delivered to the subscriber.

- **Unfiltered Subscription**: A subscription without a `?` delimiter. Treated as a standard MQTT subscription, with all topic-matched messages delivered regardless of content.

## How Subscription Filters Work

Subscription Filters use MQTT 5.0 **User Properties** as the filtering surface. When a client publishes a message, it may include key-value pairs in the message's User Properties header. EMQX evaluates each filter expression against these key-value pairs and delivers the message only if the expression matches.

Subscription Filters are disabled by default. For instructions on enabling the feature, see [Get Started with Subscription Filters](./subscription-filter-get-started.md).

### Filter Syntax

A Subscription Filter is appended to the topic filter using `?` as a delimiter:

```
<topic-filter>?<filter-expression>
```

| Component | Description |
|---|---|
| `<topic-filter>` | A standard MQTT topic filter, e.g., `sensor/+/temperature` or `home/#` |
| `?` | Delimiter separating the topic filter from the filter expression |
| `<filter-expression>` | A key-value filter condition evaluated against the message's User Properties |

### Filter Expression Format

Filter expressions support equality and comparison operators. Multiple conditions are combined with `&` (logical AND):

```
key1=value1&key2>value2
```

| Element | Description |
|---|---|
| `key` | The name of a User Property key in the published message |
| `=` | Equality match (the key's value must equal the specified string) |
| `>` | Numeric comparison (the key's value must be numerically greater than the specified number) |
| `>=` | Numeric comparison (the key's value must be numerically equal or greater than the specified number) |
| `<` | Numeric comparison (the key's value must be numerically smaller than the specified number) |
| `<=` | Numeric comparison (the key's value must be numerically equal or smaller than the specified number) |
| `&` | Combines multiple conditions; all must be true for the message to be delivered |

Filter expressions are **case-sensitive**. If a specified key is absent from the message's User Properties, the message is filtered out.

::: tip

Subscription Filters apply to MQTT 5.0 clients only. MQTT 3.1.1 clients that subscribe with a `?`-containing topic string will have the full string treated as a literal topic filter.

:::

## Behavioral Semantics

- EMQX delivers a message to a subscriber only when **both** the topic filter matches **and** the filter expression evaluates to true.
- If the filter expression references a key not present in the message's User Properties, the message is **not delivered** to that subscriber.
- Each subscription's filter expression is evaluated independently. Whether a message is delivered to one subscriber has no effect on whether it is delivered to other subscribers on the same topic.
- Subscriptions without a `?` delimiter behave identically to standard MQTT subscriptions.
- Filter expressions are evaluated server-side. Clients are not responsible for any filtering logic.

## Filter Expression Examples

The following examples illustrate common subscription patterns:

| Subscription String | Meaning |
|---|---|
| `sensor/+/temperature?location=roomA` | Receive temperature messages where User Properties include `location=roomA`. |
| `sensor/+/temperature?value>25` | Receive temperature messages where the `value` User Property is numerically greater than 25. |
| `sensor/+/temperature?location=roomA&unit=celsius` | Receive temperature messages where both `location=roomA` and `unit=celsius`. |
| `home/lights/#` | Standard subscription. All messages on matching topics are delivered). |

### Publisher Side

A publisher sends a message to `sensor/1/temperature` with User Properties:

```json
{
  "location": "roomA",
  "unit": "celsius"
}
```

### Subscriber Side

| Subscription | Delivered? | Reason |
|---|---|---|
| `sensor/+/temperature?location=roomA` | Yes | `location=roomA` matches |
| `sensor/+/temperature?location=roomB` | No | `location` value does not match |
| `sensor/+/temperature?location=roomA&unit=celsius` | Yes | Both conditions match |
| `sensor/+/temperature?location=roomA&unit=fahrenheit` | No | `unit` value does not match |
| `sensor/+/temperature` | Yes | No filter expression — standard subscription |

## Authorization Considerations

When [Authorization](../access-control/authz/authz.md) is enabled, EMQX validates the subscription topic against configured rules. The topic used for authorization is the **base topic filter** (the portion before the `?` delimiter). The filter expression is stripped before authorization is evaluated.

For example, a client subscribing to `sensor/+/temperature?location=roomA` is authorized against `sensor/+/temperature`. Ensure your authorization rules account for the base topic patterns used with Subscription Filters.

## Related Features Reference

Subscription Filters complement other messaging features in EMQX:

- [Shared Subscriptions](../messaging/mqtt-shared-subscription.md): Distributes messages among a subscriber group for load balancing. Does not support content-based filtering.
- [Retained Messages](../messaging/mqtt-retained-message.md): Stores the last message per topic for delivery to new subscribers. Retained message delivery is not affected by Subscription Filter expressions.
- [Topic Rewrite](../messaging/mqtt-topic-rewrite.md): Rewrites topic strings before routing. Topic rewrite rules are applied before Subscription Filter evaluation.
- [Wildcard Subscription](../messaging/mqtt-wildcard-subscription.md): Matches multiple topics using `+` and `#` wildcards. Wildcard topic filters can be combined with Subscription Filters.
- [Message Queue](../message-queue/message-queue-concept.md): Provides durable, asynchronous message queuing with persistent storage and configurable dispatch strategies. <!-- Needs to be verified-->

## Next Steps

Now that you understand Subscription Filter concepts, explore how to use them in practice:

- [Get Started with Subscription Filters](./subscription-filter-get-started.md): Enable the feature and follow a step-by-step walkthrough using MQTTX CLI to verify filter behavior end-to-end.
