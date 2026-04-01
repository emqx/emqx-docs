# Get Started with Subscription Filters

This page walks you through enabling the Subscription Filter feature in EMQX and verifying it with a hands-on demonstration. You will use MQTTX CLI to simulate a publisher and multiple subscribers, and observe how filter expressions control which messages each subscriber receives.

## Prerequisites

Before starting, ensure you have:

- EMQX 6.2+ running
- [MQTTX CLI](https://mqttx.app/cli) installed

## Step 1: Enable Subscription Filters

Subscription Filters are disabled by default. When disabled, the `?` character is treated as a regular part of any topic string, preserving full backward compatibility with existing subscriptions.

### Via Dashboard

1. Go to **Management** -> **MQTT Settings** -> **General** tab.
2. Locate the **Subscription Message Filter** field and set it to **enable**.
3. Click **Save Changes**.

Changes take effect immediately without restarting the broker.

### Via Configuration File

Add the following to `emqx.conf`:

```hocon
mqtt.subscription_message_filter = enable
```

Restart EMQX, or use a hot-reload if your deployment supports it, for the change to take effect.

### Via REST API

```bash
curl -s -u key:secret -X PUT \
  -H "Content-Type: application/json" \
  http://localhost:18083/api/v5/configs/mqtt \
  -d '{"subscription_message_filter": "enable"}'
```

Once enabled, clients can attach filter expressions to their subscriptions. For syntax details and examples, see [Filter Syntax](./subscription-filter-concept.md#filter-syntax) in the Subscription Filters overview.

## Step 2: Start the Subscribers

In this walkthrough, a sensor publishes temperature readings to `sensor/1/temperature`. Each message includes a `location` User Property. Three subscribers listen on the same topic with different filter expressions:

| Subscriber | Subscription | Receives messages where... |
|---|---|---|
| `sub-roomA` | `sensor/+/temperature?location=roomA` | `location=roomA` |
| `sub-roomB` | `sensor/+/temperature?location=roomB` | `location=roomB` |
| `sub-all` | `sensor/+/temperature` | All messages (no filter) |

Open three terminal windows and start each subscriber.

**Terminal 1: roomA subscriber**

```bash
mqttx sub -h localhost -p 1883 \
  --mqtt-version 5 \
  --client-id sub-roomA \
  -t "sensor/+/temperature?location=roomA"
```

**Terminal 2: roomB subscriber**

```bash
mqttx sub -h localhost -p 1883 \
  --mqtt-version 5 \
  --client-id sub-roomB \
  -t "sensor/+/temperature?location=roomB"
```

**Terminal 3: unfiltered subscriber**

```bash
mqttx sub -h localhost -p 1883 \
  --mqtt-version 5 \
  --client-id sub-all \
  -t "sensor/+/temperature"
```

::: tip

The `--mqtt-version 5` flag is required. Subscription Filters rely on MQTT 5.0 protocol features.

:::

## Step 3: Publish a Message for Room A

In a fourth terminal, publish a message with `location=roomA` in the User Properties:

```bash
mqttx pub -h localhost -p 1883 \
  --mqtt-version 5 \
  --client-id publisher \
  -t "sensor/1/temperature" \
  -m '{"value": 23.5}' \
  --user-properties "location: roomA"
```

**Expected results:**

| Subscriber | Receives message? |
|---|---|
| `sub-roomA` | Yes (`location=roomA` matches) |
| `sub-roomB` | No (`location` value does not match) |
| `sub-all` | Yes (no filter expression) |

## Step 4: Publish a Message for Room B

```bash
mqttx pub -h localhost -p 1883 \
  --mqtt-version 5 \
  --client-id publisher \
  -t "sensor/1/temperature" \
  -m '{"value": 19.1}' \
  --user-properties "location: roomB"
```

**Expected results:**

| Subscriber | Receives message? |
|---|---|
| `sub-roomA` | No (`location` value does not match) |
| `sub-roomB` | Yes (`location=roomB` matches) |
| `sub-all` | Yes (no filter expression) |

## Step 5: Test Multiple Conditions (AND Logic)

Subscription Filters support multiple conditions joined with `&`. Start a new subscriber that requires both `location` and `unit` to match:

```bash
mqttx sub -h localhost -p 1883 \
  --mqtt-version 5 \
  --client-id sub-roomA-celsius \
  -t "sensor/+/temperature?location=roomA&unit=celsius"
```

Publish a message that satisfies both conditions:

```bash
mqttx pub -h localhost -p 1883 \
  --mqtt-version 5 \
  --client-id publisher \
  -t "sensor/1/temperature" \
  -m '{"value": 22.0}' \
  --user-properties "location: roomA" \
  --user-properties "unit: celsius"
```

The `sub-roomA-celsius` subscriber receives the message. Now publish a message with a mismatched `unit`:

```bash
mqttx pub -h localhost -p 1883 \
  --mqtt-version 5 \
  --client-id publisher \
  -t "sensor/1/temperature" \
  -m '{"value": 71.6}' \
  --user-properties "location: roomA" \
  --user-properties "unit: fahrenheit"
```

The `sub-roomA-celsius` subscriber does **not** receive this message, even though `location=roomA` matches, because the `unit` condition is not satisfied.

## Step 6: Publish a Message Without User Properties

Publish a bare message with no User Properties:

```bash
mqttx pub -h localhost -p 1883 \
  --mqtt-version 5 \
  --client-id publisher \
  -t "sensor/1/temperature" \
  -m '{"value": 20.0}'
```

**Expected results:**

| Subscriber | Receives message? |
|---|---|
| `sub-roomA` | No (the `location` key is absent) |
| `sub-roomB` | No (the `location` key is absent) |
| `sub-all` | Yes, no filter expression |

This confirms that when a required User Property key is missing, the message is filtered out for subscribers with filter expressions.

## Summary

| Scenario | Behavior |
|---|---|
| Message User Properties match the filter expression | Delivered |
| Message User Properties partially match (AND condition unmet) | Not delivered |
| Required User Property key is absent | Not delivered |
| Subscription has no filter expression | All topic-matched messages delivered |

## Next Steps

- [Subscription Filters Overview](./subscription-filter-concept.md): Understand the design, concepts, and use cases in depth.
- [Wildcard Subscription](../messaging/mqtt-wildcard-subscription.md): Combine wildcard topic filters with Subscription Filters for flexible routing.
