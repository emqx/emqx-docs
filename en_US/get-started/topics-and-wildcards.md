---
description: "Use MQTTX CLI to compare single-level and multi-level wildcard subscriptions and verify which messages arrive."
---

# Use Topics and Wildcards

Subscribe to a family of topics and compare which messages arrive with single-level and multi-level wildcards.

## Prerequisites

Complete [Publish and Subscribe](publish-subscribe.md), stop its subscriber with Ctrl+C, and keep the same disposable broker running. Use MQTTX CLI in two terminals on your computer. If you changed the MQTT host port, replace `1883` in every command below.

These examples use EMQX Enterprise 6.3.1 and MQTTX CLI 1.13.0. For another broker, replace the host and port and use its credentials and TLS settings. The subscriber needs permission for its topic filter, and the publisher needs permission for each topic.

## Wildcard Basics

- `+` matches one topic level and must occupy the whole level. For example, `factory/+/temperature` matches `factory/line1/temperature` but not `factory/line1/device1/temperature`.
- `#` matches a topic and its descendants and must occupy the final whole level. For example, `factory/#` matches both topics above.
- Use wildcards only in subscription filters. Publish to concrete topic names.

For complete matching rules, including empty levels and `$` prefixes, see [Choose a Filter](../develop/mqtt/wildcard-subscriptions.md#choose-a-filter).

## Verify Single-Level Matching

1. In Terminal 1 on your computer, subscribe and wait for subscription confirmation:

   ```bash
   mqttx sub -h 127.0.0.1 -p 1883 -V 5.0 -i wildcard-reader \
     -t 'factory/+/temperature' -q 1 -v
   ```

2. In Terminal 2, publish three distinct messages:

   ```bash
   mqttx pub -h 127.0.0.1 -p 1883 -V 5.0 -t factory/line1/temperature -q 1 -m reading-001
   mqttx pub -h 127.0.0.1 -p 1883 -V 5.0 -t factory/line2/temperature -q 1 -m reading-002
   mqttx pub -h 127.0.0.1 -p 1883 -V 5.0 -t factory/line1/device1/temperature -q 1 -m reading-003
   ```

3. Verify the result: the subscriber should receive `reading-001` and `reading-002` on their original topics. It should not receive `reading-003`, whose topic has an extra level.

## Verify Multi-Level Matching

1. Stop the subscriber in Terminal 1 with Ctrl+C. Start a fresh subscription:

   ```bash
   mqttx sub -h 127.0.0.1 -p 1883 -V 5.0 -i wildcard-reader \
     -t 'factory/#' -q 1 -v
   ```

2. After subscription confirmation, repeat the three publications from Terminal 2. Verify that all three messages arrive.

Stop the test subscriber when finished. These publications do not set RETAIN, so they do not create retained values.

## Continue Learning

Next, [understand QoS](qos.md) to choose how MQTT acknowledges a delivery. Keep the broker running; after finishing the learning path, follow [Quick Start: Clean Up](quick-start.md#clean-up).

For missing messages, broad filters, and overlapping subscriptions, see [Design and Troubleshoot Filters](../develop/mqtt/wildcard-subscriptions.md#design-and-troubleshoot-filters).
