---
title: "Wildcard Subscriptions"
---

# Subscribe to Multiple Topics

Use `+` to match one topic level or `#` to match a topic and its descendants. Publish to concrete topic names; wildcards belong in subscription filters.

Use this guide to check matching rules, design topic filters, and troubleshoot subscriptions. For step-by-step CLI exercises with expected results, see [single-level matching](../../get-started/topics-and-wildcards.md#verify-single-level-matching) and [multi-level matching](../../get-started/topics-and-wildcards.md#verify-multi-level-matching) in [Use Topics and Wildcards](../../get-started/topics-and-wildcards.md). Your client needs subscribe permission for the filter, and the publisher needs permission for each topic.

## Choose a Filter

| Filter | Matches | Does Not Match |
| --- | --- | --- |
| `factory/+/temperature` | `factory/line1/temperature`, `factory/line2/temperature`, `factory//temperature` | `factory/temperature`, `factory/line1/device1/temperature` |
| `factory/#` | `factory`, `factory/temperature`, `factory/line1/device1/temperature` | `factory2/temperature` |
| `+` | One-level names such as `factory` | `factory/temperature`, names beginning with `$` |
| `#` | Ordinary topic names at any depth | Names beginning with `$`, such as `$SYS/brokers` |
| `$SYS/#` | Topics beneath the explicit `$SYS` prefix | Ordinary application topics |

`+` must occupy a whole level; `sensor+` is invalid. `#` must be the last, whole level; `sensor/#/temperature` and `sensor/bedroom#` are invalid. Empty levels are significant: `factory//temperature` contains an empty second level. Topic matching is case-sensitive, and a trailing slash changes the topic name. See the [MQTT 5 topic rules](https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Topic_Names_and).

## Design and Troubleshoot Filters

Use the narrowest filter that meets the consumer's job. A broad filter can increase fan-out, network traffic, and client queue pressure; size the workload using the [benchmark guide](../../guides/performance/benchmark-emqtt-bench.md), rather than treating wildcards as inherently unsuitable for production.

If a message is missing, check filter levels and case, subscribe acknowledgment, topic permissions, and [wildcard enablement](../../guides/configuration/mqtt.md#subscription-settings). `#` does not grant access to `$SYS` or bypass authorization. Overlapping subscriptions can change the number of deliveries depending on broker behavior and subscription options; do not use overlap as an application deduplication strategy. When comparing filters, use separate Client IDs for concurrent test subscribers.
