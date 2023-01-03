# Topic Rewrite

IoT devices may not be as easy to reconfigure or upgrade, as a result, if a subscriber is programmed to subscribe certain topics
it becomes challenging to change to a new topic.

EMQX's topic rewrite feature can help to make such changes easier: by giving EMQX a set of rules, it can help to alter
topic names for subscriptions or publishes.

EMQX's [retained message](./retained.md) and [delayed publish](./delayed-publish.md) can be used in conjunction with topic rewriting.
For example, when users want to use the delayed publish, they can use topic rewrite to redirect the message to a desired topic.

::: warning
Authorization checks are performed before the topic is rewritten.
:::

## Configure topic rewrite rules

EMQX topic rewrite rules need to be configured.
You may add multiple topic rewrite rules.
The number of rules is not limited, but any MQTT message that carries a topic needs to match the rewrite rule again.
Therefore, the performance overhead in high-throughput scenarios is proportional to the number of rules,
so, use this feature with caution.

The format of rewrite rule for each topic is as follows:

```bash
rewrite = [
  {
    action:       "all"
    source_topic: "x/#"
    dest_topic:   "x/y/z/$1"
    re:           "^x/y/(.+)$"
  }
]
```

Each rewrite rule consists of a filter, regular expression.

The rewrite rules are divided into `publish`, `subscribe` and `all` rules. The `publish` rule matches the topics carried by PUBLISH messages, and the `subscribe` rule matches the topics carried by SUBSCRIBE and UNSUBSCRIBE messages. The `all` rule is valid for topics carried by PUBLISH, SUBSCRIBE and UNSUBSCRIBE messages.

On the premise that the topic rewrite is enabled, when receiving MQTT packet such as PUBLISH messages with a topic,
EMQX will use the topic in the packet to sequentially match the topic filter part of the rules in the configuration file.
Once the match is successful the regular expression is used to extract the information in the topic,
and then the old topic is replaced by the target expression to form a new topic.

The target expression can use variables in the format of `$N` to match the elements extracted from the regular expression.
The value of `$N` is the Nth element extracted from the regular expression,
for example, `$1` is the first element extracted by the regular expression.

And the target expression alose support use `${clientid}` to represent the `client ID` and `${username}` to represent the client `username`.

It should be noted that EMQX reads the rewrite rules in order of the configuration file.
When a topic can match the topic filters of multiple topic rewrite rules at the same time,
EMQX uses the first matching rule to rewrite the topic.

If the regular expression in the rule does not match the topic of the MQTT packet,
the rewrite fails, and no other rule will be used to rewrite.
Therefore, users need to carefully design MQTT packet topics and topic rewrite rules.

## Example

Assume that the following topic rewrite rules have been added to the `etc/emqx.conf` file:

```bash
rewrite = [
  {
    action:       "all"
    source_topic: "y/+/z/#"
    dest_topic:   "y/z/$2"
    re:           "^y/(.+)/z/(.+)$"
  }
  {
    action:       "all"
    source_topic: "x/#"
    dest_topic:   "z/y/x/$1"
    re:           "^x/y/(.+)$"
  }
  {
    action:       "all"
    source_topic: "x/y/+"
    dest_topic:   "z/y/$1"
    re:           "^x/y/(\d+)$"
  }
]
```

At this time we subscribe to five topics:  `y/a/z/b`, `y/def`, `x/1/2`, `x/y/2`, and `x/y/z` :

+ `y/def` does not match any of the topic filters, so it does not perform topic rewriting, and just subscribe to `y/def` topics.
+ `y/a/z/b` matches the  `y/+/z/#` topic filter, EMQX executes the first rule, and matches the element `[a、b]` through a regular expression, bring the matched second element into `y/z/$2`, and actually subscribe to the topic `y/z/b`.
+ `x/1/2` matches `x/#` topic filter, EMQX executes the second rule. It does not match elements through regular expressions, does not perform topic rewrite, and actually subscribes to the topic of `x/1/2`.
+ `x/y/2`  matches two topic filters of `x/#` and `x/y/+` at the same time, EMQX reads the configuration in reverse order, so it matches third in priority. Through regular replacement, it actually subscribed to the `z/y/2`  topic.
+ `x/y/z`  matches two topic filters of `x/#` and `x/y/+` at the same time, EMQX reads the configuration in reverse order, so it matches third in priority. The element is not matched through the regular expression, the topic rewrite is not performed, and it actually subscribes to the `x/y/z` topic. It should be noted that even if the regular expression matching of third fails, it will not match the rules of second again.
