# Topic rewrite

The topic rewrite function of EMQX Broker supports rewriting topic A to topic B when the client subscribes to topics, publishes messages, and unsubscribes according to user-configured rules.

EMQX Broker's [retained message](./retained.md) and [delayed publish](./delayed-publish.md) can be used in conjunction with topic rewriting. For example, when users want to use the delayed publish function, they can use topic rewrite function to rewrite the related topic to the topic format for delayed publication if it is not convenient for modifying the topic published by the client.

Since the ACL check will be performed before the topic is rewritten, just make sure that the topic can pass the ACL check before rewriting.

## Enable topic rewrite

The topic rewrite function is disabled by default. To enable this function, you need to modify the `module.rewrite` configuration item in the ` etc/emqx.conf` file. The default `off` means disabled, if you want to enable it, please change it to ` on`.

```bash
module.rewrite = off
```

## Configure topic rewrite rules

EMQX Broker's topic rewrite rules need to be configured by the user. The user can add multiple topic rewrite rules. The number of rules is not limited, but any MQTT message that carries a topic needs to match the rewrite rule again. Therefore, the performance loss in high-throughput scenarios is proportional to the number of rules, and users need to use this feature with caution.

The format of rewrite rule for each topic is as follows:

```bash
module.rewrite.rule.<number> = Topic filter Regular expression Target expression
```

Each rewrite rule consists of a topic filter, regular expression, and target expression separated by spaces. On the premise that the topic rewrite function is enabled, when receiving MQTT packet such as PUBLISH messages with a topic, EMQX Broker will use the topic in the packet to sequentially match the topic filter part of the rules in the configuration file. Once the match is successful, the regular expression is used to extract the information in the topic, and then replaced to the target expression to form a new topic.

The target expression can use variables in the format of `$N` to match the elements extracted from the regular expression. The value of `$N` is the Nth element extracted from the regular expression, for example, `$1` is the first element extracted by the regular expression.

It should be noted that EMQX Broker reads the rewrite rules in the configuration file in reverse order. When a topic can match the topic filters of multiple topic rewrite rules at the same time, EMQX Broker will only use the first matching rule to rewrite the topic. If the regular expression in the rule does not match the topic of the MQTT packet, the rewrite fails, and no other rule will be used to rewrite. Therefore, users need to carefully design MQTT packet topics and topic rewrite rules when using them.

## Example

Assume that the following topic rewrite rules have been added to the `etc/emqx.conf` file:

```bash
module.rewrite.rule.1 = y/+/z/# ^y/(.+)/z/(.+)$ y/z/$2
module.rewrite.rule.2 = x/# ^x/y/(.+)$ z/y/x/$1
module.rewrite.rule.3 = x/y/+ ^x/y/(\d+)$ z/y/$1
```

At this time we subscribe to five topics:  `y/a/z/b`, `y/def`, `x/1/2`, `x/y/2`, and `x/y/z` :

+ `y/def` does not match any of the topic filters, so it does not perform topic rewriting, and just subscribe to `y/def` topics.
+ `y/a/z/b` matches the  `y/+/z/#` topic filter, EMQX Broker executes the` module.rewrite.rule.1` rule, and matches the element `[a、b]` through a regular expression, bring the matched second element into `y/z/$2`, and actually subscribe to the topic `y/z/b`.
+ `x/1/2` matches `x/#` topic filter, EMQX Broker executes the `module.rewrite.rule.2` rule. It does not match elements through regular expressions, does not perform topic rewrite, and actually subscribes to the topic of `x/1/2`.
+ `x/y/2`  matches two topic filters of `x/#` and `x/y/+` at the same time, EMQX Broker reads the configuration in reverse order, so it matches `module.rewrite.rule.3` in priority. Through regular replacement, it actually subscribed to the `z/y/2`  topic.
+ `x/y/z`  matches two topic filters of `x/#` and `x/y/+` at the same time, EMQX Broker reads the configuration in reverse order, so it matches `module.rewrite.rule.3` in priority. The element is not matched through the regular expression, the topic rewrite is not performed, and it actually subscribes to the `x/y/z` topic. It should be noted that even if the regular expression matching of `module.rewrite.rule.3` fails, it will not match the rules of `module.rewrite.rule.2`  again. 
