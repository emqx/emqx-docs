---
enterprise: true
---
# Topic Rewrite

The topic rewriting function of EMQ X supports rewriting topic A to topic B when the client subscribes to topics, publishes messages, and cancels subscriptions according to user-configured rules.

## Create module

Open [EMQ X Dashboard](http://127.0.0.1:18083/#/modules), click the "Modules" tab on the left, and choose to add:

![image-20200927213049265](./assets/modules.png)

Select MQTT topic rewrite module

![image-20200927213049265](./assets/topic_rewrite_1.png)

Configure related parameters

![image-20200927213049265](./assets/topic_rewrite_2.png)

After clicking add, the module is added

![image-20200927213049265](./assets/topic_rewrite_3.png)

## topic rewriting rules

The rewrite rules are divided into Pub rules and Sub rules. Pub rules match the topics carried in PUSHLISH packets, and Sub rules match the topics carried in SUBSCRIBE and UNSUBSCRIBE packets.

Each rewrite rule consists of three parts: subject filter, regular expression, and target expression. Under the premise that the subject rewriting function is enabled, when EMQ X receives a subject-based MQTT message such as a PUBLISH message, it will use the subject of the message to sequentially match the subject filter part of the rule in the configuration file. If the match is successful, the regular expression is used to extract the information in the subject, and then replaced with the target expression to form a new subject.

Variables in the format of `$N` can be used in the target expression to match the elements extracted from the regular expression. The value of `$N` is the Nth element extracted from the regular expression. For example, `$1` is the regular expression. The first element extracted by the expression.

It should be noted that EMQ X uses reverse order to read the rewrite rules in the configuration file. When a topic can match the topic filter of multiple topic rewrite rules at the same time, EMQ X will only use the first rule it matches. Rewrite. If the regular expression in this rule does not match the subject of the MQTT message, the rewriting will fail, and no other rules will be attempted for rewriting. Therefore, users need to carefully design MQTT message topics and topic rewriting rules when using them.

## topic rewrite example

Add the topic rewriting rules in the above figure and subscribe to `y/a/z/b`, `y/def`, `x/1/2`, `x/y/2`, `x/y/z `Five topic:

+ When the client subscribes to the topic of `y/def`, `y/def` does not match any topic filter, so no topic rewriting is performed, and the topic of `y/def` is directly subscribed.

+ When the client subscribes to the topic `y/a/z/b`, `y/a/z/b` matches the topic filter of `y/+/z/#`, and EMQ X executes `module.rewrite.sub. rule.1` rule, the element `[a, b]` is matched through regular regular expressions, and the second element that is matched is brought into `y/z/$2`, which actually subscribes to `y/z/b` theme.

+ When the client sends a message to the `x/1/2` topic, `x/1/2` matches the `x/#` topic filter, and EMQ X executes the `module.rewrite.pub.rule.1` rule, If the element is not matched by the regular expression, the subject rewriting is not performed, so the message is sent directly to the `x/1/2` subject.

+ When the client sends a message to the subject of `x/y/2`, `x/y/2` matches both `x/#` and `x/y/+` two topic filters, EMQ X reads in reverse order The configuration is taken, so `module.rewrite.pub.rule.2` is matched first, and the message is actually sent to the topic `z/y/2` through regular replacement.

+ When the client sends a message to the topic `x/y/z`, `x/y/z` matches both `x/#` and `x/y/+` two topic filters, EMQ X reads in reverse order The configuration is taken, so `module.rewrite.pub.rule.2' is first matched. If the element is not matched by the regular expression, the subject rewriting is not performed, and the message is sent directly to the `x/y/z` topic. It should be noted that even if the regular expression of `module.rewrite.pub.rule.2` fails to match, it will not match the rule of `module.rewrite.pub.rule.1` again.