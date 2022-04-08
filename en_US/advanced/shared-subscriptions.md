## Shared subscription

Shared subscription is a subscription method that achieves load balancing among multiple subscribers:

```bash
                                                   [subscriber1] got msg1
             msg1, msg2, msg3                    /
[publisher]  ---------------->  "$share/g/topic"  -- [subscriber2] got msg2
                                                 \
                                                   [subscriber3] got msg3
```

In the above picture, three subscribers subscribe to the same topic `$share/g/topic` using a shared subscription method, where ` topic` is the real topic name they subscribed to, and `$share/g/`  is a shared subscription Prefix. EMQX Broker supports shared subscription prefixes in two formats:

| Example         | Prefix      | Real topic name |
| --------------- | ----------- | --------------- |
| $queue/t/1      | $queue/     | t/1             |
| $share/abc/t/1  | $share/abc  | t/1             |


### Shared subscription with groups

Shared subscriptions prefixed with `$ share/<group-name>` are shared subscriptions with groups:

group-name can be any string. Subscribers who belong to the same group will receive messages with load balancing, but EMQX Broker will broadcast messages to different groups.

For example, suppose that subscribers s1, s2, and s3 belong to group g1, and subscribers s4 and s5 belong to group g2. Then when EMQX Broker publishes a message msg1 to this topic:

- EMQX Broker will send msg1 to both groups g1 and g2
- Only one of s1, s2, s3 will receive msg1
- Only one of s4 and s5 will receive msg1

```bash
                                       [s1]
           msg1                      /
[emqx]  ------>  "$share/g1/topic"    - [s2] got msg1
         |                           \
         |                             [s3]
         | msg1
          ---->  "$share/g2/topic"   --  [s4]
                                     \
                                      [s5] got msg1
```

### Shared subscription without group

Shared subscriptions prefixed with `$queue/` are shared subscriptions without groups. It is a special case of `$share` subscription, which is quite similar to all subscribers in a subscription group:

```bash
                                       [s1] got msg1
        msg1,msg2,msg3               /
[emqx]  --------------->  "$queue/topic" - [s2] got msg2
                                     \
                                       [s3] got msg3
```

### Balancing strategy and distribution of Ack configuration

EMQX Broker's shared subscription supports balancing strategy and distribution of Ack configuration:

```bash
# etc/emqx.conf

# balancing strategy
broker.shared_subscription_strategy = random

# Per-group balancing strategy
broker.$group_name.shared_subscription_strategy = local

# Applicable to QoS1 QoS2 messages, when enabled, message will be distributed to another group when one group is offline
broker.shared_dispatch_ack_enabled = false
```
<!-- TODO 待确认 -->

| Balancing strategy |             Description             |
| :---------- | :--------------------------- |
| random      | Select randomly among all subscribers |
| round_robin | According to the order of subscription |
| sticky      | Always sent to the last selected subscriber |
| hash        | According to the hash value of the publisher ClientID |
| local       | Selects random subscriber connected to the node which received the message. If no such subscribers present, selects a random cluster-wise |

::: tip
Whether it is a single client subscription or a shared subscription, pay attention to the client performance and message reception rate, otherwise it will cause errors such as message accumulation and client crash.
:::
