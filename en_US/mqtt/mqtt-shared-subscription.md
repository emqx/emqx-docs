# Shared Subscription

Shared subscription is a subscription method that achieves load balancing among multiple subscribers.

```txt
                                                   [subscriber1] got msg1
             msg1, msg2, msg3                    /
[publisher]  ---------------->  "$share/g/topic"  -- [subscriber2] got msg2
                                                 \
                                                   [subscriber3] got msg3
```

In the above diagram, three subscribers subscribe to the same topic `$share/g/topic` using a shared subscription method,
where ` topic` is the real topic name they subscribed to, and `$share/g/` is a shared subscription prefix.

| Example         | Prefix      | Topic Name |
| --------------- | ----------- | ---------- |
| $share/abc/t/1  | $share/abc/ | t/1        |

## Shared subscriptions in group

Shared subscriptions prefixed with `$share/<group-name>` are shared subscriptions with groups.
The group name can be any string.
Subscribers who belong to the same group will receive messages with load balancing,
but EMQX will broadcast messages to different groups at the same time.

For example, if subscribers `s1`, `s2`, and `s3` are members of group `g1`,
and subscribers `s4` and `s5` are members of group `g2`, and all subscribers subscribe to topic `t1`.
When EMQX Broker publishes a message `msg1` to topic `t1`:

- EMQX will send `msg1` to both groups `g1` and `g2`
- Only one of `s1`, `s2`, `s3` will receive `msg1`
- Only one of `s4` and `s5` will receive `msg1`

```txt
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

## Load Balancing Policy and Dispatch ACK Configuration

In MQTT specification, balancing strategy is not covered.
EMQX supports a few different balancing strategies with the help from configuration.

Balancing strategies can be specified globally or per-group.

- Global strategy can be set in `broker.shared_subscription_strategy` configuration.
- Configure `broker.shared_subscription_group.$group_name.strategy` for per-group strategy

```txt
# etc/emqx.conf

# Load Balance
broker.shared_subscription_strategy = random

# When the client is offline, or the message level is QoS1 or QoS2, and the device does not reply to the ACK for any reason, the message will send to the other devices in the group.
broker.shared_dispatch_ack_enabled = false
```

| Load Balance | Description                                 |
| :----------  | :------------------------------------------ |
| random       | Random selection among all subscribers      |
| round_robin  | In order of subscription                    |
| sticky       | Always send to the last selected subscriber |
| hash         | Hash by publisher ClientID                  |

### Discussion on message loss

EMQX sends messages to subscribers' sessions.

When session is persisted (clean_session=false) the subscriber can recover the data stream
right after reconnect without losing messages.

This is a bit contradicting with the 'load balancing' idea, since often when shared subscription
is in use, if a subscriber is offline, the other subscribers in the group are expected to take
over the data stream. Otherwise if the subscriber is offline for long enough the session
message buffer will eventually overflow and result in message loss.

Due to above reasons, persisted sessions are usually not common for shared subscribers,
but there is nothing stopping you from doing it.

The configuration `broker.shared_dispatch_ack_enabled` is introduced to improve
load sharing in case of persisted sessions. When set to `true`, EMQX will try to dispatch
messages to other members in the group if one is offline.

More on exceptional flows.

- Once a message is dispatched to a subscriber session, the message will stay in the session
  buffer but not re-dispatched immediately.
- The pending messages in a session is re-dispatched to other members in the group when
  a session terminates.
- When all members are offline, the message is dispatched to the configured strategy
- When there is no alive session in a shared group, the message is discarded
