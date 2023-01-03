# Exclusive subscription

The exclusive subscription allows mutually exclusive subscriptions to topics. Only one subscriber is allowed to subscribe to a topic at a time. Other subscribers will not be able to subscribe to the corresponding topic until the current subscriber unsubscribe the subscription.

The prefix and example of exclusive subscriptions:

| Example | Prefix | Real Topic Name |
| --------------- | ----------- | ------------ |
| $exclusive/t/1 | $exclusive/ | t/1 |

When a client **A** subscribes to `$exclusive/t/1`, other clients will fail to subscribe to `$exclusive/t/1` until **A** cancels the subscription to `$exclusive/t/1` .

**Note**: Exclusive subscriptions must be prefixed with `$exclusive/`, in the above example, other clients can still successfully subscribe via `t/1`.

## Subscription error code

| Code            | Reason        | 
| --------------- | ----------- | 
| 0x8F     | use `$exclusive/` without exclusive subscription enable  | 
| 0x97 | A client has already subscribed to this topic  |


## Configuration settings

Exclusive subscription is disabled by default and can be configured in `etc/emqx.conf`:

|               configuration              |      type       | default |                 description                 |
| ----------------------------------- | --------------- | ------ | ------------------------------------ |
| mqtt.exclusive_subscription  | boolean          | false   | default switch for exclusive subscription  |
| zone.external.exclusive_subscription | boolean | not set | switch for exclusive subscription on external zone   |
| zone.internal.exclusive_subscription | boolean | not set | switch for exclusive subscription on internal zone |

If the value of `exclusive_subscription` is not set on the zone, EMQX will use `mqtt.exclusive_subscription` to determine whether the function is enable.