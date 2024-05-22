# Incompatible Changes in EMQX 5.7

{% emqxce %}

## v5.7.0


{% endemqxce %}

{% emqxee %}

## e5.7.0

- [#12895](https://github.com/emqx/emqx/pull/12895) Complemented some necessary but missed keys for the DynamoDB connector and the action.
* The old configuration no longer works, although it actually didn't work properly until this fix.
* For DynamoDB connector, a new key `region` is necessary.
* `hash_key` and `range_key` are now supported in the DynamoDB action, and `hash_key` is required.

- [#12957](https://github.com/emqx/emqx/pull/12957) Stop building packages for macOS 12.

{% endemqxee %}
