# Incompatible Changes in EMQX 5.7

{% emqxce %}

## v5.7.0


{% endemqxce %}

{% emqxee %}

## e5.7.0

- [#12947](https://github.com/emqx/emqx/pull/12947) For JWT authentication, support new `disconnect_after_expire` option. When enabled, the client will be disconnected after the JWT token expires.

  This option is enabled by default, so the default behavior is changed.
  Previously, the clients with actual JWTs could connect to the broker and stay connected
  even after the JWT token expired.
  Now, the client will be disconnected after the JWT token expires.

  To preserve the previous behavior, set `disconnect_after_expire` to `false`.
- [#12895](https://github.com/emqx/emqx/pull/12895) Complemented some necessary but missed keys for the DynamoDB connector and the action.
* The old configuration no longer works, although it actually didn't work properly until this fix.
* For DynamoDB connector, a new key `region` is necessary.
* `hash_key` and `range_key` are now supported in the DynamoDB action, and `hash_key` is required.

- [#12957](https://github.com/emqx/emqx/pull/12957) Stop building packages for macOS 12.

{% endemqxee %}
