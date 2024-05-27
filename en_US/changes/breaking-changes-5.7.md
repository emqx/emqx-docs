# Incompatible Changes in EMQX 5.7

{% emqxce %}

## v5.7.0

- [#12947](https://github.com/emqx/emqx/pull/12947) For JWT authentication, a new boolean option `disconnect_after_expire` has been added with default value set to `true`. When enabled, the client will be disconnected after the JWT token expires.

  Previously, the clients with actual JWTs could connect to the broker and stay connected even after the JWT token expired. Now, the client will be disconnected after the JWT token expires. To preserve the previous behavior, set `disconnect_after_expire` to `false`.
  
- [#12957](https://github.com/emqx/emqx/pull/12957) Stopped building packages for macOS 12.

{% endemqxce %}

{% emqxee %}

## e5.7.0

- [#12947](https://github.com/emqx/emqx/pull/12947) For JWT authentication, a new boolean option `disconnect_after_expire` has been added with default value set to `true`. When enabled, the client will be disconnected after the JWT token expires.

  Previously, the clients with actual JWTs could connect to the broker and stay connected even after the JWT token expired. Now, the client will be disconnected after the JWT token expires. To preserve the previous behavior, set `disconnect_after_expire` to `false`.
  
- [#12957](https://github.com/emqx/emqx/pull/12957) Stopped building packages for macOS 12.

- [#12895](https://github.com/emqx/emqx/pull/12895) Complemented some necessary but missed keys for the DynamoDB connector and the action. The old configuration is obsolete, as it didn't function properly before this fix. Specifically, for the DynamoDB connector, the addition of a new key, `region`, is required. Additionally, `hash_key` and `range_key` are now supported in the DynamoDB action, with `hash_key` being mandatory.

{% endemqxee %}
