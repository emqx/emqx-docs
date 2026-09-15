# Incompatible Changes in EMQX 5.7

## v5.7.0

- [#12947](https://github.com/emqx/emqx/pull/12947) For JWT authentication, a new boolean option `disconnect_after_expire` has been added with default value set to `true`. When enabled, the client will be disconnected after the JWT token expires.

  Previously, the clients with actual JWTs could connect to the broker and stay connected even after the JWT token expired. Now, the client will be disconnected after the JWT token expires. To preserve the previous behavior, set `disconnect_after_expire` to `false`.

- [#12957](https://github.com/emqx/emqx/pull/12957) Stopped building packages for macOS 12.
