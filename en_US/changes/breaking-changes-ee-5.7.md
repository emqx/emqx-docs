# Incompatible Changes in EMQX 5.7


## e5.7.2

- [#13327](https://github.com/emqx/emqx/pull/13327) The directory path scheme for on-disk Kafka/Confluent/Azure Event Hub buffers has changed.  It now uses the Action name instead of the topic name.

  Upgrading to this version will invalidate (not use) old buffer files, and will require manual cleanup of the old directories.

- [#13332](https://github.com/emqx/emqx/pull/13332) When an S3 Bridge is improperly configured, error messages now contain more informative and easy to read details.

  * S3 Bridge configuration with invalid aggregated upload key template will no longer work. Before this change, such configuration was considered valid but the bridge would never work anyway.

- [#13420](https://github.com/emqx/emqx/pull/13420) Added a schema validation that prevents configuring an empty set of topic filters for a Schema Validation.  Any such configurations will have to define at least one topic filter to be valid.  Such configurations, though, are probably very rare, as a Schema Validation with empty topics is essentially the same as having no validation at all.

## e5.7.0

- [#12947](https://github.com/emqx/emqx/pull/12947) For JWT authentication, a new boolean option `disconnect_after_expire` has been added with default value set to `true`. When enabled, the client will be disconnected after the JWT token expires.

  Previously, the clients with actual JWTs could connect to the broker and stay connected even after the JWT token expired. Now, the client will be disconnected after the JWT token expires. To preserve the previous behavior, set `disconnect_after_expire` to `false`.
  
- [#12957](https://github.com/emqx/emqx/pull/12957) Stopped building packages for macOS 12.

- [#12895](https://github.com/emqx/emqx/pull/12895) Complemented some necessary but missed keys for the DynamoDB connector and the action. The old configuration is obsolete, as it didn't function properly before this fix. Specifically, for the DynamoDB connector, the addition of a new key, `region`, is required. Additionally, `hash_key` and `range_key` are now supported in the DynamoDB action, with `hash_key` being mandatory.
