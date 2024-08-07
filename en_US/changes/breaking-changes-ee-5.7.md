# Incompatible Changes in EMQX 5.7


## e5.7.2

- [#13327](https://github.com/emqx/emqx/pull/13327) The fix for issues in Kafka, Confluent, and Azure Event Hubs integrations has resulted in a change to the directory path scheme for on-disk buffers of these integrations. The new scheme uses the action name instead of the topic name. Upgrading to this version will invalidate old buffer files, and require manual cleanup of the old directories.
- [#13332](https://github.com/emqx/emqx/pull/13332) Error messages for misconfigured Amazon S3 integrations are now more descriptive and user-friendly. The Amazon S3 Sink configurations with an invalid object key templates for aggregated upload will no longer work. Before this change, such configurations were considered valid but but resulted in non-functional integrations.
- [#13420](https://github.com/emqx/emqx/pull/13420) Added a configuration validation that prevents configuring an empty set of topic filters for a Schema Validation.  Any such configurations have to define at least one topic filter to be valid.  Such configurations, though, are probably very rare, as a Schema Validation with empty topics is essentially the same as having no validation at all.

## e5.7.0

- [#12947](https://github.com/emqx/emqx/pull/12947) For JWT authentication, a new boolean option `disconnect_after_expire` has been added with default value set to `true`. When enabled, the client will be disconnected after the JWT token expires.

  Previously, the clients with actual JWTs could connect to the broker and stay connected even after the JWT token expired. Now, the client will be disconnected after the JWT token expires. To preserve the previous behavior, set `disconnect_after_expire` to `false`.
  
- [#12957](https://github.com/emqx/emqx/pull/12957) Stopped building packages for macOS 12.

- [#12895](https://github.com/emqx/emqx/pull/12895) Complemented some necessary but missed keys for the DynamoDB connector and the action. The old configuration is obsolete, as it didn't function properly before this fix. Specifically, for the DynamoDB connector, the addition of a new key, `region`, is required. Additionally, `hash_key` and `range_key` are now supported in the DynamoDB action, with `hash_key` being mandatory.
