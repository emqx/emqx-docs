# Incompatible Changes in EMQX 5.10

## 5.10.5

- [#17864](https://github.com/emqx/emqx/pull/17864) The Dashboard user and API-key endpoints now reject scope lists that mix privilege scopes (`system`, `user_management`, `api_key_management`, `sso_management`) with other scopes. Each of the four privilege scopes is administrator-equivalent in effect, so combining them with a restricted scope list cannot meaningfully restrict the account. Use either a privilege-only scope list or a non-privilege-only scope list, depending on whether the account should have administrator-equivalent capability. Pre-existing records with a mixed scope set continue to function until the next update; the next update must split the list to succeed.

- [#18465](https://github.com/emqx/emqx/pull/18465) EMQX now uses restricted SQL parsers to validate and safely render templated `INSERT` statements in ClickHouse, TDengine, and SQL Server actions, and in MySQL actions when batch insert is enabled. Existing templates that contain comments or use unsupported SQL syntax must be updated before they can be used with the new parsers.

  ClickHouse, TDengine, and SQL Server reject invalid templates when the action is created. MySQL logs a parsing error but may still create the action; batch requests using an unsupported template fail at runtime.

  Supported backend-specific syntax:

  - **MySQL**: `ON DUPLICATE KEY UPDATE`
  - **ClickHouse**: `FORMAT Values` and `FORMAT JSONCompactEachRow`
  - **TDengine**: `INSERT ... USING ... TAGS` and table identifier interpolation

  Other behavior changes:

  - The MySQL bridge now disables `ANSI_QUOTES` and `NO_BACKSLASH_ESCAPES` for all connections.
  - The ClickHouse bridge now infers the batch value separator from the SQL template and ignores the configured `batch_value_separator`.

- [#17593](https://github.com/emqx/emqx/pull/17593) Added `--force` flag to `emqx ctl relup upgrade`. By default, the upgrade now refuses to proceed if `data/patches/` contains any `*.beam` hot-patch files (which would shadow modules from the upgrade target). Pass `--force` to keep the patches and proceed anyway.

- [#18823](https://github.com/emqx/emqx/pull/18823) Fixed a misspelled field name in the `emqx ctl listeners` output.

  The command printed the listener's enabled flag as `enbale`. It now prints `enable`. Scripts that parse this output must be updated to match the corrected name.

## 5.10.4

- [#17244](https://github.com/emqx/emqx/pull/17244) Removed the hot-upgrade REST API endpoints (`/api/v5/relup/*`). Hot-upgrade is now operated exclusively through the `emqx ctl relup` CLI on each node, with no Dashboard surface.

  Place the target release tarball and its `.sha256` sidecar (same base name, same directory) anywhere readable by the EMQX process. Run `emqx ctl relup upgrade <TarballPath>` on each node to apply the upgrade; the target version is read from `releases/emqx_vars` (`REL_VSN`) inside the tarball.

## 5.10.3

- [#16491](https://github.com/emqx/emqx/pull/16491) Stop releasing packages for macOS 13 (Ventura).

## 5.10.2

- [#16062](https://github.com/emqx/emqx/pull/16062) Fixed an issue where RocketMQ actions ignored the configured payload template and sent the entire rule output instead.

  If you relied on the previous (incorrect) behavior, you may need to update your payload templates to ensure messages are formatted as expected.

## 5.10.1

- [#15752](https://github.com/emqx/emqx/pull/15752) Listener connection rate limits (`max_conn_rate` and `max_conn_burst`) are now enforced per listener rather than per acceptor, restoring the pre-5.9.0 behavior. As a result, configurations from versions 5.9.0, 5.9.1, and 5.10.0 are incompatible: specified rates must be scaled up by the number of acceptors configured for respective listeners.

## 5.10.0

- [#15289](https://github.com/emqx/emqx/pull/15289) Added a new `resource_opts.health_check_timeout` configuration to all Connectors, Actions, and Sources, with a default value of 60 seconds.  If a health check takes more than this to return a response, the Connector/Action/Source will be deemed `disconnected`.

  Note: since the default is 60 seconds, this means that if a Connector/Action/Source previously could take more than that to return a healthy response, now it'll be deemed disconnected in such situations.

- [#15286](https://github.com/emqx/emqx/pull/15286) Configuration option `broker.routing.storage_schema` is now deprecated and ignored. Legacy `v1` routing storage schema is no longer supported, and EMQX will refuse to start in a cluster running older versions that still use it. For instructions on upgrading a cluster that uses `v1` routing schema, see [Rolling Upgrade Considerations for EMQX 5.10 or Later](../get-started/deploy/rolling-upgrades.md#rolling-upgrade-considerations-for-emqx-5.10-or-later).

- [#15239](https://github.com/emqx/emqx/pull/15239) The type for the `multi_tenancy.default_max_sessions` is now either `infinity` or a positive integer.  Previously, `0` would be accepted.

- [#15156](https://github.com/emqx/emqx/pull/15156) Schema validation was added to `dashboard.sso.oidc.issuer` field.  Now, this value is checked to be a valid URL.
