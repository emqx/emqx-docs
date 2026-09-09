# EMQX Backup Sync

The EMQX Backup Sync plugin periodically synchronizes selected configuration and built-in database data from a primary EMQX cluster to a secondary cluster. It is available in EMQX Enterprise 5.10.5 and later 5.10 releases.

Install and start the plugin only on the secondary cluster. The primary cluster does not need the plugin, but its Dashboard Data Backup APIs must be reachable from the secondary cluster.

## How Synchronization Works

After the plugin starts with a valid configuration, it runs an initial synchronization asynchronously and repeats it at the configured interval. In a secondary cluster, only one running core node performs synchronization. If that node becomes unavailable, another core node can take over on a subsequent interval.

Each synchronization performs the following operations:

1. Requests the primary cluster to export the selected configuration roots and Mnesia table sets.
2. Downloads the backup from the primary cluster and uploads it to the secondary cluster.
3. Imports the backup on the secondary cluster.
4. Removes or retains the backup files according to the cleanup settings.

Configuration roots and Mnesia table sets use different import behaviors:

- **Configuration roots**: Use the standard EMQX configuration import behavior. Values from the primary cluster are inserted or updated, while configuration that exists only on the secondary cluster is not deleted.
- **Mnesia table sets**: Use snapshot restore behavior. The corresponding tables on the secondary cluster are replaced by the primary cluster's snapshot.

::: warning Important Notice

Snapshot restore deletes records that exist in a selected table set only on the secondary cluster. Review `sync.table_sets` carefully before starting the plugin.

:::

Synchronization runs only when the plugin is started and its configuration is valid. Applying a valid configuration triggers an immediate synchronization if no task is running. If a task is running, the plugin requests cancellation and uses the updated configuration for later attempts. Because cancellation is checked only between stages, an import already in progress may still finish.

## Configure the Plugin

1. On the primary cluster, [create an API key](../../api.md#create-api-keys) with the `administrator` role. If API scopes are configured, include the `system` scope, which grants access to the `/data/*` endpoints used by this plugin.

2. Ensure that the primary cluster's Dashboard API is reachable from the secondary cluster. Run both clusters on EMQX 5.10, because a secondary cluster cannot import backups created by a later major or minor version.

3. On the secondary cluster, install `emqx_backup_sync` as described in [Manage Plugins](../plugin-management.md).

4. Configure the plugin. The following example enables TLS certificate verification:

   ```hocon
   primary {
     base_url = "https://primary.example.com:18083/api/v5"
     api_key = "sync-key"
     api_secret = "sync-secret"
     ssl {
       enable = true
       server_name_indication = "primary.example.com"
       verify = "verify_peer"
       cacertfile = "/etc/emqx/certs/primary-ca.pem"
       certfile = ""
       keyfile = ""
     }
   }

   sync {
     interval = "5m"
     root_keys = [
       "connectors",
       "actions",
       "sources",
       "rule_engine",
       "listeners",
       "schema_registry"
     ]
     table_sets = [
       "banned",
       "builtin_authn",
       "builtin_authz"
     ]
     timeout = "30s"
     retain_remote_backup = false
     retain_backup_after_import = true
   }
   ```

   `primary.api_key` and `primary.api_secret` can contain credentials directly or use `file://` paths, for example, `file:///etc/emqx/backup-sync-api-key`. When a credential file ends with line breaks, the plugin removes the trailing line breaks before use.

   Because any running core node can become the selected synchronization node, credential files and the files configured by `primary.ssl.cacertfile`, `primary.ssl.certfile`, and `primary.ssl.keyfile` must exist at the same paths and be readable by EMQX on every core node in the secondary cluster.

5. Start the plugin on the secondary cluster.

### Configuration Options

| Option | Default | Description |
| --- | --- | --- |
| `primary.base_url` | None | Dashboard API base URL of the primary cluster, including `/api/v5`. |
| `primary.api_key` | None | API key used to access the primary cluster. A direct value or a `file://` path is accepted. |
| `primary.api_secret` | None | API secret used to access the primary cluster. A direct value or a `file://` path is accepted. |
| `primary.ssl.enable` | `false` | Enables TLS options for HTTPS requests to the primary cluster. |
| `primary.ssl.server_name_indication` | `disable` | Server Name Indication (SNI) sent during the TLS handshake. |
| `primary.ssl.verify` | `verify_none` | TLS certificate verification mode. Supported values are `verify_none` and `verify_peer`. For production deployments, use `verify_peer` and configure `primary.ssl.cacertfile`. |
| `primary.ssl.cacertfile` | None | Path to the CA certificate file used to verify the primary server. |
| `primary.ssl.certfile` | None | Path to the client certificate file for mutual TLS. |
| `primary.ssl.keyfile` | None | Path to the client private key file for mutual TLS. |
| `sync.interval` | `5m` | Time between synchronization attempts. |
| `sync.root_keys` | See [Configuration Scope](#configuration-scope) | Configuration roots exported from the primary cluster. |
| `sync.table_sets` | See [Configuration Scope](#configuration-scope) | Mnesia table sets exported from the primary cluster and restored as snapshots on the secondary cluster. |
| `sync.timeout` | `30s` | Timeout for each HTTP request to the primary cluster. |
| `sync.retain_remote_backup` | `false` | Whether to retain each exported backup on the primary cluster. By default, the plugin deletes it during cleanup. |
| `sync.retain_backup_after_import` | `true` | Whether to retain each uploaded backup on the secondary cluster after an import attempt. |

The HTTP client does not automatically follow redirects. Configure `primary.base_url` with the final Dashboard API address.

Cleanup runs after every successful export, even if a later stage fails or the synchronization is canceled. By default, the exported backup is deleted from the primary cluster. Set `sync.retain_remote_backup = true` to preserve it for troubleshooting.

## Configuration Scope

The default `sync.root_keys` values are:

- `connectors`
- `actions`
- `sources`
- `rule_engine`
- `listeners`
- `schema_registry`

The other supported values are `authentication` and `authorization`.

Rules commonly depend on connectors, actions, sources, and Schema Registry objects. If you synchronize `rule_engine`, include its dependent roots unless equivalent objects already exist on the secondary cluster. Otherwise, imports can fail or the imported rules might not work as expected.

By default, `sync.table_sets` contains `banned`, `builtin_authn`, and `builtin_authz`. You can also select `builtin_retainer`, `psk`, and `mt`. Set `sync.table_sets = []` to synchronize configuration only.

The plugin authenticates to the Data Backup API with an API key. Therefore, it cannot synchronize the `dashboard_users` or `api_keys` table sets. The Data Backup API omits these sensitive table sets from exports requested with an API key.

## Check Synchronization Status

Run the following command on any node in the secondary cluster:

```bash
emqx ctl backup_sync status
```

The command queries the selected core node and displays the overall status, health, whether synchronization is enabled, synchronization task state, selected core node, time until the next synchronization, primary API base URL, interval, root keys, and table sets. API credentials are not displayed.

If an import succeeds but backup cleanup fails, the synchronization is reported as failed. Use the health output and EMQX logs to identify whether the failure occurred during export, download, upload, import, cleanup, or worker cancellation.
