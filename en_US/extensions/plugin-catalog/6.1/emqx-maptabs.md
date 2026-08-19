# Mapping Tables

The EMQX Mapping Tables plugin provides named mapping tables for Rule SQL. Use this plugin when a rule needs to map stable identifiers, codes, or binary field IDs to structured values without maintaining long `CASE WHEN` expressions in the SQL statement.

This plugin is available starting from EMQX v6.1.5. Install and start the plugin before using `maptab_lookup` in Rule SQL.

Mapping tables are loaded from JSON files through the `emqx ctl maptabs` CLI and stored in EMQX's replicated database. Rules query the tables with the `maptab_lookup` SQL function. For function signatures and SQL examples, see [Built-in SQL Functions](../../../data-integration/rule-sql-builtin-functions.md).

## Table Files

A mapping table is a JSON file. The file name without the `.json` extension is used as the table name. Table names can contain only letters, digits, underscores, and hyphens.

The JSON file must contain an array of row objects. Each row must include a `key` field. All other fields in the row form the row's value map.

Example:

```json
[
  {
    "key": 1,
    "signal_name": "temperature_c",
    "start_bit": 17,
    "length": 8,
    "type": "integer",
    "signedness": "signed",
    "endian": "big"
  },
  {
    "key": 2,
    "signal_name": "pressure_kpa",
    "start_bit": 17,
    "length": 32,
    "type": "float",
    "signedness": "unsigned",
    "endian": "big"
  }
]
```

A `key` must be a JSON integer or string. Native JSON types are preserved, so the integer `50` and the string `"50"` are different keys.

Loading is fail-closed. EMQX rejects the whole file and keeps the previous table version when the file contains any of the following issues:

- invalid JSON
- a top-level value that is not an array
- a row that is not an object
- a row without `key`
- duplicate keys
- a key whose type is float, boolean, null, array, or object

## CLI Commands

Use the `emqx ctl maptabs` CLI to manage mapping tables.

| Command | Description |
| --- | --- |
| `emqx ctl maptabs list` | List tables cached on the local node, including row count and version. |
| `emqx ctl maptabs status` | List tables on every running node. Use this command to detect cache drift. |
| `emqx ctl maptabs load <file>` | Validate a table JSON file and replicate it to all nodes. |
| `emqx ctl maptabs reload` | Rebuild caches from storage on all running nodes. Use this command when caches need to be synchronized again. |
| `emqx ctl maptabs get <name>` | Print the stored JSON content of a table. |
| `emqx ctl maptabs delete <name>` | Delete a table from all nodes. |

All command output is JSON, except that `emqx ctl maptabs get <name>` prints the stored table JSON content directly when the table exists.

## Configuration

Configure the plugin through the standard plugin configuration API `PUT /api/v5/plugins/<name-vsn>/config` or the plugin configuration file.

| Configuration | Default | Description |
| --- | --- | --- |
| `max_tables` | `100` | Maximum number of mapping tables. Loading a new table beyond this limit is rejected. Replacing an existing table is allowed. |
| `max_rows_per_table` | `10000` | Maximum number of rows in one table. A file with more rows is rejected. |
| `max_table_file_bytes` | `10000000` | Maximum size, in bytes, of a table JSON file. Larger files are rejected before being read into memory and replicated. |

Limits are checked when a table is loaded. Changing a limit does not delete or truncate already loaded tables.

## Cluster Behavior

The plugin stores table content in EMQX's built-in replicated database. Loading or deleting a table is replicated to every node in the cluster, and each node rebuilds its in-memory cache from the stored table content.

Install and start the plugin on all nodes in the cluster. A node that was down during a table load or delete catches up when it restarts and rebuilds its cache from storage.

Cache updates are atomic for readers. A rule lookup sees either the old table version or the new table version, not a partial update.

## Access and Sharing

Mapping tables are managed only by administrators through the CLI. The tables are shared across tenant namespaces. A lookup returns the same rows for every client, whether or not the client belongs to a multi-tenancy namespace.

If rows must differ by tenant, encode the tenant in the table data. For example, include the tenant in the lookup key:

```sql
maptab_lookup('signals', concat(client_attrs.tns, ':', item_id))
```

You can also use one table per tenant and compose the table name in the rule. Apply the same convention to every key in the table and every lookup site.
