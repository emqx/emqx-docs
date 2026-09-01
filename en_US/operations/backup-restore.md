# Backup and Restore

EMQX adopts a distributed storage schema and also introduces a cluster transfer feature to ensure the system's high availability.

This page discusses how to back up your operating data and configuration files to prevent data loss in case of system malfunctions.

## Function Description

EMQX provides CLI commands for data import and export to implement backup and recovery. Although similar to the commands in EMQX 4.x, the export file format is incompatible with 4.x:

- In EMQX 4.x, a single JSON file was used to save all necessary data of EMQX configuration and the built-in database.
- In EMQX 5.x, the exported data is compressed into a tar file format, allowing for more efficient and structured handling of potentially large amounts of user data.

In addition to CLI commands, EMQX Enterprise provides a data backup and recovery page in the EMQX Dashboard, where you can import and export data.

The data that EMQX supports for import and export includes:

- Contents of EMQX [configuration rewrite file](../configuration/configuration.md#configuration-rewrite-file):
  - Authentication and authorization configuration
  - Rules, connectors, and Sink/Source
  - Listeners, gateway configuration
  - Other EMQX configurations
- Built-in database (Mnesia) data
  - Dashboard users and REST API keys
  - Client authentication credentials (built-in database password authentication, enhanced authentication)
  - PSK authentication data
  - Authorization rules
  - Blacklist data
  - Retained messages
- SSL/TLS certificates stored in the EMQX data directory (`node.data_dir`)
- Authorization `acl.conf` file stored in the EMQX data directory

::: warning Important Notice

- Built-in database authentication credentials and authorization rules associated with Namespaces cannot be exported or imported for an individual Namespace. To back up or restore these records, use a global backup. A global backup processes the records for all Namespaces together.
- A backup includes only the SSL/TLS certificates and `acl.conf` file stored in the EMQX data directory. Before you import the backup, separately copy any certificates or `acl.conf` files stored outside the data directory to the appropriate locations.

:::

::: tip Backup File Details

- The exported file name format is `emqx-export-YYYY-MM-DD-HH-mm-ss.sss.tar.gz`, and the export directory is `<EMQX data directory>/backup`.
- Starting from EMQX v5.7.1, retained messages are backed up even when their storage method is configured as `ram` (memory).

:::

### Export

Data can be exported from any running cluster node.

Starting from EMQX 6.3.0, the exported backup records the node's security profile in `META.hocon`. EMQX uses this metadata to check profile compatibility when the backup is imported.

### Import

To import data, the EMQX node must be running, and some conditions need to be met for the import operation to be successful:

- If the [core node + replica node](../deploy/cluster/mria-introduction.md) mode is enabled, data import can only be performed on the core node. This will not affect the actual import behavior, as data will be replicated to all cluster nodes, including core and replica nodes. Operating on the core node ensures correct data import.
- The data file cannot be renamed.

If any of the above conditions are not met, the import process will be aborted, and a corresponding error message will be displayed.

During the data import operation, data will be inserted (if it does not exist in the target EMQX cluster) or updated (if there are conflicts) into EMQX. The import process will not delete any existing data from the EMQX cluster.

::: tip Special Note

In rare cases, existing data may be incompatible with the imported data. For example, an EMQX cluster uses built-in database authentication and sets the salt position to "suffix," while the imported data sets the same configuration to "prefix." After the import, the new configuration will take effect, and previously created old user credentials will no longer work.

Therefore, importing data into an EMQX cluster without clearing data may require extra caution.

:::

#### Security Profile Compatibility

A [security profile](../access-control/security-profile.md) selects a set of security-related default behaviors for a node. EMQX provides the `legacy` and `hardened` profiles. Starting from EMQX 6.3.0, EMQX checks the backup's recorded security profile during import. Uploading a backup file does not run this check or restore any data.

| Backup Security Profile | Target Node Security Profile | Default Import Result |
| --- | --- | --- |
| `hardened` | `hardened` | Allowed |
| `legacy` | `hardened` | Rejected unless you explicitly allow the mismatch |
| No security profile metadata, such as a backup created before EMQX 6.3.0 | `hardened` | Treated as `legacy` and rejected unless you explicitly allow the mismatch |
| Any profile | `legacy` | Allowed |

This table describes only the security profile check. Allowing a mismatch bypasses only this check. All other backup compatibility checks still apply.

::: warning Important Notice
Restoring data captured under `legacy` to a `hardened` node can change how the restored deployment behaves:

- On a target node where `node.default_listener_address` is not set, MQTT and Dashboard HTTP listeners configured with a port but no address resolve to loopback instead of all network interfaces.
- An empty or fully disabled authenticator chain denies all clients instead of allowing them.
- A restored Dashboard account that still uses the default password cannot log in.
- Authentication and authorization backend failures that `legacy` ignores cause the operation to be denied.

Review these differences before allowing a security profile mismatch.
:::

## Manage Backup Files in Dashboard

Global administrators can manage backup files in **Global** or a specific [Namespace](../multi-tenancy/namespace-overview.md). Namespaced administrators can manage and download backup files from their assigned Namespace, but cannot access backup files in **Global** or another Namespace.

:::tip

- Backup and recovery through the Dashboard are available in EMQX Enterprise edition v5.4.0 and later versions.
- Backup files exported via CLI can also be managed on the Backup and Recovery page of the Dashboard.

:::

1. Log in to the Dashboard and go to **System** -> **Backup & Restore**.

2. If you are a global administrator, select **Global** or a specific Namespace from the Namespace selector. The page loads the backup file list for the selected scope. When you select a Namespace, verify that the notice above the list identifies the target Namespace.

   Namespaced administrators do not see the selector. EMQX restricts their backup operations to their assigned Namespace.

3. To export data, click **Create**. Global administrators can create a backup only in the **Global** view. When a global administrator selects a specific Namespace, **Create** is disabled. Namespaced administrators can create a backup for their assigned Namespace.

   The backup file list displays the following information:

   - **File Name**: The name of the backup file.
   - **Node Name**: This name refers to the node where the backup file is stored, and it does not mean that the backup only contains data from that node.
   - **Created At**: The creation time of the backup file.
   - **File Size**: The size of the backup file.

4. To add a backup file to the selected scope, click **Upload**. Uploading a file does not restore its data. For a specific Namespace, the success message identifies the target Namespace. After the upload succeeds, verify that the file appears in the backup file list.

5. Manage a backup file by clicking one of the following buttons in the **Actions** column:

   - **Download**: Download the backup file to your local device.
   - **Delete**: Delete the backup file from the selected scope.
   - **Restore**: Import the backup file into the selected scope. If you selected a specific Namespace, verify the target Namespace in the confirmation dialog before you confirm the restore. The **Allow Security Profile Mismatch** checkbox is cleared by default. Select it only after reviewing and accepting the risks described in [Security Profile Compatibility](#security-profile-compatibility). After the restore succeeds, verify that the success message identifies the target Namespace.

In a specific Namespace view, upload, download, delete, and restore operations apply to that Namespace. A global administrator can manage and restore backup files in this view but cannot create a backup.

### Manage Backup Files Through REST API

A global administrator can pass the optional `namespace` query parameter to the following endpoints:

- `GET /api/v5/data/files`: List backup files.
- `POST /api/v5/data/files`: Upload a backup file.
- `GET /api/v5/data/files/{filename}`: Download a backup file.
- `DELETE /api/v5/data/files/{filename}`: Delete a backup file.
- `POST /api/v5/data/import`: Import a backup file.

If the global administrator omits `namespace`, the operation applies to backup files in **Global**. For a namespaced caller, EMQX ignores the parameter and applies the operation to the caller's assigned Namespace.

For `POST /api/v5/data/import`, the optional `allow_security_profile_mismatch` request-body field defaults to `false`. Set it to `true` only to import a backup exported under the `legacy` profile or a backup without security profile metadata into a `hardened` node after accepting the compatibility risks. For example:

```json
{
  "filename": "emqx-export-2026-09-01-08-30-00.000.tar.gz",
  "allow_security_profile_mismatch": true
}
```

## CLI Example

This section shows how to import and export data using the command-line interface.

1. Export data. The file name format of the exported file is `emqx-export-YYYY-MM-DD-HH-mm-ss.sss.tar.gz`, and the export directory is `<EMQX data directory>/backup`:

    ```bash
    $ ./emqx ctl data export
    Exporting data to "data/backup/emqx-export-2023-06-19-15-14-19.947.tar.gz"...
    Exporting cluster configuration...
    Exporting additional files from EMQX data_dir: "data"...
    Exporting built-in database...
    Exporting emqx_admin database table...
    Exporting emqx_authn_mnesia database table...
    Exporting emqx_enhanced_authn_scram_mnesia database table...
    Exporting emqx_app database table...
    Exporting emqx_acl database table...
    Exporting emqx_psk database table...
    Exporting emqx_banned database table...
    Data has been successfully exported to data/backup/emqx-export-2023-06-19-15-14-19.947.tar.gz.
    ```
2. Import data. The name of the imported file can be specified as an absolute path or a relative path.
   If the file resides in `<EMQX data directory>/backup` directory, its basename without a path can also be used, e.g.:

    ```bash
    # import the file by the absolute path
    $ ./emqx ctl data import /tmp/emqx-export-2023-06-19-15-14-19.947.tar.gz
    Importing data from "/tmp/emqx-export-2023-06-19-15-14-19.947.tar.gz"...
    Importing cluster configuration...
    Importing built-in database...
    Importing emqx_banned database table...
    Importing emqx_psk database table...
    Importing emqx_acl database table...
    Importing emqx_app database table...
    Importing emqx_enhanced_authn_scram_mnesia database table...
    Importing emqx_authn_mnesia database table...
    Importing emqx_admin database table...
    Data has been imported successfully.
   
    # import the file by the path relative to EMQX root directory:
    $ ./emqx ctl data import ../../../tmp/emqx-export-2023-06-21-13-28-06.418.tar.gz
    Importing data from "../../../tmp/emqx-export-2023-06-21-13-28-06.418.tar.gz"...
    Importing cluster configuration...
    Importing built-in database...
    Importing emqx_enhanced_authn_scram_mnesia database table...
    Importing emqx_authn_mnesia database table...
    Importing emqx_admin database table...
    Importing emqx_acl database table...
    Importing emqx_banned database table...
    Importing emqx_psk database table...
    Importing emqx_app database table...
    Data has been imported successfully.
   
    # import the file from `<EMQX data directory>/backup` directory:
    $ cp /tmp/emqx-export-2023-06-21-13-28-06.418.tar.gz /opt/emqx/data/backup/
    $ ./emqx ctl data import emqx-export-2023-06-21-13-28-06.418.tar.gz
    Importing data from "data/backup/emqx-export-2023-06-21-13-28-06.418.tar.gz"...
    Importing cluster configuration...
    Importing built-in database...
    Importing emqx_enhanced_authn_scram_mnesia database table...
    Importing emqx_authn_mnesia database table...
    Importing emqx_admin database table...
    Importing emqx_acl database table...
    Importing emqx_banned database table...
    Importing emqx_psk database table...
    Importing emqx_app database table...
    Data has been imported successfully.
    ```

   To import a backup exported under the `legacy` profile or a backup without security profile metadata into a `hardened` node after reviewing the compatibility risks, append `--allow-security-profile-mismatch`:

   ```bash
   ./emqx ctl data import emqx-export-2026-09-01-08-30-00.000.tar.gz --allow-security-profile-mismatch
   ```
