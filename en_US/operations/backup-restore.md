# Backup and Restore

EMQX adopts a distributed storage schema and also introduces a cluster transfer feature to ensure the system's high availability.

This chapter will discuss how to backup your operating data and configuration files to prevent data losses in case of system malfunctions.

## Configuration Files

When EMQX is deployed with a deployment tool, the configuration center, or Git, these configurations will be automatically backed up, and no manual backup is needed.

However, for the manually configured items, it is recommended to run a periodic backup. The configuration files may locate at:

* `/etc/emqx`: when installed from RPM or DEB packages
* `/opt/emqx/etc`: when running in docker container
* `/path/to/install/dir/etc`: when directly extracted from zip package.

Besides the `etc` directory, there are also some configuration-related files in the `data` directory. For details, see [Configuration file](../configuration/configuration.md).

## Built-in Database

By default, EMQX will use the built-in database for data storage, therefore you can back up the `mnesia` folder under `data`.

<!-- TODO 功能完成后提供 -->

Based on the installation mode, the file path could be:

* `/var/lib/emqx/data`: when installed from RPM or DEB packages
* `/opt/emqx/data`: when running in docker container
* `./data`: when directly extracted from zip package.

You can also specify the `data` directory location via `node.data_dir` or the `EMQX_NODE__DATA_DIR` environment variable.

## Persisted Sessions

Before EMQX 5.0, persistent sessions were stored in memory, and for the EMQX enterprise version, they are stored in an external database, so you cannot back up persistent sessions in EMQX.

## Data Restore

To restore the configuration file, you only need to place the backed-up configuration files and data in the corresponding directory before starting EMQX.

## Data Import and Export

EMQX 5.1 introduces a user-friendly tool for data import and export. While similar to the one available in EMQX 4.x, it has several significant differences and is not compatible with it.

In EMQX 4.x, a single JSON file was used to carry all the necessary data from both the EMQX configuration and the built-in database. However, in EMQX 5.1, an opaque compressed tar archive is used. This allows for more efficient and structured handling of potentially large amounts of user data.

The data supported for import and export in EMQX 5.1 is analogous to that of EMQX 4.x. It includes:

* Contents of EMQX [configuration rewrite file](../configuration/configuration.md#configuration-rewrite-file):
    * Rules and bridges
    * Authentication and authorization configuration
    * Listeners, gateways configuration
    * Other EMQX settings
* Built-in database (Mnesia) data:
    * Dashboard users and API keys
    * Authentication user credentials (both simple and enhanced)
    * PSK authentication data
    * Authorization rules
    * Banned clients
* SSL/TLS certificates stored in EMQX data directory (`node.data_dir`)
* Authorization acl.conf file stored in EMQX data directory

::: tip Warning

Any SSL/TLS certificate or acl.conf files stored outside the EMQX data directory are not included in the exported archive. Therefore, when exporting and importing data, it's important to note that these files should be provisioned on all EMQX target nodes before importing the data. This ensures that the necessary SSL/TLS certificates and acl.conf files are available for proper functionality.

:::

### Export

Data can be exported from any running cluster node.

### Import

To import data, the EMQX node must be running, and certain conditions must be met for a successful import operation:

* If [Core + Replicant](../deploy/cluster/mria-introduction.md#enable-core-replicant-mode) mode is enabled, the data import can only be initiated on core nodes. This does not affect the actual import behavior as the data will be copied to all cluster nodes, both core and replicant. Initiating the import on a core node ensures proper coordination of the operation.
* Data exported from EMQX Enterprise Edition cluster cannot be imported to EMQX Open Source Edition cluster.
* The data file must not be renamed.

If any of the above conditions are not satisfied, the import process will be aborted, and an appropriate error message will be displayed.

During the data import operation, the data will either be inserted (if it was not present in the target EMQX cluster) or updated (in case of a conflict). The import process does not remove any existing data from the EMQX cluster.

::: tip Warning
In rare circumstances, the existing data may not be compatible with the imported data. For example, if the EMQX cluster uses the built-in database authentication mechanism with a 'suffix' salt position, and the imported data defines the same authentication source with a 'prefix' salt position, the old user credentials created before the import will no longer be operational. Therefore, importing data to a non-clean EMQX cluster may require extra caution.
:::

### Example

This section shows how to import and export data using the command Line Interface.

1. Export data. The file name format of the exported file is `emqx-export-YYYY-MM-DD-HH-mm-ss.sss.tar.gz`, and export directory is `<EMQX data directory>/backup`:

    ```bash
    $ ./emqx_ctl data export
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
2. Import data. The name of the imported file should be specified as an absolute path, e.g.:

    ```bash
    $ ./emqx_ctl data import /tmp/emqx-export-2023-06-19-15-14-19.947.tar.gz
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
    ```
