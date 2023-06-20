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

# Data import and export

EMQX 5.1 introduced user-friendly data import and export tool. It is similar to one available in EMQX 4.x but has several significant differences and is not compatible with it.
Unlike EMQX 4.x that used a single JSON file to carry all the necessary data from both EMQX configuration and built-in database, EMQX 5.1 uses an opaque compressed tar archive, which allows to handle potentially large amount of user data in a more efficient and structured way.
The data currently supported for import and export in EMQX 5.1 is analogous to EMQX 4.x. It includes:

* the content of EMQX [configuration rewrite file](../configuratation/configuration.md#configuration-rewrite-file), including:
    * rules and bridges
    * authentication and authorization configuration
    * listeners, gateways configuration
    * other EMQX settings
* built-in database (Mnesia) data, including:
    * Dashboard users and API keys
    * Authentication user credentials (both simple and enhanced)
    * PSK authentication data
    * Authorization rules
    * banned clients
* SSL/TLS certificates stored in EMQX data directory (`node.data_dir`)
* Authorization acl.conf file stored in EMQX data directory

::: warning
Any SSL/TLS certificate or acl.conf files stored outside EMQX data directory are not included in the exported archive.
It is possible to export and import data successfully in such case, but these files should be provisioned on all EMQX target nodes before
importing the data.
:::

## Export

Data can be exported from any running cluster node.

## Import

Data can be imported only on a running EMQX node. Several conditions must be met for the import operation to succeed:

* if [Core + Replicant](../deploy/cluster/mria-introduction.md#enable-core-replicant-mode) mode is used, the data import
  can only be initiated on core nodes. This doesn't affect actual data import behavior in any way: the data will be copied
  to all cluster nodes (both core and replicant), it just needs to be triggered on a core node to properly coordinate the operation.
* Data exported from EMQX Enterprise Edition cluster cannot be imported to EMQX Community Edition cluster.
* A data file must not be renamed.

If any of the above conditions is not satisfied, the import will be aborted with an appropriate error message.

Data import operation either inserts the data (if it was not present on the target EMQX cluster before) or updates existing data (when there is a conflict). No previous data is removed on EMQX cluster during the import.

::: warning
In rare circumstances, the existing data may be not compatible with the imported data.
For example, if EMQX cluster uses built-in database authentication mechanism with 'suffix' salt position, and the imported data defines the same
authentication source with 'prefix' salt position, all the old user credentials created before the import will be not operational any more,
since they are not compatible with the newly imported configuration.
Therefore, importing data to a non-clean EMQX cluster may require extra caution.
:::

## Example

### Command line interface

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
