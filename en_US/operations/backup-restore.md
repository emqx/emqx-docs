# Data backup and restoration

EMQX adopts a distributed storage schema to replicate data among all nodes, and a cluster transfer feature to ensure the system's high availability. 

As part of our "fail-safety" design, EMQX also provides data backup and restoration for disaster recovery. This chapter guides you on how to back up and restore EMQX data, which mainly includes the configuration files and the operating data. 

## Configuratation files 

When EMQX is deployed with a deployment tool, the configuration center, or Git, these configurations will be automatically backed up, and no manual backup is needed. 

However, for the manually configured items, it is recommended to run a periodic backup. The configuration files may locate at:

* `/etc/emqx`: when installed from RPM or DEB packages
* `/opt/emqx/etc`: when running in docker container
* `/path/to/install/dir/etc`: when directly extracted from zip package.

Besides the `etc` directory, there are also some configuration-related files in the `data` directory. For details, see [Configuration file](../configuration/configuration.md).

## Built-in database

By default, EMQX will use the built-in database for data storage, therefore you can back up the `mnesia` folder under `data`. 

<!-- TODO 功能完成后提供 -->

Based on the installation mode, the file path could be:

* `/var/lib/emqx/data`: when installed from RPM or DEB packages
* `/opt/emqx/data`: when running in docker container
* `./data`: when directly extracted from zip package.

You can also specify the `data` directory location via `node.data_dir` or the `EMQX_NODE__DATA_DIR` environment variable.

## Persisted sessions

Before EMQX 5.0, persistent sessions were stored in memory, and for the EMQX enterprise version, they are stored in an external database, so you cannot back up persistent sessions in EMQX.

## Data restoration

To restore the configuration file, you only need to place the backed-up configuration files and data in the corresponding directory before starting EMQX.
