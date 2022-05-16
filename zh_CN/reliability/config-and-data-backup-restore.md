# Config and data backup & restore

EMQX is highly available when clustered, and most of the data in EMQX cluster is
fully replicated to all nodes -- However, we should still plan for the worst.
Let's discuss data backup and restore for disaster recovery.

## Terminologies

[Mnesia](https://en.wikipedia.org/wiki/Mnesia): The name of the built-in database inside each EMQX node.

## Files

### Config files

When using morden provisioning tools for EMQX deployment, configuration changes
are mostly managed by the tools, and even source controled in for example a git repo.

In case manual config changes are made to the config files, it's a good idea to have them backed up.

The config files are possibly located in below places, but it differ depending on installation

* Default to `/etc/emqx` when installed from RPM or DEB packages
* Default to `/opt/emqx/etc` shen running in docker container
* In `/path/to/install/dir/etc` when directly extracted from zip package.

## Mnesia Database

There are two approaches to create a backup of the Mnesia database.

### Export 'mnesia' data

The commands introduced in [Data import and export](./data-import-and-export.md),
can be used backup the database to a JSON file using the export command,
and restore whth the import command.

When recovering a EMQX cluster from the exported file, the new EMQX cluster
should be started with at empty state, and the import command should
repopulate the database for all nodes in the cluster.

### Backup 'mnesia' dir

In EMQX Mnesia database, tables are stored in the `mnesia` sub directory in EMQX's `data` directory.
Backing up the database can be as simple as making a copy of all the files in the `mnesia` directory.

The database schema (which is also stored in `mnesia` directory) is unique per EMQX node, meaning
a backup can not be used to restore another node in the cluster.

Depending on installation and configuration, `data` dir can be located in below possible paths.

* Where the environment variable `EMQX_NODE__DATA_DIR` points to
* Where the `node.data_dir` config key points to in `emqx.conf`
* `/opt/emqx/data` when running in docker (typically a mounted volume)
* `<install-path>/data` when installed from zip package extraction
* `/var/lib/emqx/data` when installed from RPM or DEB packages

## What about Persisted Sessions ?

Prior to v5.0, EMQX nodes are rather stateless by themselves, in the sense that persistent
session states are delegated to external databases (enterprise edition feature).

So, for persisted sessions, there is nothing to backup from where the EMQX nodes are running.
