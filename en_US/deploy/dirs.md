# Directory Structures

Depending on the type of installation the location of the directories needed by EMQX may vary.

| Description         | tar.gz       | RPM/DEB                  |
| --------------------| ------------ | ------------------------ |
| Config files        | `./etc`      | `/etc/emqx/etc`          |
| Database and files  | `./data`     | `/var/lib/emqx/data`     |
| Log files           | `./log`      | `/var/log/emqx`          |
| Boot instructions   | `./releases` | `/usr/lib/emqx/releases` |
| Executables         | `./bin`      | `/usr/lib/emqx/bin`      |
| Erlang code         | `./lib`      | `/usr/lib/emqx/lib`      |
| Erlang runtime      | `./erts-*`   | `/usr/lib/emqx/erts-*`   |
| Plugins             | `./plugins`  | `/usr/lib/emqx/plugins`  |

The directories are all created at installation time.

Except for `etc`, `data`, `log`, `plugins`, all other directories are static and rarely have to
be changed before an upgrade.

:::: tip Tip
It is recommended that the `data` directory is mounted on a high performance hard drive.
::::

## etc: config files

EMQX only reads the files in this directory.

* `emqx.conf`: the bootstrapping config file for EMQX
* `vm.args`: The boot arguments for Erlang virtual machine
* `certs/`: X.509 keys are certificate files for EMQX SSL listeners or SSL clients when
  integrating with external systems. e.g. HTTPS client for webhooks

## data: database and files

This directory is for EMQX to persist its state.

:::: tip Tip
In EMQX documents this directory is often referred to as `data_dir`.
::::

A list of the sub-directories

* `authz`: File authorization rules uploaded from HTTP API or dashboard.
* `certs`: Certificate files uploaded from HTTP API or dashboard.
* `configs`: Generated config file at boot, or config overrides when changed from API or CLI.
* `mnesia`: The built-in database. Inside this directory there should be one and only one sub-directory named
   after the node, e.g. `emqx@127.0.0.1`. In case the node is renamed the old directory should be deleted
   or moved to elsewhere.
* `patches`: Put `.beam` files here for EMQX to load as a hot-patch. Handy to remedy urgent issues.
* `trace`: Online tracing log files.

In the case that an EMQX node is intended to be static and long-living
it is recommended that all sub directories (except for `trace`) should have a proper backup.

We will try to elaborate more about some of the sub-directories below

### mnesia

Mnesia the database, is also often being referred to as the `built-in database` in EMQX documents.

EMQX stores and distributes its state in this database.
Some examples are:

* Connected clients
* MQTT topics
* Routing information, when clustered, it helps to map MQTT topics to a peer node in the cluster
* Alarms

### configs

Not be confused with the `etc` directory.

The `etc` directory is read-only for EMQX. From the config file(s) in `etc` EMQX bootstraps
some intermediate configs each time when it reboots. The intermediate config files are stored
in `<data_dir>/configs`.

EMQX provides HTTP APIs for sysadmins to change configs at runtime. If such changes are to be
persisted, this `configs` directory is where the override configs reside.

## log directory

When EMQX is configured to log to file, the files are written to this sub-directory.
