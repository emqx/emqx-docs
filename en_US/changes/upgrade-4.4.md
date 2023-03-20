# Upgrade from 4.3 to 4.4

EMQX 4.3 and 4.4 nodes co-exist in the same cluster.
Hence it's possible to apply a rolling upgrade of a EMQX cluster.
**Note that it is the latest version of 4.4**
For example: connection_low_watermark_alarm configured for e4.3.7 e4.4.1 feature.
If you upgrade from e4.3.7 to e4.4.0, it will not be recognized and you need to upgrade to e4.4.1.
So it is recommended to upgrade to the latest version of 4.4.

The upgrade steps of each EMQX node are summarised below:

1. Optional: remove the node from load-balancer
1. Stop the node, (e.g. `emqx stop`, `systemctl stop emqx`)
1. Backup 'data' and 'etc' directories. (more details below)
1. Uninstall v4.3
1. Install v4.4, and restore the backup directories for 'data' and 'etc'
1. Start v4.4, and monitor alerts/error logs
1. Optional: add the node back to the load-balancer

## Data and config backup

Before upgrading, please make sure to backup `data` and `etc` directories.
The `data` directory can be located in below possible paths depending
on installation and configuration.

* Where the environment variable `EMQX_NODE__DATA_DIR` points to
* Where the `node.data_dir` config key points to in `emqx.conf`
* `/opt/emqx/data` when running in docker (typically a mounted volume)
* `<install-path>/data` when installed from zip package extraction
* `/var/lib/emqx/data` when installed from RPM or DEB packages

Take RPM or DEB installation for example.

```bash
## Create a backup dir
mkdir -p ~/emqx-backup/etc/
mkdir -p ~/emqx-backup/data/

## Ensure EMQX is stopped
systemctl stop emqx
systemctl status emqx

## Copy the directories
cp -r /etc/emqx ~/emqx-backup/etc/
cp -r /var/lib/emqx/ ~/emqx-backup/data/
```

## Uninstall v4.3

- Taking RPM installation for example

```bash
## Inspect the current installation
rpm -qa | grep emqx

## uninstall
rpm -e emqx-4.3.x-x.x86_64
```

## Install v4.4 and restore data and config from backup

- Taking RPM installation for example

```bash
rpm -ivh emqx-4.4.0-otp24.1.5-3-centos7-amd64.rpm
```

- Restore the data and config from backup

```bash
cp -r ~/emqx-backup/etc/ /etc/emqx/
cp -r ~/emqx-backup/data/ /var/lib/emqx/
```

## Start v4.4

- Taking systemctl for example

```bash
# Start
systemctl start emqx
# Check status
systemctl status emqx
```

## Monitor the new service

- Check clustering status

The v4.4 node should automatically re-join the cluster

```bash
/usr/bin/emqx_ctl cluster status

```

- Inspect the logs to make sure everything is working as expected

```bash
## Find the latest log rotation file
ls -htl /var/log/emqx/emqx.log.*[0-9] | head -n 1
## Check the latest (100) lines where N is the latest rotation found above
tail -f -n 100 /var/log/emqx/emqx.log.N
```

- Check EMQX dashboard to see if the node is running as expected.

- Ensure the `emqx_schema_registry` plugins started.
  If you are using the `Schema Registry` feature of the rule engine, `emqx_schema_registry` must be started.
  On dashboard, click on "General" - "Plugin", make sure the `emqx_schema_registry` plugins is started.
  If not, you can type in `emqx_schema_registry` in the search bar to find the plugin and then start it.
