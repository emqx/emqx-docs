# Upgrade from 4.2 to 4.3

Due to database schema and inter-broker API changes, an EMQX 4.3 node can not
join a 4.2 cluster.

The recommended way to upgrade is as follows

1. Export database from one of the 4.2 nodes (see below for db export command)
1. Create a standing-by 4.3 cluster using 4.3 configurations
1. Import database to one of the 4.3 nodes and restart the cluster
1. Cut traffic from 4.2 cluster over to 4.3 cluster
1. Shutdown the 4.2 cluster

To provision the new cluster, old data stored in the builtin database (Mnesia)
can be migrated with data export and import [command](#database-migration) runs.

There are also a few important [configuration changes](#important-config-changes)
and [behaviour changes](#important-behaviour-changes) which are summarised below.

## Data migration

It is recommended to take a backup of the `data` directory for each 4.2 node before
the upgrade. The `data` directory can be located in below possible paths depending
on different installation and configuration.

* Where the environment variable `EMQX_NODE__DATA_DIR` points to
* Where the `node.data_dir` config key points to in `emqx.conf`
* `/opt/emqx/data` when running in docker (typically a mounted volume)
* `<install-path>/data` when installed from zip package extraction
* `/var/lib/emqx/data` when installed from RPM or DEB packages

### Copy Data directory

When upgrading from 4.2 to 4.3 in a different host,
the `data` directory should be copied from the 4.2 host to the new 4.3 host.

However due to database schema changes, the old files in `data/mnesia` should *not* be copied,
to the 4.3 node.

### Database migration

NOTE: Please refer to [Data import and export](../advanced/data-import-and-export.md) for details.

To export data from the old cluster, execute command.
```bash
$ emqx_ctl data export
```

This will generate a JSON file with timestamp in its name. This file will be the input of the import command.

`emqx_auth_mnesia` plugin now supports rules based on both `clientid` and `username`.
Previously only one type of filter was supported, as configured in `etc/plugins/emqx_auth_mnesia.conf` file.
In order to import data from the previous EMQX versions, it is necessary to specify the value of this parameter by passing it as a CLI option:

```bash
$ emqx_ctl data import <filename> --env '{"auth.mnesia.as":"username"}'
```

or

```bash
$ emqx_ctl data import <filename> --env '{"auth.mnesia.as":"clientid"}'
```

Or by editing the import file using the same format.

## Important config changes

- EMQX now tries to use tlsv1.3 by default, please make sure openssl is up to date (1.1.1), otherwise SSL related configs such as `listener.ssl.external.tls_versions` may have to be changed and remove tls1.3 from the list.
- New configs `listener.ws.$zone.check_origin_enable` ,`listener.ws.$zone.allow_origin_absence` and `listener.ws.$zone.check_origins` for better websocket security.
- Config `listener.ws.$name.verify_protocol_header` is replaced by `listener.ws.external.fail_if_no_subprotocol` and `listener.ws.external.supported_subprotocols`
- Config `node.heartbeat` cannot be overriden from environment variable `EMQX_NODE__HEARTBEAT`. To be fixed [#5929](https://github.com/emqx/emqx/issues/5929)
- Set `log.formatter=json` to log in JSON format, it may requre more CPU resources.
- Set `log.single_line=true` to collect log entries in single lines.
- Config `rpc.tcp_client_num` now is set to 1 by default.

### Important plugin config changes

#### `emqx_auth_http`

To make it easier to understand, we use the key word REQUEST for `auth_req`，`super_req` and `acl_req`.

- The config key `auth.http.REQUEST` is replaced by `auth.http.REQUEST.url`.
- Config key `auth.http.header.<Key>` is replaced by `auth.http.REQUEST.headers.<Key> = <Value>`. I.e. it is now possible to configure different HTTP headers per REQUEST type.
- Config key `auth.http.REQUEST.content_type` is replaced by `auth.http.REQUEST.headers.content_type`.
- Config key `auth.http.request.timeout` is replaced by `auth.http.timeout`.
- Config key `auth.http.request.connect_timeout` is replaced by `auth.http.connect_timeout`.
- The config keys `auth.http.request.retry_times`，`auth.http.request.retry_interval` and `auth.http.request.retry_backoff` are deleted.
- New config `auth.http.pool_size` for configurable pool size.
- New config `auth.http.enable_pipelining` to enable HTTP Pipelining.
- New security related configs: `auth.http.ssl.verify` and `auth.http.ssl.server_name_indication`.

#### `emqx_auth_mongo`:

- Config key `auth.mongo.login` is renamed to `auth.mongo.username`
- Config keys `auth.mongo.ssl_opts.*` are replaced by ` auth.mongo.ssl.*`

#### `emqx_auth_pgsql`:

- Config keys `auth.pgsql.ssl_opts.*` are replaced by ` auth.pgsql.ssl.*`

#### `emqx_auth_redis`:

- SSL configs are now groupped by `.ssl` in the config path. E.g. `auth.redis.cacertfile` is now `auth.redis.ssl.cacertfile`

#### `emqx_web_hook` config changes

Note: webhook resources and actions in rule engine are migrated by the database export&import command.

- Config key `web.hook.api.url` is renamed to `web.hook.url`.
- Config key `web.hook.encode_payload` is replaced by `web.hook.body.encoding_of_payload_field`
- New security config `web.hook.ssl.verify` and `web.hook.ssl.server_name_indication`
- New config `web.hook.pool_size` makes possible to configure http connection pool size.
- New config `web.hook.enable_pipelining` to enable http pipelining.

## Important behaviour changes

- Log timestamp is now in RFC3339 format, make sure your log indexer is ready for this change.
- When `round_robin` strategy is used for shared subscription, the dispatch now starts from a random member in the group (instead of always starting from the first).
- When rule engin starts up, unavailable resources are automatically retried.
- New MacOS package no longer supports the version older than 10.14.
- The underlying transport protocol for `emqx_exhook` plugin (which supports developing plugins in other languages) has been changed from erlport to gRPC,
  meaning it will not able to communicate with extension plugins develped for 4.2.
