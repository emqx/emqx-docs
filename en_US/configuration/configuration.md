---
# 标题
title: 配置项
# 编写日期
date: 2020-03-03 10:18:36
# 作者 Github 名称
author: tigercl
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref: undefined
---

# Configuration

## cluster

### cluster.name

| Type   | Default  |
| ------ | -------- |
| string | `emqxcl` |

##### Description

 Cluster name.

<br />

### cluster.proto_dist

| Type | Optional Value                      | Default    |
| ---- | ----------------------------------- | ---------- |
| enum | `inet_tcp`, `inet6_tcp`, `inet_tls` | `inet_tcp` |

##### Description

Distributed Erlang cluster protocol type. Available values are:

- `inet_tcp`: using IPv4
- `inet6_tcp`: using IPv6
- `inet_tls`: using TLS, required to be used with `node.ssl_dist_optfile` configuration

<br />

### cluster.discovery

| Type | Optional Value                                    | Default  |
| ---- | ------------------------------------------------- | -------- |
| enum | `manual`, `static`, `mcast`, `dns`, `etcd`, `k8s` | `manual` |

##### Description

Cluster node discovery method. Available values are:

- `manual`: join the cluster manually
- `static`: Configure static nodes. Configure several fixed nodes, and the new node joins the cluster by connecting one of the fixed nodes.
- `mcast`: Use UDP multicast to discover nodes.
- `dns`: Use DNS A records to discover nodes.
- `etcd`: Use etcd to discover nodes.
- `k8s`: Use Kubernetes to discover nodes.

<br />

### cluster.autoheal

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### Description

Enable or disable the automatic recovery mechanism of cluster brain splitting.

<br />

### cluster.autoclean

| Type     | Default |
| -------- | ------- |
| duration | `5m`    |

##### Description

Specify how long to delete short-line nodes from the cluster.

<br />

### cluster.static.seeds

| Type   | Default | Example                                   |
| ------ | ------- | ----------------------------------------- |
| string | -       | `emqx1@192.168.0.100,emqx2@192.168.0.101` |

##### Description

When using static clustering, specify a fixed list of nodes, separated by commas `,` between multiple nodes.

<br />

### cluster.mcast.addr

| Type   | Default       |
| ------ | ------------- |
| ipaddr | `239.192.0.1` |

##### Description

When using the mcast cluster, specify the multicast address.

<br />

### cluster.mcast.ports

| Type   | Default |
| ------ | ------- |
| string | `4369`  |

##### Description

When using the mcast cluster, specify the multicast port. If there are multiple ports, separate them with commas `,`.

<br />

### cluster.mcast.iface

| Type   | Default   |
| ------ | --------- |
| ipaddr | `0.0.0.0` |

##### Description

When using mcast cluster, specify which local IP address the node discovery service needs to bind to.

<br />

### cluster.mcast.ttl

| Type    | Default |
| ------- | ------- |
| integer | 255     |

##### Description

When using mcast cluster, specify the Time-To-Live value of multicast.

<br />

### cluster.mcast.loop

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    |  `on`, `off`   | `on`    |

##### Description

When using mcast clustering, set whether multicast packets are delivered to the local loopback address.

<br />

### cluster.dns.name

| Type   | Default | Example         |
| ------ | ------- | --------------- |
| string | -       | `mycluster.com` |

##### Description

When using the dns cluster, specify the name of the DNS A record. emqx will access the DNS A record to obtain a list of IP addresses, and then splice the APP name specified in `cluster.dns.app` to get a list of all nodes in the cluster.

##### Example

Set `cluster.dns.app = emqx`, and configure a DNS: `mycluster.com`, which points to 3 IP addresses:

```
192.168.0.100
192.168.0.101
192.168.0.102
```

Then get the list of cluster nodes as follows:

```
emqx@192.168.0.100
emqx@192.168.0.101
emqx@192.168.0.102
```

<br />

### cluster.dns.app

|  Type  | Default | Example |
| ------ | ------- | ------- |
| string | -       | `emqx`  |

##### Description

When using dns cluster, it is used to splice the IP list obtained from `cluster.dns.name` to get a list of node names.

<br />

### cluster.etcd.server

|  Type  | Default | Example                 |
| ------ | ------- | ----------------------- |
| string | -       | `http://127.0.0.1:2379` |

##### Description

When using etcd cluster, specify the address of etcd service. If there are multiple services, use commas to separate them.

<br />

### cluster.etcd.prefix

|  Type  | Default | Example  |
| ------ | ------- | -------- |
| string | -       | `emqxcl` |

##### Description

When using etcd cluster, specify the prefix of etcd path. Each node creates a path in etcd:

```
v2/keys/<prefix>/<cluster.name>/<node.name>
```

<br />

### cluster.etcd.node_ttl

|   Type   | Default | Example |
| -------- | ------- | ------- |
| duration | -       | `1m`    |

##### Description

When using etcd cluster, specify the expiration time of the node path in etcd.

<br />

### cluster.etcd.ssl.keyfile

|   Type   | Default | Example                    |
| -------- | ------- | -------------------------- |
| string   |  -      | `etc/certs/client-key.pem` |

##### Description

When using SSL to connect to etcd, specify the client's private key file.

<br />

### cluster.etcd.ssl.certfile

|   Type   | Default | Example                |
| -------- | ------- | ---------------------- |
| string   |  -      | `etc/certs/client.pem` |

##### Description

When using SSL to connect to etcd, specify the SSL client certificate file.

<br />

### cluster.etcd.ssl.cacertfile

|   Type   | Default | Example            |
| -------- | ------- | ------------------ |
| string   |  -      | `etc/certs/ca.pem` |

##### Description

When using SSL to connect to etcd, specify the CA certificate file for SSL.

<br />

### cluster.k8s.apiserver

|   Type   | Default | Example                      |
| -------- | ------- | ---------------------------- |
| string   | -       | `http://10.110.111.204:8080` |

##### Description

When using the k8s cluster, specify the Kubernetes API Server. If there are multiple Servers, separate them with commas `,`.

<br />

### cluster.k8s.service_name

|   Type   | Default | Example |
| -------- | ------- | ------- |
| string   | -       | `emqx`  |

##### Description

When using k8s cluster, specify the service name of EMQ X Broker in Kubernetes.

<br />

### cluster.k8s.address_type

| Type |  Optional Value         | Default |
| ---- | ----------------------- | ------- |
| enum | `ip`, `dns`, `hostname` | `ip`    |

##### Description

When using k8s cluster, address_type is used to obtain the host list from the response of the Kubernetes interface.

##### Example

Specifying `cluster.k8s.address_type` as `ip`, it will get the list of IP addresses of emqx services from the Kubernetes interface:

```
172.16.122.31
172.16.122.32
172.16.122.33
```

Then splice with the app name specified by `cluster.k8s.app_name` configuration to get a list of emqx nodes:

```
emqx@172.16.122.31
emqx@172.16.122.32
emqx@172.16.122.33
```

<br />

### cluster.k8s.app_name

|   Type   | Default | Example |
| -------- | ------- | ------- |
| string   |  -      | `emqx`  |

##### Description

When using k8s clustering, app_name is used to splice with the obtained Host list to get the node list.

<br />

### cluster.k8s.suffix

|   Type   | Default | Example             |
| -------- | ------- | ------------------- |
| string   | -       | `pod.cluster.local` |

##### Description

When using the k8s method and specifying `cluster.k8s.address_type` as the dns type, you can set the suffix of the emqx node name, and splice with `cluster.k8s.namespace` to get a list of node names.

<br />

### cluster.k8s.namespace

| Type   | Default | Example   |
| ------ | ------- | --------- |
| string | -       | `default` |

##### Description

When using the k8s method and specifying `cluster.k8s.address_type` as the dns type, you can set the namespace of the emqx node name, and splice with `cluster.k8s.suffix` to get a list of node names.

##### Example

Setting `cluster.k8s.address_type` to `dns`, you will get the dns list of emqx service from the Kubernetes interface:

```
172-16-122-31
172-16-122-32
172-16-122-33
```

Then splice with `cluster.k8s.app_name = emqx`，`cluster.k8s.suffix = pod.cluster.local`，`cluster.k8s.namespace = default` to get a list of emqx node names in the form of dns:

```
emqx@172-16-122-31.default.pod.cluster.local
emqx@172-16-122-32.default.pod.cluster.local
emqx@172-16-122-33.default.pod.cluster.local
```

<br />

### node.name

| Type   | Default          |
| ------ | ---------------- |
| string | `emqx@127.0.0.1` |

##### Description

The node name. The format is `<name> @ <host>`. Where `<host>` can be an IP address or FQDN. See [http://erlang.org/doc/reference_manual/distributed.html](http://erlang.org/doc/reference_manual/distributed.html) for details

<br />

### node.cookie

| Type   | Default            |
| ------ | ------------------ |
| string | `emqxsecretcookie` |

##### Description

The cookie value used by the distributed Erlang cluster.

<br />

### node.data_dir

| Type   | Default  |
| ------ | -------- |
| folder | `./data` |

##### Description

The node's data directory, which is used to store Mnesia data files.

<br />

### node.heartbeat

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    |  `on`, `off`   | `off`   |

##### Description

System tuning parameters. This configuration will override the `-heart` parameter in the `vm.args` file.

Enable or disable Erlang runtime detection mechanism, and restart automatically when the runtime terminates. Use with care to avoid restarting the monitored process when emqx is closed manually.

<br />

### node.async_threads

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| integer | 0 - 1024       | 4       |

##### Description

System tuning parameters. This configuration will override the `+A` parameter in the `vm.args` file.

Set the number of threads in the asynchronous thread pool in Erlang runtime, see [http://erlang.org/doc/man/erl.html](http://erlang.org/doc/man/erl.html) for details.

<br />

### node.process_limit

|   Type   | Optional Value   | Default |
| -------- | ---------------- | ------- |
| integer  | 1024 - 134217727 | 2097152 |

##### Description

System tuning parameters. This configuration will override the `+P` parameter in the `vm.args` file.

Set the maximum number of processes allowed by Erlang, which will affect the number of connections that emqx nodes can process. See [http://erlang.org/doc/man/erl.html](http://erlang.org/doc/man/erl.html) for details.

<br />

### node.max_ports

|   Type   | Optional Value   | Default |
| -------- | ---------------- | ------- |
| integer  | 1024 - 134217727 | 1048576 |

##### Description

System tuning parameters. This configuration will override the `+Q ` parameter in the `vm.args` file.

Set the maximum number of ports allowed by Erlang. See [http://erlang.org/doc/man/erl.html](http://erlang.org/doc/man/erl.html) for details.

<br />

### node.dist_buffer_size

| Type     | Optional Value | Default |
| -------- | -------------- | ------- |
| bytesize | 1KB - 2GB      | `8MB`   |

##### Description

System tuning parameters. This configuration will override the `+zdbbl` parameter in the `vm.args` file.

Set the maximum cache size used by Erlang distributed communication. See [http://erlang.org/doc/man/erl.html](http://erlang.org/doc/man/erl.html) for details.

<br />

### node.max_ets_tables

| Type    | Default |
| ------- | ------- |
| integer | 262144  |

##### Description

System tuning parameters. This configuration will override the `+e` parameter in the `vm.args` file.

Set the maximum number of ETS tables allowed in Erlang runtime. See [http://erlang.org/doc/man/erl.html](http://erlang.org/doc/man/erl.html) for details.

<br />

### node.global_gc_interval

| Type     | Default |
| -------- | ------- |
| duration | `15m`   |

##### Description

System tuning parameters, which set how often Erlang runs to force a global garbage collection.

<br />

### node.fullsweep_after

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| integer | 0 - 65535      | 1000    |

##### Description

System tuning parameters. This configuration will override the `-env ERL_FULLSWEEP_AFTER` parameter in the `vm.args` file.

Set how many times the generational GC will run before Erlang runs a fullsweep GC. For details, see [http://erlang.org/doc/man/erlang.html#spawn_opt-4](http://erlang.org/doc/man/erlang.html#spawn_opt-4).

<br />

### node.crash_dump

| Type    | Default          |
| ------- | ---------------- |
| string  | `log/crash.dump` |

##### Description

Set the storage path and file name of the Erlang crash_dump file.

<br />

### node.ssl_dist_optfile

| Type   | Default             |
| ------ | ------------------- |
| string | `etc/ssl_dist.conf` |

##### Description

This configuration will override the `-ssl_dist_optfile` parameter in the `vm.args` file.

If you use SSL to establish an emqx cluster, you need to specify the SSL distributed protocol configuration file. It needs to be used with `cluster.proto_dist = inet_tls`.

<br />

### node.dist_net_ticktime

| Type    | Default |
| --------| ------- |
| integer | 120     |

##### Description

System tuning parameters. This configuration will override the `-kernel net_ticktime` parameter in the `vm.args` file.

Specifying how long time when a node has been unresponsive, it is considered to be down and disconnected. For details, see [http://www.erlang.org/doc/man/kernel_app.html#net_ticktime](http://www.erlang.org/doc/man/kernel_app.html#net_ticktime).

<br />

### node.dist_listen_min

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| integer | 1024 - 65535   | 6369    |

##### Description

Set a TCP port range together with `node.dist_listen_max`. This port ranget is used for distribution to distributed Erlang as a listening port for distributed channels. Note that if a firewall is set between nodes, this port range needs to be placed into the firewall's whitelist.

<br />

### node.dist_listen_max

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| integer | 1024 - 65535   | 6369    |

##### Description

Set a TCP port range together with `node.dist_listen_min`. This port range is used for distribution to distributed Erlang as a listening port for distributed channels. Note that if a firewall is set up between nodes, this port rangeneeds to be put in The firewall's whitelist.

<br />

### rpc.mode

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `sync`, `async` | `async` |

##### Description

RPC mode. Synchronous or asynchronous mode is optional.

<br />

### rpc.async_batch_size

| Type    | Default |
| ------- | ------- |
| integer | 256     |

##### Description

The maximum number of batch messages sent in asynchronous mode. Note that this configuration does not work in synchronous mode.

<br />

### node.tcp_server_port

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| integer | 1024 - 65535   | 5369    |

##### Description

Set the listening port used by RPC local service

<br />

### node.tcp_client_port

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| integer | 1024-65535     | 5369    |

##### Description

Set the port of the remote RPC service.

<br />

### node.tcp_client_num

| Type    | Optional Value | Default         |
| ------- | -------------- | --------------- |
| integer | 1 - 256        | CPU core number / 2 |

##### Description

Set the number of RPC communication channels initiated by this node to each remote node. Set to 1 to ensure the order of messages. Keep the default value (half the number of CPU cores) to improve RPC throughput.

<br />

### rpc.connect_timeout

| Type     | Default |
| -------- | ------- |
| duration | `5s`    |

##### Description

Timeout for establishing an RPC connection. It means how long will it give up after trying if the remote node does not respond when establishing a connection, .

<br />

### rpc.send_timeout

| Type     | Default |
| -------- | ------- |
| duration | `5s`    |

##### Description

Timeout for sending, which means how long to give up after sending the message.

<br />

### rpc.authentication_timeout

|   Type   | Default |
| -------- | ------- |
| duration | `5s`    |

##### Description

RPC authentication timeout. It means how long it will give up if the remote node does not respond, .

<br />

### rpc.call_receive_timeout

|   Type   | Default |
| -------- | ------- |
| duration | `15s`   |

##### Description

The timeout period of RPC synchronous mode. It means how long it will take before giving up if the RPC synchronous call fails to receive a reply.

<br />

### rpc.socket_keepalive_idle

|   Type   | Default |
| -------- | ------- |
| duration | `900s`  |

##### Description

It means how long after the last packet was sent, keepalive probe packets are sent.

<br />

### rpc.socket_keepalive_interval

|   Type   | Default |
| -------- | ------- |
| duration | `75s`   |

##### Description

The interval between keepalive detection messages.

<br />

### rpc.socket_keepalive_count

| Type    | Default |
| ------- | ------- |
| integer | 9       |

##### Description

For how many times if the keepalive probe message fails to receive a reply, the RPC connection is considered lost.

<br />

### rpc.socket_sndbuf

| Type     | Default |
| -------- | ------- |
| bytesize | `1MB`   |

##### Description

TCP tuning parameters. TCP sending buffer size.

<br />

### rpc.socket_recbuf

|   Type   | Default |
| -------- | ------- |
| bytesize | `1MB`   |

##### Description

TCP tuning parameters. TCP receiving buffer size.

<br />

### rpc.socket_buffer

|   Type   | Default |
| -------- | ------- |
| bytesize | `1MB`   |

##### Description

TCP tuning parameters. Socket buffer size in user mode.

<br />

### log.to

| Type | Optional Value                   | Default |
| ---- | -------------------------------- | ------- |
| enum | `off`, `file`, `console`, `both` | `both`  |

##### Description

Where to output the log. The optional values are:

- **off:** Disable logging completely
- **file:** Only output log to file
- **console:** Only output logs to standard output (emqx console)
- **both:** output log to file and standard output at the same time (emqx console)

<br />

### log.level

| Type | Optional Value                                                                     | Default   |
| ---- | ---------------------------------------------------------------------------------- | --------- |
| enum | `debug`, `info`, `notice`, `warning`</>`error`, `critical`, `alert`, `emergency` | `warning` |

##### Description

Global log level. This includes the primary log level and all log handlers. For details, see [log level and log handlers](../getting-started/log.md#log-level-and-log-handlers).

<br />

### log.dir

| Type | Default |
| ---- | ------- |
| dir  | `./log` |

##### Description

Log file directory.

<br />

### log.file

| Type   | Default    |
| ------ | ---------- |
| string | `emqx.log` |

##### Description

The prefix of the log file. For example, if you use the default value (`log.file = emqx.log`), the log file name will be `emqx.log.1`, `emqx.log.2`, ...

<br />

### log.chars_limit

| Type    | Default |
| ------- | ------- |
| integer | -1      |

##### Description

Set the maximum length of a single log message. If this length is exceeded, the log message will be truncated. `-1` means no limit.

<br />

### log.rotation.size

| Type     | Default |
| -------- | ------- |
| bytesize | `10MB`  |

##### Description

Set the size of a single log file. If it exceeds this size, the log file will be rolled to create a new log file.

<br />

### log.rotation.count

| Type    | Default |
| ------- | ------- |
| integer | 5       |

##### Description

Set the total number of log files. If this number is exceeded, the next log file will overwrite the first file.

<br />

### log.\<level>.file

| Type   | Default |
| ------ | ------- |
| string | -       |

##### Description

Set a separate log file for a certain log level.

##### Example

Separately output info and above logs to `info.log.N` file:

```
log.info.file = info.log
```

Output error and error logs separately to the `error.log.N` file

```
log.error.file = error.log
```

<br />

### allow_anonymous

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### Description

Whether to allow anonymous users to log in to the system.

::: tip Tip
It is recommended to disable this option in the production environment.
:::

<br />

### acl_nomatch

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `allow`, `deny` | `allow` |

##### Description

When the ACL is not hit, allow or deny the publish/subscribe operation.

<br />

### acl_file

| Type   | Default        |
| ------ | -------------- |
| string | `etc/acl.conf` |

##### Description

The default path of ACL file.

<br />

### enable_acl_cache

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### Description

Whether to enable ACL caching.

<br />

### acl_cache_max_size

| Type    | Default  |
| ------- | -------- |
| integer | 32       |

##### Description

Maximum cache number of ACL rule.

<br />

### acl_cache_ttl

| Type     | Default |
| -------- | ------- |
| duration | `1m`    |

##### Description

Maximum cache time of ACL rule.

<br />

### acl_deny_action

| Type    | Optional Value         | Default  |
| ------- | ---------------------- | -------- |
| enum    | `ignore`, `disconnect` | `ignore` |

##### Description

What to do after the ACL check fails.

- `ignore`：No operation
- `disconnect`：disconnect.

<br />

### flapping_detect_policy

| Type   | Default      |
| ------ | ------------ |
| string | `30, 1m, 5m` |

##### Description

Specify the `Flapping` inspection strategy.

Format: \<threshold>,\<duration>,\<banned>`.

For example, `30, 1m, 5m`, it means that if the client disconnects 30 times within 1 minute, then login is prohibited for the next 5 minutes

<br />

### mqtt.max_packet_size

| Type      | Default |
| --------- | ------- |
| bytesize  | `1MB`   |

##### Description

The maximum allowed length of MQTT messages.

<br />

### mqtt.max_clientid_len

| Type    | Default |
| ------- | ------- |
| integer | 65535   |

##### Description

The maximum allowed length of  Client ID  string.

<br />

### mqtt.max_topic_levels

| Type    | Default |
| ------- | ------- |
| integer | 0       |

##### Description

The maximum allowed level of topics for client subscription. 0 means no limit.

<br />

### mqtt.max_qos_allowed

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `0`, `1`, `2`  | `2`     |

##### Description

The maximum allowed QoS level for client to publish.

<br />

### mqtt.max_topic_alias

| Type    | Default |
| ------- | ------- |
| integer | 65535   |

##### Description

The maximum allowed number of topic aliases. 0 means that topic aliases are not supported.

<br />

### mqtt.retain_available

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### Description

Whether to support Retain message.

<br />

### mqtt.wildcard_subscription

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### Description

Whether to support subscribing to wildcard topics.

<br />

### mqtt.shared_subscription

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### Description

Whether to support shared subscriptions.

<br />

### mqtt.ignore_loop_deliver

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### Description

Whether to ignore the message sent by itself. If it is ignored, it means that EMQ X Broker will not deliver this message to the sender of the message.

<br />

### mqtt.strict_mode

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### Description

Whether to enable the strict check mode. The strict check mode will check the correctness of the MQTT message in more detail.

<br />

### zone.external.idle_timeout

| Type     | Default |
| -------- | ------- |
| duration | `15s`   |

##### Description

The daze time after the TCP connection is established. If no packets are received within this time, the connection will be shutdown.

<br />

### zone.external.enable_acl

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### Description

Whether to enable ACL check.

<br />

### zone.external.enable_ban

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### Description

Whether to enable blacklist.

<br />

### zone.external.enable_stats

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### Description

Whether to enable client status statistics.

<br />

### zone.external.acl_deny_action

| Type | Optional Value         | Default  |
| ---- | -------------------- - | -------- |
| enum | `ignore`, `disconnect` | `ignore` |

##### Description

What to do after the ACL check fails.

- `ignore`：No any operation.
- `disconnect`：disconnect.

<br />

### zone.external.force_gc_policy

| Type    | Default      |
| ------- | ------------ |
| string  | `16000|16MB` |

##### Description

When a certain number of messages, or bytes, are received, a garbage collection is forced.

Format: `<Number> | <Bytes>`.

For example, `16000|16MB` means that when ` 16000` messages are received, or a byte of `16MB` flows in, a garbage collection is forced.

<br />

### zone.external.force_shutdown_policy

| Type    | Default |
| ------- | ------- |
| string  | -       |

##### Description

When the process message queue length, or the memory bytes reaches a certain value, the process is forced to close.

The "message queue" here refers to the "message mailbox" of the Erlang process, not the "mqueue" of QoS 1 and QoS 2.

Format: `<Number> | <Bytes>`.

For example, `32000|32MB` means that when the process accumulates `32000` messages, or the process occupies memory up to `32MB`, the process is closed.

<br />

### zone.external.max_packet_size

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### Description

The maximum allowed length of MQTT packet.

<br />

### zone.external.max_clientid_len

| Type    | Default |
| ------- | ------- |
| integer | -       |

##### Description

The maximum length of Client ID string.

<br />

### zone.external.max_topic_levels

| Type    | Default |
| ------- | ------- |
| integer | -       |

##### Description

The maximum allowed level of topics for client subscription. 0 means no limit.

<br />

### zone.external.max_qos_allowed

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `0`, `1`, `2`  | -       |

##### Description

The maximum QoS level allowed for the client to publish.

<br />

### zone.external.max_topic_alias

| Type    | Default |
| ------- | ------- |
| integer | -       |

##### Description

The maximum number of topic aliases. 0 means that topic aliases are not supported.

<br />

### zone.external.retain_available

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | -       |

##### Description

Whether to support Retain message.

<br />

### zone.external.wildcard_subscription

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | -       |

##### Description

Whether to support subscribing to wildcard topics.

<br />

### zone.external.shared_subscription

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | -       |

##### Description

Whether to support shared subscriptions.

<br />

### zone.external.server_keepalive

| Type    | Default |
| ------- | ------- |
| integer | -       |

##### Description

Keepalive time specified by the server, used for MQTT v5.0 CONNACK messages

<br />

### zone.external.keepalive_backoff

| Type  | Optional Value | Default |
| ----- | -------------- | ------- |
| float | > 0.5          | 0.75    |

##### Description

Keepalive backoff index. If no data packet is received from the client within the time of  `Keepalive * backoff * 2`, it is considered that the client has heartbeat timeout.

<br />

### zone.external.max_subscriptions

| Type    | Default |
| ------- | ------- |
| integer | 0     |

##### Description

The maximum number of topics that a single client is allowed to subscribe to. `0` means no limit.

<br />

### zone.external.upgrade_qos

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `off`   |

##### Description

Allow EMQ X Broker to force the QoS level of the message upgrading to the subscribed QoS level when publishing the message.

<br />

### zone.external.max_inflight

| Type    | Default |
| ------- | ------- |
| integer | 32      |

##### Description

Inflight window size: The inflight window is used to store unacknowledged QoS 1 and QoS 2 messages.

<br />

### zone.external.retry_interval

| Type     | Default |
| -------- | ------- |
| duration | `30s`   |

##### Description

Message retransmission interval: EMQ X Broker checks whether message retransmission is required at each interval.

<br />

### zone.external.max_awaiting_rel

| Type    | Default |
| ------- | ------- |
| integer | 100     |

##### Description

The maximum receiving window for QoS 2 messages, which configures how many QoS 2 messages from the client can be processed by EMQ X Broker simultaneously. `0` means no limit.

<br />

### zone.external.await_rel_timeout

| Type     | Default |
| -------- | ------- |
| duration | `300s`  |

##### Description

Time for QoS 2 message processing timeout. If the QoS PUBREL message has not been received after the timeout, the message is dropped from the receiving window.

<br />

### zone.external.session_expiry_interval

| Type     | Default |
| -------- | ------- |
| duration | `2h`    |

##### Description

The default timeout period of the session, which is mainly used for MQTT v3.1 and v3.1.1 protocols. In MQTT v5.0, this value is usually carried in the client's connection message.

<br />

### zone.external.max_mqueue_len

| Type    | Default |
| ------- | ------- |
| integer | 1000    |

##### Description

The maximum length of the message queue. When the flight window is full, or the client is offline, the message will be stored in the queue. 0 means no limit.

<br />

### zone.external.mqueue_priorities

| Type   | Optional Value   | Default |
| ------ | ---------------- | ------- |
| string | `none`, `<Spec>` | `none`  |

##### Description

Queue message priority configuration:

- `none`：no prioritization.
- `<Spec>`：A message priority table, which configures the priority of messages under a certain topic. For example:
    * `topic/1=10`: indicates that the message priority of the topic `topic/1` is `10`.
    * `topic/1=10,topic/2=8`: indicates that the priority of two topics is configured, which are `10` and `8` respectively.
    * Among them, the higher the priority value, the higher the priority level.

When the length of the message queue is limited, low priority messages will be dropped first.

<br />

### zone.external.mqueue_default_priority

| Type    | Optional Value      | Default   |
| ------- | ------------------- | --------- |
| enum    | `highest`, `lowest` | `highest` |

##### Description

The default priority level of the message.

<br />

### zone.external.mqueue_store_qos0

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### Description

Whether the message queue stores QoS 0 messages.

<br />

### zone.external.enable_flapping_detect

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `off`   |

##### Description

Whether to enable `Flapping` check.

<br />

### zone.external.mountpoint

| Type   | Default |
| ------ | ------- |
| string | -       |

##### Description

After topic mount point is configured, all subscribed and published topics will be prefixed by EMQ X Broker.

The available placeholders are:

- `%c`：Client ID.
- `%u`：Username.

For example, if the mount point is set to `user/%c/`. , when the client with client ID `tom` publishes the topic `open` message, the topic actually routed in EMQ X Broker is `user/tom/open`.

<br />

### zone.external.use_username_as_clientid

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### Description

Whether to use the client's Username as its Client ID.

<br />

### zone.external.ignore_loop_deliver

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### Description

Whether to ignore the message sent by yourself. If ignored, it means that EMQ X Broker will not deliver this message to the sender of the message.

<br />


### zone.external.strict_mode

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### Description

Whether to enable the strict check mode. The strict check mode will check the correctness of the MQTT message in more detail.

<br />

### zone.internal.allow_anonymous

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### Description

Whether to allow anonymous users to log in to the system.

<br />

### zone.internal.enable_stats

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### Description

Whether to enable client status statistics.

<br />

### zone.internal.enable_acl

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `off`   |

##### Description

Whether to enable ACL check.

<br />

### zone.internal.acl_deny_action

| Type    | Optional Value         | Default  |
| ------- | ---------------------- | -------- |
| enum    | `ignore`, `disconnect` | `ignore` |

##### Description

What to do after the ACL check fails.

- `ignore`：No operation.
- `disconnect`：Disconnect.

<br />

### zone.internal.force_gc_policy

| Type    | Default |
| ------- | ------- |
| string  | -       |

##### Description

When a certain number of messages, or bytes, are received, a garbage collection is forced.

Format: `<Number> | <Bytes>`.

For example, `16000|16MB` means that when ` 16000` messages are received, or a byte of `16MB` flows in, a garbage collection is forced.

<br />

### zone.internal.wildcard_subscription

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | -       |

##### Description

Whether to support subscribing to wildcard topics.

<br />

### zone.internal.shared_subscription

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | -       |

##### Description

Whether to support shared subscriptions.

<br />

### zone.internal.max_subscriptions

| Type    | Default |
| ------- | ------- |
| integer | 0       |

##### Description

The maximum number of topics that a single client is allowed to subscribe to. `0` means no limit.

<br />

### zone.internal.max_inflight

| Type    | Default |
| ------- | ------- |
| integer | 128     |

##### Description

Inflight window size: The flight window is used to store unanswered QoS 1 and QoS 2 messages.

<br />

### zone.internal.max_awaiting_rel

| Type    | Default |
| ------- | ------- |
| integer | 1000    |

##### Description

The maximum receiving window for QoS 2 messages, that configures how many QoS 2 messages from the client can be processed by EMQ X Broker simultaneously. `0` means no limit.

<br />

### zone.internal.max_mqueue_len

| Type    | Default |
| ------- | ------- |
| integer | 10000   |

##### Description

The maximum length of the message queue. When the flight window is full, or the client is offline, the message will be stored in the queue. `0` means no limit.

<br />

**``zone.internal.mqueue_store_qos0**

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### Description

Whether the message queue stores QoS 0 messages.

<br />

### zone.internal.enable_flapping_detect

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `off`   |

##### Description

Whether to enable `Flapping` check.

<br />

### zone.internal.force_shutdown_policy

| Type    | Default |
| ------- | ------- |
| string  | -       |

##### Description

When the process message queue length, or the memory bytes reaches a certain value, the process is forced to close.

The "message queue" here refers to the "message mailbox" of the Erlang process, not the "mqueue" of QoS 1 and QoS 2.

Format: `<Number> | <Bytes>`.

For example, `32000|32MB` means that when the process accumulates `32000` messages, or the process occupies memory up to `32MB`, the process is closed.

<br />

### zone.internal.mountpoint

| Type    | Default |
| ------- | ------- |
| string  | -       |

##### Description

After topic mount point is configured, all subscribed and published topics will be prefixed by EMQ X Broker.

The available placeholders are:

- `%c`：Client ID.
- `%u`：Username.

For example, if the mount point is set to `user/%c/`. , when the client with client ID `tom` publishes the topic `open` message, the topic actually routed in EMQ X Broker is `user/tom/open`.

<br />

### zone.internal.ignore_loop_deliver

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### Description

Whether to ignore the message sent by itself. If ignored, it means that EMQ X Broker will not deliver this message to the sender of the message.

<br />

### zone.internal.strict_mode

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### Description

Whether to enable the strict check mode. The strict check mode will check the correctness of the MQTT message in more detail.

<br />

### zone.internal.bypass_auth_plugins

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### Description

Whether to allow clients under this zone to bypass the authentication step of the authentication plugin.

<br />

### listener.tcp.external

| Type    | Default        |
| ------- | -------------- |
| string  | `0.0.0.0:1883` |

##### Description

Configure the listening address of the MQTT / TCP listener named `external`.

##### Example

`1883`: monitors IPv4 `0.0.0.0: 1883`.
`127.0.0.1: 1883`: monitor address is `1883` port on the `127.0.0.1` network card.
`:: 1: 1883`: monitors the IPv6 address as `1883` port on the `:: 1` network card.

<br />

### listener.tcp.external.acceptors

| Type    | Default |
| ------- | ------- |
| integer | 8       |

##### Description

The size of the listener's receiving pool.

<br />

### listener.tcp.external.max_connections

| Type    | Default |
| ------- | ------- |
| integer | 1024000 |

##### Description

The maximum number of concurrent connections allowed by the listener.

<br />

### listener.tcp.external.max_conn_rate

| Type    | Default |
| ------- | ------- |
| integer | 1000    |

##### Description

The maximum access rate allowed by the listener. Unit: pcs / sec

<br />

### listener.tcp.external.active_n

| Type    | Default |
| ------- | ------- |
| integer | 100     |

##### Description

The number of times the listener continues to receive TCP packets.

<br />

### listener.tcp.external.zone

| Type    | Default    |
| ------- | ---------- |
| string  | `external` |

##### Description

The configuration zone to which the listener belongs.

<br />

### listener.tcp.external.rate_limit

| Type    | Default |
| ------- | ------- |
| string  | -       |

##### Description

The rate limit of the listener. The format is `<limit>,<duration>`.

##### Example

`100KB,10s`：Limit the number of incoming bytes within 10 seconds not to exceed 100 KB.

<br />

### listener.tcp.external.access.1

| Type    | Default     |
| ------- | ----------- |
| string  | `allow all` |

##### Description

List of ACL rules of the listener. It is used to set the white/black list of the connection layer.

##### Example

`allow all`：Allow all TCP connections.
`allow 192.168.0.0/24`：Allow TCP connection with network address `192.168.0.0/24`.

At the same time, this configuration can configure multiple rules:
```
listener.tcp.external.access.1 = deny 192.168.0.1
listener.tcp.external.access.2 = allow all
```

It means that all TCP connections except `192.168.0.1` are allowed.

<br />

### listener.tcp.external.proxy_protocol

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | -       |

##### Description

Whether the listener enables `Proxy Protocol` support.

If the EMQ X cluster is deployed behind HAProxy or Nginx, and you need to get the client's real source IP address and port, you need to enable this configuration.

`Proxy Protcol` : [https://www.haproxy.com/blog/haproxy/proxy-protocol](https://www.haproxy.com/blog/haproxy/proxy-protocol).

<br />

### listener.tcp.external.proxy_protocol_timeout

| Type     | Default |
| -------- | ------- |
| duration | -       |

##### Description

Set the timeout for Proxy Protocol parsing. If no Proxy Protocol packet is received within this time, EMQ X Broker will close its connection.

<br />

### listener.tcp.external.backlog

| Type    | Default |
| ------- | ------- |
| integer | 1024    |

##### Description

The maximum length of the TCP connection queue. It indicates the maximum number of TCP connection queues that are allowed in the system to undergo three-time handshake.

<br />

### listener.tcp.external.send_timeout

| Type     | Default |
| -------- | ------- |
| duration | `15s`   |

##### Description

Timeout for sending TCP packets.

<br />

### listener.tcp.external.send_timeout_close

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `on`    |

##### Description

Whether to close the connection after TCP packet sending timeout.

<br />

### listener.tcp.external.recbuf

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### Description

TCP receiving buffer size (operating system kernel parameter)

Reference: http://erlang.org/doc/man/inet.html

<br />

### listener.tcp.external.sndbuf

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### Description

TCP sending buffer size (operating system kernel parameter).

Reference:[http://erlang.org/doc/man/inet.html](http://erlang.org/doc/man/inet.html).

<br />

### listener.tcp.external.buffer

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### Description

TCP buffer size (user level).

This value is recommended to be greater than or equal to the maximum value of `sndbuff` and `recbuff` to avoid some performance problems. Without configuration, it equals to the maximum value of sndbuff and recbuff by default.

Reference: [http://erlang.org/doc/man/inet.html](http://erlang.org/doc/man/inet.html).

<br />

### listener.tcp.external.tune_buffer

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | -       |

##### Description

If this configuration is enabled, please set the value equal to the maximum value of `sndbuff` and `recbuff`.

<br />

### listener.tcp.external.nodelay

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### Description

This is the `TCP_NODELAY` parameter. Enabling this option allows small TCP data packets to be sent immediately.

<br />

### listener.tcp.external.reuseaddr

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### Description

This is the `SO_REUSEADDR` parameter. Enabling this option allows the local port to be reused without waiting for the end of the `TIME_WAIT` state.

<br />

### listener.tcp.internal

| Type    | Default           |
| ------- | ----------------- |
| string  | `127.0.0.1:11883` |

##### Description

Configure the listening address of the MQTT / TCP listener named `internal`.

##### Example

`11883`: listen to `0.0.0.0: 11883` of IPv4.
`127.0.0.1:11883`: listening address is` 11883` port on the `127.0.0.1` network card.
`:: 1: 11883`: listen to the `11883` port on the `:: 1` network card of IPv6 address.

<br />

### listener.tcp.internal.acceptors

| Type    | Default |
| ------- | ------- |
| integer | 4       |

##### Description

The size of the listener's receiving pool.

<br />

### listener.tcp.internal.max_connections

| Type    | Default |
| ------- | ------- |
| integer | 1024000 |

##### Description

The maximum number of concurrent connections allowed by the listener.

<br />

### listener.tcp.internal.max_conn_rate

| Type    | Default |
| ------- | ------- |
| integer | 1000    |

##### Description

The maximum access rate allowed by the listener. Unit: pcs / sec

<br />

### listener.tcp.internal.active_n

| Type    | Default |
| ------- | ------- |
| integer | 1000    |

##### Description

The number of times the listener continues to receive TCP packets.

<br />

### listener.tcp.internal.zone

| Type    | Default    |
| ------- | ---------- |
| string  | `internal` |

##### Description

The configuration zone to which the listener belongs.

<br />

### listener.tcp.internal.rate_limit

| Type    | Default |
| ------- | ------- |
| string  | -       |

##### Description

The rate limit of the listener. The format is `<limit>,<duration>`.

##### Example

`100KB,10s`：Limit the number of incoming bytes within 10 seconds no tot exceed 100 KB.


### listener.tcp.internal.backlog

| Type    | Default |
| ------- | ------- |
| integer | 512     |

##### Description

The maximum length of the TCP connection queue. It indicates the maximum number of TCP connection queues that are allowed in the system to undergo three-time handshake.

<br />

### listener.tcp.internal.send_timeout

| Type     | Default |
| -------- | ------- |
| duration | `5s`    |

##### Description

Timeout for sending TCP packets.

<br />

### listener.tcp.internal.send_timeout_close

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `on`    |

##### Description

Whether to close the connection after TCP packet sending timeout.

<br />

### listener.tcp.internal.recbuf

| Type     | Default |
| -------- | ------- |
| bytesize | `64KB`  |

##### Description

TCP receiving buffer size (operating system kernel parameter)

<br />

### listener.tcp.internal.sndbuf

| Type     | Default |
| -------- | ------- |
| bytesize | `64KB`  |

##### Description

TCP sending buffer size (operating system kernel parameter)

<br />

### listener.tcp.internal.buffer

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### Description

TCP buffer size (user level).

<br />

### listener.tcp.internal.tune_buffer

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | -       |

##### Description

If this configuration is enabled, please set the value equal to the maximum value of `sndbuff` and `recbuff`.

<br />

### listener.tcp.internal.nodelay

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### Description

This is the `TCP_NODELAY` parameter. Enabling this option allows small TCP data packets to be sent immediately.

<br />

### listener.tcp.internal.reuseaddr

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### Description

This is the `SO_REUSEADDR` parameter. Enabling this option allows the local port to be reused without waiting for the end of the `TIME_WAIT` state.

<br />

### listener.ssl.external

| Type    | Default        |
| ------- | -------------- |
| string  | `0.0.0.0:8883` |

##### Description

Configure an SSL listener named `external`.

<br />

### listener.ssl.external.acceptors

| Type    | Default |
| ------- | ------- |
| integer | 16      |

##### Description

The size of the listener's receiving pool.

<br />

### listener.ssl.external.max_connections

| Type    | Default |
| ------- | ------- |
| integer | 102400  |

##### Description

The maximum number of concurrent connections allowed by the listener.

<br />

### listener.ssl.external.max_conn_rate

| Type    | Default |
| ------- | ------- |
| integer | 500     |

##### Description

The maximum access rate allowed by the listener. Unit: pcs / sec.

<br />

### listener.ssl.external.active_n

| Type    | Default |
| ------- | ------- |
| integer | 100     |

##### Description

The number of times the listener continues to receive TCP packets.

<br />

### listener.ssl.external.zone

| Type    | Default    |
| ------- | ---------- |
| string  | `external` |

##### Description

The configuration group to which the listener belongs.

<br />

### listener.ssl.external.access.1

| Type    | Default     |
| ------- | ----------- |
| string  | `allow all` |

##### Description

List of ACL rules of the listener. It is used to set the white/black list of the connection layer.

For example:

`allow all`：Allow all TCP connections.
`allow 192.168.0.0/24`：Allow TCP connection with network address `192.168.0.0/24` to access.

At the same time, the configuration can configure multiple rules:

```
listener.ssl.external.access.1 = deny 192.168.0.1
listener.ssl.external.access.2 = allow all
```

<br />

### listener.ssl.external.rate_limit

| Type    | Default |
| ------- | ------- |
| string  | -       |

##### Description

Listener rate limit, with the format of `<limit>,<duration>`.

<br />

### listener.ssl.external.proxy_protocol

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | -       |

##### Description

Whether the listener enables `Proxy Protocol` support.

If the EMQ X cluster is deployed behind HAProxy or Nginx, and it is required to get the client's real source IP address and port, you need to enable this configuration.

`Proxy Protcol` reference: [https://www.haproxy.com/blog/haproxy/proxy-protocol](https://www.haproxy.com/blog/haproxy/proxy-protocol).

<br />

### listener.ssl.external.proxy_protocol_timeout

| Type     | Default |
| -------- | ------- |
| duration | -       |

##### Description

Set the timeout for Proxy Protocol parsing. If no Proxy Protocol packet is received within this time, EMQ X Broker will close its connection.

<br />

### listener.ssl.external.tls_versions

| Type   | Default                 |
| ------ | ----------------------- |
| string | `tlsv1.2,tlsv1.1,tlsv1` |

##### Description

Specify the SSL version list supported by the server. For details, see [http://erlang.org/doc/man/ssl.html](http://erlang.org/doc/man/ssl.html).

<br />

### listener.ssl.external.handshake_timeout

| Type     | Default |
| -------- | ------- |
| duration | `15s`   |

##### Description

Specify the timeout period for the SSL handshake process.

<br />

### listener.ssl.external.keyfile

| Type   | Default             |
| ------ | ------------------- |
| string | `etc/certs/key.pem` |

##### Description

Specify SSL private key file (PEM).

<br />

### listener.ssl.external.certfile

| Type   | Default              |
| ------ | -------------------- |
| string | `etc/certs/cert.pem` |

##### Description

Specify SSL certificate file(PEM).

<br />

### listener.ssl.external.cacertfile

| Type   | Default                |
| ------ | ---------------------- |
| string | `etc/certs/cacert.pem` |

##### Description

Specify the CA certificate file for SSL (PEM).

<br />

### listener.ssl.external.dhfile

| Type   | Default                   |
| ------ | ------------------------- |
| string | `etc/certs/dh-params.pem` |

##### Description

If using the Ephemeral Diffie-Hellman algorithm, specify the key file used by the algorithm.

<br />

### listener.ssl.external.verify

| Type | Optional Value               | Default       |
| ---- | ---------------------------- | ------------- |
| enum | `verify_peer`, `verify_none` | `verify_peer` |

##### Description

Specifies whether to verify the client during the handshake.

<br />

### listener.ssl.external.fail_if_no_peer_cert

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### Description

If the client does not have a certificate during the SSL handshake, it determines whether to let the handshake fail.

<br />

### listener.ssl.external.ciphers

| Type   | Default |
| ------ | ------- |
| string | `ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384,ECDHE-ECDSA-AES256-SHA384,ECDHE-RSA-AES256-SHA384,ECDHE-ECDSA-DES-CBC3-SHA,ECDH-ECDSA-AES256-GCM-SHA384,ECDH-RSA-AES256-GCM-SHA384,ECDH-ECDSA-AES256-SHA384,ECDH-RSA-AES256-SHA384,DHE-DSS-AES256-GCM-SHA384,DHE-DSS-AES256-SHA256,AES256-GCM-SHA384,AES256-SHA256,ECDHE-ECDSA-AES128-GCM-SHA256,ECDHE-RSA-AES128-GCM-SHA256,ECDHE-ECDSA-AES128-SHA256,ECDHE-RSA-AES128-SHA256,ECDH-ECDSA-AES128-GCM-SHA256,ECDH-RSA-AES128-GCM-SHA256,ECDH-ECDSA-AES128-SHA256,ECDH-RSA-AES128-SHA256,DHE-DSS-AES128-GCM-SHA256,DHE-DSS-AES128-SHA256,AES128-GCM-SHA256,AES128-SHA256,ECDHE-ECDSA-AES256-SHA,ECDHE-RSA-AES256-SHA,DHE-DSS-AES256-SHA,ECDH-ECDSA-AES256-SHA,ECDH-RSA-AES256-SHA,AES256-SHA,ECDHE-ECDSA-AES128-SHA,ECDHE-RSA-AES128-SHA,DHE-DSS-AES128-SHA,ECDH-ECDSA-AES128-SHA,ECDH-RSA-AES128-SHA,AES128-SHA` |

##### Description

Specify the cipher suite supported by the server.

<br />

### listener.ssl.external.psk_ciphers

| Type   | Default                                                                  |
| ------ | ------------------------------------------------------------------------ |
| string | `PSK-AES128-CBC-SHA,PSK-AES256-CBC-SHA,PSK-3DES-EDE-CBC-SHA,PSK-RC4-SHA` |

##### Description

If using the PSK algorithm, specify the PSK Cipher list supported by the server. Note that only one of 'listener.ssl.external.ciphers' and 'listener.ssl.external.psk_ciphers' can be configured.

<br />

### listener.ssl.external.secure_renegotiate

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `off`   |

##### Description

Specifies whether to reject renegotiation requests if the client does not follow RFC 5746

<br />

### listener.ssl.external.reuse_sessions

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `on`    |

##### Description

Specify whether to support SSL session reuse. For details, see[http://erlang.org/doc/man/ssl.html](http://erlang.org/doc/man/ssl.html).

<br />

### listener.ssl.external.honor_cipher_order

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `on`    |

##### Description

Specify whether to use the server's preferences to select Ciphers.

<br />

### listener.ssl.external.peer_cert_as_username

| Type | Optional Value    | Default |
| ---- | ----------------- | ------- |
| enum | `cn`, `dn`, `crt` | `cn`    |

##### Description

Use the value of the CN, DN, or CRT field in the client certificate as the value of the Username field in the MQTT CONNECT packet.
Note that `listener.ssl.external.verify` should be set to `verify_peer`.

<br />

### listener.ssl.external.backlog

| Type    | Default |
| ------- | ------- |
| integer | 1024    |

##### Description

The maximum length of the TCP connection queue. It indicates the maximum number of TCP connection queues that are allowed in the system to undergo three-time handshake.

<br />

### listener.ssl.external.send_timeout

| Type     | Default |
| -------- | ------- |
| duration | `15s`   |

##### Description

Timeout for sending TCP packets.

<br />

*`listener.ssl.external.send_timeout_close`**

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `on`    |

##### Description

Whether to close the connection after TCP packet sending timeout.

<br />

### listener.ssl.external.recbuf

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### Description

TCP receiving buffer size (operating system kernel level parameter).

Reference:[http://erlang.org/doc/man/inet.html](http://erlang.org/doc/man/inet.html).

<br />

### listener.ssl.external.sndbuf

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### Description

TCP sending buffer size (operating system kernel level parameter).

Reference:[http://erlang.org/doc/man/inet.html](http://erlang.org/doc/man/inet.html).

<br />

### listener.ssl.external.buffer

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### Description

CP buffer size (user level).

This value is recommended to be greater than or equal to the maximum value of `sndbuff` and `recbuff` to avoid some performance problems. Without configuration, it equals to the maximum value of sndbuff and recbuff by default.

Reference:[http://erlang.org/doc/man/inet.html](http://erlang.org/doc/man/inet.html).

<br />

### listener.ssl.external.tune_buffer

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | -       |

##### Description

If this configuration is enabled, please set the value equal to the maximum value of `sndbuff` and `recbuff`.

<br />

### listener.ssl.external.nodelay

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### Description

This is the `TCP_NODELAY` parameter. Enabling this option means that the Nagle algorithm is disabled and small packets will be sent immediately.

<br />

### listener.ssl.external.reuseaddr

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### Description

This is the `SO_REUSEADDR` parameter. Enabling this option allows the local port to be reused without waiting for the end of the `TIME_WAIT` state.

<br />

### listener.ws.external

| Type    | Default |
| ------- | ------- |
| string  | `8083`  |

##### Description

Configure the listening address of the MQTT/WS listener named `external`.

##### Example

`8083`: Listen s to `0.0.0.0: 8083` of IPv4.
`127.0.0.1:8083`: Listening address is `8083` port on the `127.0.0.1` network card.
`:: 1: 8083`: Listen to the `8083` port on the network card `:: 1` of IPv6 address.

<br />

### listener.ws.external.mqtt_path

| Type    | Default |
| ------- | ------- |
| string  | `/mqtt` |

##### Description

WebSocket's MQTT protocol path. So the address of EMQ X Broker's WebSocket is: `ws://<ip>:<port>/mqtt`.

<br />

### listener.ws.external.acceptors

| Type    | Default |
| ------- | ------- |
| integer | 4       |

##### Description

The size of the listener's receiving pool.

<br />

### listener.ws.external.max_connections

| Type    | Default |
| ------- | ------- |
| integer | 102400  |

##### Description

The maximum number of concurrent connections allowed by the listener.

<br />

### listener.ws.external.max_conn_rate

| Type    | Default |
| ------- | ------- |
| integer | 1000    |

##### Description

The maximum access rate allowed by the listener. Unit: pcs/sec

<br />

### listener.ws.external.active_n

| Type    | Default |
| ------- | ------- |
| integer | 100     |

##### Description

The number of times the listener continues to receive TCP packets.

<br />

### listener.ws.external.rate_limit

| Type    | Default     |
| ------- | ----------- |
| string  | `100KB,10s` |

##### Description

The rate limit of the listener. The format is `<limit>,<duration>`.

##### Example

`100KB,10s`： Limit the number of incoming bytes within 10 seconds to not exceed 100 KB.

<br />

### listener.ws.external.zone

| Type    | Default    |
| ------- | ---------- |
| string  | `external` |

##### Description

The configuration zone to which the listener belongs.

<br />

### listener.ws.external.access.1

| Type    | Default     |
| ------- | ----------- |
| string  | `allow all` |

##### Description

List of ACL rules of the listener. It is used to set the white/black list of the connection layer.

<br />

### listener.ws.external.verify_protocol_header

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `on`    |

##### Description

Whether to verify that the HTTP header carried by WebSocket is correct. **WeChat applet needs to disable this verification.**

<br />

### listener.ws.external.proxy_address_header

| Type    | Optional Value    | Default |
| ------- | ----------------- |-------- |
| string  | `X-Forwarded-For` | -       |

##### Description

If the EMQ X cluster is deployed behind HAProxy or Nginx, you can open the configuration to obtain the real IP address of the client.

<br />

### listener.ws.external.proxy_port_header

| Type    | Optional Value     | Default |
| ------- | ------------------ | ------- |
| string  | `X-Forwarded-Port` | -       |

##### Description

If the EMQ X cluster is deployed behind HAProxy or Nginx, you can open the configuration to get the real port of the client.

<br />

### listener.ws.external.proxy_protocol

| Type    | Optional Value      | Default |
| ------- | ------------------- | ------- |
| enum    | `on`, `off`         | -       |

##### Description

Whether the listener enables `Proxy Protocol` support.

If the EMQ X cluster is deployed behind HAProxy or Nginx, and you need to get the client's real source IP address and port, you need to open this configuration.

`Proxy Protcol` reference: [https://www.haproxy.com/blog/haproxy/proxy-protocol](https://www.haproxy.com/blog/haproxy/proxy-protocol).

<br />

### listener.ws.external.proxy_protocol_timeout

| Type     | Default |
| -------- | ------- |
| duration | -       |

##### Description

Set the timeout for Proxy Protocol parsing. If no Proxy Protocol packet is received within this time, EMQ X Broker will close its connection.

<br />

### listener.ws.external.backlog

| Type    | Default |
| ------- | ------- |
| integer | 1024    |

##### Description

The maximum length of the TCP connection queue. It indicates the maximum number of TCP connection queues that are allowed in the system to undergo three-time handshake.

<br />

### listener.ws.external.send_timeout

| Type     | Default |
| -------- | ------- |
| duration | `15s`   |

##### Description

Timeout for sending TCP packets.

<br />

### listener.ws.external.send_timeout_close

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `on`    |

##### Description

Whether to close the connection after TCP packet sending timeout.

<br />

### listener.ws.external.recbuf

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### Description

TCP receiving buffer size (operating system kernel level parameter)

<br />

### listener.ws.external.sndbuf

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### Description

TCP sending buffer size (operating system kernel level parameter)

<br />

### listener.ws.external.buffer

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### Description

TCP buffer size (user level).

<br />

### listener.ws.external.tune_buffer

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | -       |

##### Description

If this configuration is enabled, please set the value equal to the maximum value of `sndbuff` and `recbuff`.

<br />

### listener.ws.external.nodelay

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### Description

This is the `TCP_NODELAY` parameter. Enabling this option allows small TCP data packets to be sent immediately.

<br />

### listener.ws.external.compress

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | -       |

##### Description

Whether to compress WebSocket messages. The implementation of compression depends on [zlib](http://www.zlib.net).

The configuration items under `defalte_opts` belong to the compression-related parameter configuration, if not necessary, please do not modify it.

<br />

### listener.ws.external.deflate_opts.level

| Type    | Optional Value                                      | Default |
| ------- | --------------------------------------------------- | ------- |
| enum    | `none`, `default`, `best_compression`, `best_speed` | -       |

##### Description

compression level

<br />

### listener.ws.external.deflate_opts.mem_level

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| integer | 1 - 9          | -       |

##### Description

Compression parameters. It means memory usage limit level, and configure how much memory can be opened to participate in the compression process.

`1`: The least memory, but will reduce the compression rate.
`9`: The most memory, and will increase the calculation speed and compression rate.

If not configured, the default is `8`.

<br />

### listener.ws.external.deflate_opts.strategy

| Type    | Optional Value                                | Default |
| ------- | --------------------------------------------- | ------- |
| enum    | `default`, `filtered`, `huffman_only`, `rle`  | -       |

##### Description

Compression strategy for tuning compression ratio:

- `default`: for ordinary data.
- `filtered`: data generated by filters or predictors, suitable for content with strong randomness.
- `huffman_only`: Mandatory use of Huffman algorithm. Better than `filtered`.
- `rle`: limit the matching distance to 1 (Run-Lenght Encoding), faster than `huffman_only`, but mainly used for PNG images.

These strategies only affect the compression ratio and will not have any impact on correctness.

<br />

### listener.ws.external.deflate_opts.server_context_takeover

| Type    | Optional Value            | Default |
| ------- | ------------------------- | ------- |
| enum    | `takeover`, `no_takeover` | -       |

##### Description

Whether to allow the server's compression context to be passed between frames.

<br />

### listener.ws.external.deflate_opts.client_context_takeover

| Type    | Optional Value            | Default |
| ------- | ------------------------- | ------- |
| enum    | `takeover`, `no_takeover` | -       |

##### Description

Whether to allow the client's compression context to be passed between frames.

<br />

### listener.ws.external.deflate_opts.server_max_window_bits

| Type    | Optional Value  | Default |
| ------- | --------------- | ------- |
| integer | 8 - 15          | -       |

##### Description

Maximum window value on the server side. Setting a larger value will result in better compression ratio, but will consume additional memory.

<br />

### listener.ws.external.deflate_opts.client_max_window_bits

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| integer | 8 - 15         | -       |

##### Description

Client maximum window value. Setting a larger value will result in better compression ratio, but will consume additional memory.

<br />

### listener.ws.external.idle_timeout

| Type     | Default |
| -------- | ------- |
| duration | -       |

##### Description

The daze time after the TCP connection is established. If no packets are received within this time, the connection will be closed.

<br />

### listener.ws.external.max_frame_size

| Type    | Default |
| ------- | ------- |
| integer | -       |

##### Description

The maximum allowed length of a single MQTT packet.

<br />

### listener.wss.external

| Type    | Default        |
| ------- | -------------- |
| string  | `0.0.0.0:8084` |

##### Description

Configure a WSS (MQTT/WebSocket/SSL) listener named `external`.

<br />

### listener.wss.external.mqtt_path

| Type    | Default |
| ------- | ------- |
| string  | `/mqtt` |

##### Description

WebSocket URL Path.

<br />

### listener.wss.external.acceptors

| Type    | Default |
| ------- | ------- |
| integer | 4       |

##### Description

The size of the listener's receiving pool.

<br />

### listener.wss.external.max_connections

| Type    | Default |
| ------- | ------- |
| integer | 16      |

##### Description

The maximum number of concurrent connections allowed by the listener.

<br />

### listener.wss.external.max_conn_rate

| Type    | Default |
| ------- | ------- |
| integer | 1000    |

##### Description

The maximum access rate allowed by the listener. Unit: pcs/sec.

<br />

### listener.wss.external.active_n

| Type    | Default |
| ------- | ------- |
| integer | 100     |

##### Description

The number of times the listener continues to receive TCP packets.

<br />

### listener.wss.external.rate_limit

| Type    | Default |
| ------- | ------- |
| string  | -       |

##### Description

The rate limit of the listener. The format is `<limit>,<duration>`.

<br />

### listener.wss.external.zone

| Type    | Default    |
| ------- | ---------- |
| string  | `external` |

##### Description

The configuration group to which the listener belongs.

<br />

### listener.wss.external.access.1

| Type    | Default     |
| ------- | ----------- |
| string  | `allow all` |

##### Description

List of ACL rules of the listener. It is used to set the white/black list of the connection layer.

E.g:

`allow all`: Allow all TCP connections.
`allow 192.168.0.0/24`: Allow TCP connections with a network address of `192.168.0.0 / 24` to access.

At the same time, the configuration can configure multiple rules:

```
listener.wss.external.access.1 = deny 192.168.0.1
listener.wss.external.access.2 = allow all
```

<br />

### listener.wss.external.verify_protocol_header

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `on`    |

##### Description

Whether to verify that the HTTP header carried by WebSocket is correct. **WeChat applet needs to disable this verification.**

<br />

### listener.wss.external.proxy_address_header

| Type   | Default           |
| ------ | ----------------- |
| string | `X-Forwarded-For` |

##### Description

If the EMQ X cluster is deployed in HAProxy or Nginx, you can open the configuration to obtain the real IP address of the client.

<br />

### listener.wss.external.proxy_protocol

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | -       |

##### Description

Whether the listener enables `Proxy Protocol` support.

If the EMQ X cluster is deployed behind HAProxy or Nginx, and you need to get the client's real source IP address and port, you need to open this configuration.

`Proxy Protcol` reference:[https://www.haproxy.com/blog/haproxy/proxy-protocol](https://www.haproxy.com/blog/haproxy/proxy-protocol).

<br />

### listener.wss.external.proxy_protocol_timeout

| Type     | Default |
| -------- | ------- |
| duration | -       |

##### Description

Set the timeout for Proxy Protocol parsing. If no Proxy Protocol packet is received within this time, EMQ X Broker will close its connection.

<br />

### listener.wss.external.tls_versions

| Type   | Default                |
| ------ | ----------------------- |
| string | `tlsv1.2,tlsv1.1,tlsv1` |

##### Description

Specify the SSL version list supported by the server. For details, see [http://erlang.org/doc/man/ssl.html](http://erlang.org/doc/man/ssl.html).

<br />

### listener.wss.external.keyfile

| Type   | Default             |
| ------ | ------------------- |
| string | `etc/certs/key.pem` |

##### Description

Specify SSL private key file (PEM).

<br />

### listener.wss.external.certfile

| Type   | Default              |
| ------ | -------------------- |
| string | `etc/certs/cert.pem` |

##### Description

Specify SSL certificate file(PEM).

<br />

### listener.wss.external.cacertfile

| Type   | Default                |
| ------ | ---------------------- |
| string | `etc/certs/cacert.pem` |

##### Description

If using SSL, specify the CA certificate file for SSL (PEM).

<br />

### listener.wss.external.dhfile

| Type   | Default                   |
| ------ | ------------------------- |
| string | `etc/certs/dh-params.pem` |

##### Description

If using the Ephemeral Diffie-Hellman algorithm, specify the key file used by the algorithm.

<br />

### listener.wss.external.verify

| Type | Optional Value               | Default       |
| ---- | ---------------------------  | ------------- |
| enum | `verify_peer`, `verify_none` | `verify_peer` |

##### Description

Specifies whether to verify the client during the handshake.

<br />

### listener.wss.external.fail_if_no_peer_cert

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### Description

If the client does not have a certificate during the SSL handshake, it determines whether to let the handshake fail.

<br />

### listener.wss.external.ciphers

| Type   | Default |
| ------ | ------- |
| string | `ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384,ECDHE-ECDSA-AES256-SHA384,ECDHE-RSA-AES256-SHA384,ECDHE-ECDSA-DES-CBC3-SHA,ECDH-ECDSA-AES256-GCM-SHA384,ECDH-RSA-AES256-GCM-SHA384,ECDH-ECDSA-AES256-SHA384,ECDH-RSA-AES256-SHA384,DHE-DSS-AES256-GCM-SHA384,DHE-DSS-AES256-SHA256,AES256-GCM-SHA384,AES256-SHA256,ECDHE-ECDSA-AES128-GCM-SHA256,ECDHE-RSA-AES128-GCM-SHA256,ECDHE-ECDSA-AES128-SHA256,ECDHE-RSA-AES128-SHA256,ECDH-ECDSA-AES128-GCM-SHA256,ECDH-RSA-AES128-GCM-SHA256,ECDH-ECDSA-AES128-SHA256,ECDH-RSA-AES128-SHA256,DHE-DSS-AES128-GCM-SHA256,DHE-DSS-AES128-SHA256,AES128-GCM-SHA256,AES128-SHA256,ECDHE-ECDSA-AES256-SHA,ECDHE-RSA-AES256-SHA,DHE-DSS-AES256-SHA,ECDH-ECDSA-AES256-SHA,ECDH-RSA-AES256-SHA,AES256-SHA,ECDHE-ECDSA-AES128-SHA,ECDHE-RSA-AES128-SHA,DHE-DSS-AES128-SHA,ECDH-ECDSA-AES128-SHA,ECDH-RSA-AES128-SHA,AES128-SHA` |

##### Description

Specifies the cipher suite supported by the server.

<br />

### listener.wss.external.psk_ciphers

| Type   | Default                                                                  |
| ------ | ------------------------------------------------------------------------ |
| string | `PSK-AES128-CBC-SHA,PSK-AES256-CBC-SHA,PSK-3DES-EDE-CBC-SHA,PSK-RC4-SHA` |

##### Description

If using the PSK algorithm, specify the PSK Cipher list supported by the server. Note that only one of 'listener.wss.external.ciphers' and 'listener.wss.external.psk_ciphers' can be configured.

<br />

### listener.wss.external.secure_renegotiate

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `off`   |

##### Description

Specifies whether to reject renegotiation requests if the client does not follow RFC 5746

<br />

### listener.wss.external.reuse_sessions

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `on`    |

##### Description

Specifies whether to support SSL session reuse. For details, see [http://erlang.org/doc/man/ssl.html](http://erlang.org/doc/man/ssl.html).

<br />

### listener.wss.external.honor_cipher_order

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `on`    |

##### Description

Specify whether to use the server's preferences to select Ciphers.

<br />

### listener.wss.external.peer_cert_as_username

| Type | Optional Value    | Default |
| ---- | ----------------- | ------- |
| enum | `cn`, `dn`, `crt` | `cn`    |

##### Description

Use the value of the CN, DN, or CRT field in the client certificate as the value of the Username field in the MQTT CONNECT packet.
Note that `listener.wss.external.verify` should be set to `verify_peer`.

<br />

### listener.wss.external.backlog

| Type    | Default |
| ------- | ------- |
| integer | 1024    |

##### Description

The maximum length of the TCP connection queue. It indicates the maximum number of TCP connection queues that are allowed in the system to undergo three-time handshake.

<br />

### listener.wss.external.send_timeout

| Type     | Default |
| -------- | ------- |
| duration | `15s`   |

##### Description

Timeout for sending TCP packets.

<br />

*`listener.wss.external.send_timeout_close`**

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `on`    |

##### Description

Whether to close the connection after TCP packet sending timeout.

<br />

### listener.wss.external.recbuf

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### Description

TCP receiving buffer size (operating system kernel level parameter)

Reference:http://erlang.org/doc/man/inet.html

<br />

### listener.wss.external.sndbuf

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### Description

TCP sending buffer size (operating system kernel level parameter)

Reference:http://erlang.org/doc/man/inet.html

<br />

### listener.wss.external.buffer

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### Description

TCP buffer size (user level).

This value is recommended to be greater than or equal to the maximum value of `sndbuff` and `recbuff` to avoid some performance problems. Without configuration, it equals to the maximum value of sndbuff and recbuff by default.

Reference:http://erlang.org/doc/man/inet.html

<br />

### listener.wss.external.tune_buffer

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | -       |

##### Description

If you open this configuration, please set the value equal to the maximum value of `sndbuff` and `recbuff`.

<br />

### listener.wss.external.nodelay

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### Description

This is the `TCP_NODELAY` parameter. Enabling this option allows small TCP data packets to be sent immediately.

<br />

### listener.wss.external.compress

| Type    | Optional Value  | Default |
| ------- | --------------- | ------- |
| enum    | `true`, `false` | `false` |

##### Description

If this option is set to true, Websocket messages will be compressed.

<br />

### listener.wss.external.deflate_opts.level

| Type    | Optional Value                                      | Default   |
| ------- | --------------------------------------------------- | --------- |
| enum    | `none`, `default`, `best_compression`, `best_speed` | `default` |

##### Description

Compression level.

<br />

### listener.wss.external.deflate_opts.mem_level

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| integer | 1 - 9          | -       |

##### Description

Compression parameters. It means memory usage limit level, configures how much memory can be opened to participate in the compression process.

`1`: The least memory, but will reduce the compression rate.
`9`: The most memory, and will increase the calculation speed and compression rate.

If not configured, the default is `8`.

<br />

### listener.wss.external.deflate_opts.strategy

| Type    | Optional Value                                | Default |
| ------- | --------------------------------------------- | ------- |
| enum    | `default`, `filtered`, `huffman_only`, `rle`  | -       |

##### Description

Compression strategy for tuning compression ratio:

- `default`: for ordinary data.
- `filtered`: data generated by filters or predictors, suitable for content with strong randomness.
- `huffman_only`: Mandatory use of Huffman algorithm. Better than `filtered`.
- `rle`: limit the matching distance to 1 (Run-Lenght Encoding), faster than `huffman_only`, but mainly used for PNG images.

These strategies only affect the compression ratio and will not have any impact on correctness.

<br />

### listener.wss.external.deflate_opts.server_context_takeover

| Type    | Optional Value            | Default |
| ------- | ------------------------- | ------- |
| enum    | `takeover`, `no_takeover` | -       |

##### Description

Whether to allow the server's compression context to be passed between frames.

<br />

### listener.wss.external.deflate_opts.client_context_takeover

| Type    | Optional Value            | Default |
| ------- | ------------------------- | ------- |
| enum    | `takeover`, `no_takeover` | -       |

##### Description

Whether to allow the client's compression context to be passed between frames.

<br />

### listener.wss.external.deflate_opts.server_max_window_bits

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| integer | 8 - 15         | -       |

##### Description

Maximum window value on the server side. Setting a larger value will result in better compression ratio, but will consume additional memory.

<br />

### listener.wss.external.deflate_opts.client_max_window_bits

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| integer | 8 - 15         | -       |

##### Description

Client maximum window value. Setting a larger value will result in better compression ratio, but will consume additional memory.

<br />

### listener.wss.external.idle_timeout

| Type     | Default |
| -------- | ------- |
| duration | -       |

##### Description

The daze time after the TCP connection is established. If no packets are received within this time, the connection will be closed.

<br />

### listener.wss.external.max_frame_size

| Type    | Default |
| ------- | ------- |
| integer | -       |

##### Description

The maximum length of a single MQTT packet.

<br />

### plugins.etc_dir

| Type    | Default       |
| ------- | ------------- |
| string  | `etc/plugins` |

##### Description

The configuration directory of the plugin.

<br />

### plugins.loaded_file

| Type    | Default              |
| ------- | -------------------- |
| string  | `etc/loaded_plugins` |

##### Description

The configuration file path of the plugin startup list.

<br />

### plugins.expand_plugins_dir

| Type    | Default    |
| ------- | ---------- |
| string  | `plugins/` |

##### Description

External plugin storage directory.

<br />

### broker.sys_interval

| Type      | Default |
| --------- | ------- |
| duration  | `1m`    |

##### Description

Set the system topic (`$SYS`) message release interval.

<br />

### broker.sys_heartbeat

| Type     | Default |
| -------- | ------- |
| duration | `30s`   |

##### Description

Set the system heartbeat message release interval. The system heartbeat message includes the following two topics:

- "$SYS/brokers/\<node>/uptime"
- "$SYS/brokers/\<node>/datetime"

<br />

### broker.enable_session_registry

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `on`    |

##### Description

Enable or disable global session registration.

<br />

### broker.session_locking_strategy

| Type | Optional Value                  | Default  |
| ---- | ------------------------------- | -------- |
| enum | `local`, `one`, `quorum`, `all` | `quorum` |

##### Description

Set the type of session cluster lock. The session cluster lock is used to prevent the same client from creating multiple sessions on multiple different nodes, which is common when clients frequently switch between nodes for logging.

<br />

### broker.shared_subscription_strategy

| Type | Optional Value                            | Default  |
| ---- | ----------------------------------------- | -------- |
| enum | `random`, `round_robin`, `sticky`, `hash` | `random` |

##### Description

Set a distribution strategy for shared subscriptions. Optional values are:

- **random**: Choose randomly among all subscribers
- **round_robin**: According to the order of subscription
- **sticky**: Always sent to the last selected subscriber
- **hash**: According to the hash value of the publisher ClientID

<br />

### broker.shared_dispatch_ack_enabled

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### Description

Enable or disable the ACK check function for qos1/qos2 messages in shared subscriptions. After enabling, if it is delivered to a subscriber but fails to receive the ACK, it will try to deliver to the next subscriber in the subscription group.

<br />

### broker.route_batch_clean

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `off`   |

##### Description

Enable or disable batch cleanup routing information. Batch cleanup routing can be used in a short period of time when a large number of clients go offline to improve cleanup efficiency.

<br />

### sysmon.long_gc

| Type     | Default |
| -------- | ------- |
| duration | `0ms`   |

##### Description

Enable garbage collection time monitoring and trigger an alarm when the collection time exceeds the set value, 0 means disabling this monitoring.

<br />

### sysmon.long_schedule

| Type     | Default |
| -------- | ------- |
| duration | `240ms` |

##### Description

Enable process scheduling time monitoring and trigger an alarm when the scheduling time exceeds the set value, 0 means disabling this monitoring.

<br />

### sysmon.large_heap

| Type     | Default |
| -------- | ------- |
| bytesize | `8MB`   |

##### Description

Enable stack size monitoring and trigger an alarm when the stack size is still greater than the set value after the process performs garbage collection. 0 means disabling this monitoring.

<br />

### sysmon.busy_port

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### Description

Specifies whether to enable inter-process message channel busy monitoring.

<br />

### sysmon.busy_dist_port

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### Description

Specifies whether to enable cluster RPC channel busy monitoring.

<br />

### os_mon.cpu_check_interval

| Type     | Default |
| -------- | ------- |
| duration | `60s`   |

##### Description

CPU usage rate check cycle.

<br />

### os_mon.cpu_high_watermark

| Type    | Default |
| ------- | ------- |
| percent | `80%`   |

##### Description

An alarm will be triggered when the CPU usage exceeds `os_mon.cpu_high_watermark`.

<br />

### os_mon.cpu_low_watermark

| Type    | Default |
| ------- | ------- |
| percent | `60%`   |

##### Description

The alarm will be cleared when the CPU usage drops back below `os_mon.cpu_low_watermark` .

<br />

### os_mon.mem_check_interval

| Type     | Default |
| -------- | ------- |
| duration | `60s`   |

##### Description

Memory usage check cycle.

<br />

### os_mon.sysmem_high_watermark

| Type    | Default |
| ------- | ------- |
| percent | `70%`   |

##### Description

When the memory allocated by EMQ X Broker for all processes as a percentage of system memory exceeds `os_mon.procmem_high_watermark`, an alarm will be triggered.

<br />

### os_mon.procmem_high_watermark

| Type    | Default |
| ------- | ------- |
| percent | `5%`    |

##### Description

When the memory allocated by EMQ X Broker for a single process as a percentage of system memory exceeds `os_mon.procmem_high_watermark`, an alarm will be triggered.

<br />

### vm_mon.check_interval

| Type     | Default |
| -------- | ------- |
| duration | `30s`   |

##### Description

Check interval for process number.

<br />

### vm_mon.process_high_watermark

| Type    | Default |
| ------- | ------- |
| percent | `80%`   |

##### Description

When the current process number as a percentage of the maximum process number exceeds `vm_mon.process_high_watermark`, an alarm will be triggered. The maximum process number is determined by the `node.process_limit` configuration item.

<br />

### vm_mon.process_low_watermark

| Type    | Default |
| ------- | ------- |
| percent | `60%`   |

##### Description

When the percentage of the current number of processes in the maximum number of processes falls below `vm_mon.process_low_watermark`, an alarm will be triggered. The maximum number of processes is determined by the `node.process_limit` configuration item.

<br />

## [emqx-auth-clientid](https://github.com/emqx/emqx-auth-clientid)

### auth.client.\<Number>.clientid` & `auth.client.\<Number>.password

| Type   | Default |
| ------ | ------- |
| string | -       |

##### Description

The authentication data of the client, where  `auth.client.<Number>.password` is the clear text password. `auth.client.<Number>.clientid` and `auth.client.<Number>.password` for the same `<Number>`  must appear in pairs. `<Number> `is an integer number used to distinguish authentication data of multiple clients.

<br />

### auth.client.password_hash

| Type | Optional Value                  | Default  |
| ---- | ------------------------------- | -------- |
| enum | `plain`, `md5`, `sha`, `sha256` | `sha256` |

##### Description

Hash algorithm is used when the password is stored in the database. The following options are available:

`plain`

The password is stored in clear text.

`md5`

The password is encrypted and stored using the MD5 algorithm.

`sha`

The password is encrypted and stored using the SHA-1 algorithm.

`sha256`

The password is encrypted and stored using the SHA-256 algorithm.

<br />

## [emqx-auth-http](https://github.com/emqx/emqx-auth-http)

### auth.http.auth_req

| Type   | Default                           |
| ------ | --------------------------------- |
| string | `http://127.0.0.1:8991/mqtt/auth` |

##### Description

Specify the target URL of the authentication request.

<br />

### auth.http.auth_req.method

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `get`, `post`  | `post`  |

##### Description

Specify the request method of the authentication request.

<br />

### auth.http.auth_req.params

| Type   | Format                                                       | Default                               |
| ------ | ------------------------------------------------------------ | ------------------------------------- |
| string | `K=v` key-value pairs separated by`, `,` v` can be fixed content or placeholder | `clientid=%c,username=%u,password=%P` |

##### Description

Specify the data carried in the authentication request. When using the GET method, the value of `auth.http.auth_req.params` will be converted into `k=v` key-value pairs separated by `&` and sent as query string parameters. When using the POST method, the value of `auth.http.auth_req.params` will be converted into `k=v` key-value pairs separated by `&` and sent in the form of Request Body. All placeholders will be replaced by run-time data , and the available placeholders are as follows:

| Placeholder | Replace content |
| ------ | -------------------- |
| `%u`   | Username |
| `%c`   | MQTT Client ID       |
| `%a`   | Client's network IP address |
| `%r`   | The protocol used by the client can be:`mqtt`, `mqtt-sn`, `coap`, `lwm2m` and `stomp` |
| `%P`   | Password |
| `%p`   | Server port for client connection |
| `%c`   | Common Name in client certificate |
| `%d`   | Subject in client certificate |

<br />

### auth.http.super_req

| Type   | Default                                |
| ------ | -------------------------------------- |
| string | `http://127.0.0.1:8991/mqtt/superuser` |

##### Description

Specify the target URL for the superuser authentication request.

### auth.http.super_req.method

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `get`, `post`  | `post`  |

##### Description

Specifies the request method of the super user authentication request.

### auth.http.super_req.params

| Type   | Format                                                       | Default                   |
| ------ | ------------------------------------------------------------ | ------------------------- |
| string | `K=v` key-value pairs separated by`, `,` v` can be fixed content or placeholder | `clientid=%c,username=%u` |

##### Description

Specify the data carried in the authentication request. When using the GET method, the value of `auth.http.auth_req.params` will be converted into `k=v` key-value pairs separated by `&` and sent as query string parameters. When using the POST method, the value of `auth.http.auth_req.params` will be converted into `k=v` key-value pairs separated by `&` and sent in the form of Request Body. All placeholders will be replaced by run-time data , and the available placeholders are the same as those of `auth.http.auth_req.params`.

<br />

### auth.http.acl_req

| Type   | Default                          |
| ------ | -------------------------------- |
| string | `http://127.0.0.1:8991/mqtt/acl` |

##### Description

Specify the target URL for ACL verification requests.

<br />

### auth.http.acl_req.method

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `get`, `post`  | `post`  |

##### Description

Specifies the request method for ACL verification requests.

<br />

### auth.http.acl_req.params

| Type   | Format                                                       | Default                                                      |
| ------ | ------------------------------------------------------------ | ------------------------------------------------------------ |
| string | `K=v` key-value pairs separated by`, `,` v` can be fixed content or placeholder | `access=%A,username=%u,clientid=%c,ipaddr=%a,topic=%t,mountpoint=%m` |

##### Description

Specify the data carried in the authentication request. When using the GET method, the value of `auth.http.auth_req.params` will be converted into `k=v` key-value pairs separated by `&` and sent as query string parameters. When using the POST method, the value of `auth.http.auth_req.params` will be converted into `k=v` key-value pairs separated by `&` and sent in the form of Request Body. All placeholders will be replaced by run-time data , and the available placeholders are as follows:

| Placeholder | Replace content                                              |
| ----------- | ------------------------------------------------------------ |
| `%A`        | Permission to be verified, 1 means subscription, 2 means publish |
| `%u`        | MQTT Client ID                                               |
| `%c`        | Client identifier                                            |
| `%a`        | Client network IP address                                    |
| `%r`        | The protocol used by the client can be: `mqtt`, `mqtt-sn`, `coap`, `lwm2m` and `stomp` |
| `%m`        | Mount point                                                  |
| `%t`        | Topic                                                        |

<br />

### auth.http.request.timeout

| Type     | Default |
| -------- | ------- |
| duration | `0s`    |

##### Description

HTTP request timeout. Any setting equivalent to `0s` means never timeout.

<br />

### auth.http.request.connect_timeout

| Type     | Default |
| -------- | ------- |
| duration | `0s`    |

##### Description

Connection timeout for HTTP requests. Any setting value equivalent to `0s` means never time out.

<br />

### auth.http.request.retry_times

| Type    | Default |
| ------- | ------- |
| integer | 3       |

##### Description

The number of retries when an HTTP request fails.

<br />

### auth.http.request.retry_interval

| Type     | Default |
| -------- | ------- |
| duration | `1s`    |

##### Description

Retry interval when HTTP request fails.

<br />

### auth.http.request.retry_backoff

| Type  | Default |
| ----- | ------- |
| float | 2.0     |

##### Description

When the HTTP request fails, the retry interval uses the exponential backoff algorithm. This configuration item is used to specify the backoff coefficient of the exponential backoff algorithm.

<br />

### auth.http.header.\<Key>

| Type   | Default |
| ------ | ------- |
| string | -       |

##### Description

Specify the data in the HTTP request header. `<Key>` Specify the field name in the HTTP request header, and the value of this configuration item is the corresponding field value. `<Key>` can be the standard HTTP request header field. User can also customize the field to configure multiple different request header fields.

##### Example

```
auth.http.header.Accept = */*
auth.http.header.Accept-Encoding = *
```

<br />

### auth.http.ssl.cacertfile

| Type   | Default            |
| ------ | ------------------ |
| string | `etc/certs/ca.pem` |

##### Description

CA certificate file path.

<br />

### auth.http.ssl.certfile

| Type   | Default                     |
| ------ | --------------------------- |
| string | `etc/certs/client-cert.pem` |

##### Description

Client certificate file path.

<br />

### auth.http.ssl.keyfile

| Type   | Default                    |
| ------ | -------------------------- |
| string | `etc/certs/client.key.pem` |

##### Description

Client private key file path.

<br />

## [emqx-auth-jwt](https://github.com/emqx/emqx-auth-jwt)

### auth.jwt.secret

| Type    | Default      |
| ------- | ------------ |
| string  | `emqxsecret` |

##### Description

Set HMAC Secret.

<br />

### auth.jwt.from

| Type | Optional Value         | Default    |
| ---- | ---------------------- | ---------- |
| enum | `username`, `password` | `password` |

##### Description

Where to get JWT. Optional values are

- username: The username field of the MQTT CONNECT packet is used as JWT.
- password: The password field of the MQTT CONNECT packet is used as JWT.

<br />

### auth.jwt.pubkey

| Type    | Default                        |
| ------- | ------------------------------ |
| string  | `etc/certs/jwt_public_key.pem` |

##### Description

If you use RSA or ECDSA encryption algorithm, you must specify the private key file.

<br />

### auth.jwt.verify_claims

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `off`   |

##### Description

Enable or disable Claims verification.

<br />

### auth.jwt.verify_claims.\<claims>

| Type   | Default |
| ------ | ------- |
| string | -       |

##### Description

When the Claims verification function is enabled, you can set optional values for fields in the JWT.

For example, if the value of `sub` in the Claim in JWT is expected to be" abc ", the following rules can be configured:

```
auth.jwt.verify_claims.sub = abc
```

The expected value supports two wildcards:

- `%u`: username
- `%c`: clientid

For example, if the value of the `sub` field in the JWT is expected to be the same as the username field in the MQTT CONNECT message, the following rules can be configured:

```
auth.jwt.verify_claims.sub = %u
```

<br />

## [emqx-auth-ldap](https://github.com/emqx/emqx-auth-ldap)

### auth.ldap.servers

| Type     | Default     |
| -------- | ----------- |
| string   | `127.0.0.1` |

##### Description

LDAP service address.

<br />

### auth.ldap.port

| Type     | Default |
| -------- | ------- |
| integer  | 389     |

##### Description

LDAP service port.

<br />

### auth.ldap.pool

| Type     | Optional Value | Default |
| -------- | -------------- | ------- |
| integer  | > 0            | 8       |

##### Description

Connection pool size.

<br />

### auth.ldap.bind_dn

| Type     | Default                 |
| -------- | ----------------------- |
| string   | `cn=root,dc=emqx,dc=io` |

##### Description

The DN for logging into the LDAP service.

<br />

### auth.ldap.bind_password

| Type     | Default  |
| -------- | -------- |
| string   | `public` |

##### Description

The password for logging into the LDAP service.

<br />

### auth.ldap.timeout

| Type     | Default |
| -------- | ------- |
| duration | `30s`   |

##### Description

The query timeout.

<br />

### auth.ldap.device_dn

| Type     | Default                    |
| -------- | -------------------------- |
| string   | `ou=device,dc=emqx,dc=io`  |

##### Description

The DN to which the client belongs.

<br />

### auth.ldap.match_objectclass

| Type     | Default     |
| -------- | ----------- |
| string   | `mqttUser`  |

##### Description

The name of the client object.

<br />

### auth.ldap.username.attributetype

| Type     | Default |
| -------- | ------- |
| string   | `uid`   |

##### Description

The data type of the Username attribute.

<br />

### auth.ldap.password.attributetype

| Type     | Default          |
| -------- | ---------------- |
| string   | `userPassword`   |

##### Description

The data type of the Password attribute.

<br />

### auth.ldap.ssl

| Type     | Optional Value   | Default |
| -------- | ---------------- | ------- |
| enum     | `true`, `false`  | `false` |

##### Description

Whether to enable SSL.

<br />

### auth.ldap.ssl.certfile

| Type     | Default |
| -------- | ------- |
| string   | -       |

##### Description

SSL server certificate path.

<br />

### auth.ldap.ssl.keyfile

| Type     | Default |
| -------- | ------- |
| string   | -       |

##### Description

SSL server key file path.

<br />

### auth.ldap.ssl.cacertfile

| Type     | Default |
| -------- | ------- |
| string   | -       |

##### Description

CA certificate file path.

<br />

### auth.ldap.ssl.verify

| Type     | Optional Value                | Default |
| -------- | ----------------------------- | ------- |
| enum     | `verify_peer`, `verify_none`  | -       |

##### Description

SSL authentication method:

- `verify_none`：One-way authentication.
- `verify_peer`：Two-way authentication.

<br />

### auth.ldap.ssl.fail_if_no_peer_cert

| Type     | Optional Value   | Default |
| -------- | ---------------- | ------- |
| enum     | `true`, `false`  | `false` |

##### Description

If the client does not provide an SSL certificate, disconnect it.

<br />

## [emqx-auth-mongo](https://github.com/emqx/emqx-auth-mongo)

### auth.mongo.type

| Type | Optional Value                      | Default  |
| ---- | ----------------------------------- | -------- |
| enum | `single`, `unknown`, `sharded`, `rs`| `single` |

##### Description

Set the topology type of MongoDB:

- single: single node
- unknown: unknown
- sharded: sharding mode
- rs: replicated set

<br />

### auth.mongo.rs_set_name

| Type   | Default |
| ------ | ------- |
| string | -       |

##### Description

In the case of using rs mode, set the name of rs.

<br />

### auth.mongo.rs_set_name

| Type   | Default           |
| ------ | ----------------- |
| string | `127.0.0.1:27017` |

##### Description

Set the address of MongoDB service. If there are multiple items, use comma `,` to separate them.

<br />

### auth.mongo.pool

| Type    | Default |
| ------- | ------- |
| integer | 8       |

##### Description

Set the number of processes in the MongoDB connection pool.

<br />

### auth.mongo.login

| Type   | Default |
| ------ | ------- |
| string | -       |

##### Description

Set the MongoDB's username.

<br />

### auth.mongo.password

| Type   | Default |
| ------ | ------- |
| string | -       |

##### Description

Set the MongoDB's password.

<br />

### auth.mongo.auth_source

| Type   | Default |
| ------ | ------- |
| string | `mqtt`  |

##### Description

Set the MongoDB authentication source database name.

<br />

### auth.mongo.database

| Type   | Default |
| ------ | ------- |
| string | `mqtt`  |

##### Description

Set MongoDB database name.

<br />

### auth.mongo.query_timeout

| Type     | Default |
| -------- | ------- |
| duration | `5s`    |

##### Description

Set the timeout for accessing MongoDB.

<br />

### auth.mongo.ssl

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### Description

Set whether to use SSL to access MongoDB.

<br />

### auth.mongo.ssl_opts.keyfile

| Type   | Default |
| ------ | ------- |
| string | -       |

##### Description

If using SSL to access MongoDB, set the private key file of the SSL client.

<br />

### auth.mongo.ssl_opts.certfile

| Type   | Default |
| ------ | ------- |
| string | -       |

##### Description

If using SSL to access MongoDB, set the SSL client certificate file.

<br />

### auth.mongo.ssl_opts.cacertfile

| Type   | Default |
| ------ | ------- |
| string | -       |

##### Description

If you use SSL to access MongoDB, set the SSL certificate file.

<br />

### auth.mongo.w_mode

| Type | Optional Value            | Default |
| ---- | ------------------------- | ------- |
| enum | `unsafe`, `safe`, `undef` | `undef` |

##### Description

Set the write mode of MongoDB.

<br />

### auth.mongo.r_mode

| Type | Optional Value                | Default |
| ---- | ----------------------------- | ------- |
| enum | `master`, `slave_ok`, `undef` | `undef` |

##### Description

Set the read mode of MongoDB.

<br />

### auth.mongo.auth_query.collection

| Type   | Default     |
| ------ | ----------- |
| string | `mqtt_user` |

##### Description

Collection name used in the authentication process.

<br />

### auth.mongo.auth_query.password_field

| Type   | Default    |
| ------ | ---------- |
| string | `password` |

##### Description

The main fields used in the authentication process. To add salt after the password, it can be configured as:

```
auth.mongo.auth_query.password_field = password,salt
```

<br />

### auth.mongo.auth_query.password_hash

| Type |               Optional Value              | Default  |
| ---- | ----------------------------------------- | -------- |
| enum | `plain`, `md5`, `sha`, `sha256`, `bcrypt` | `sha256` |

##### Description

Set the hash algorithm used for the password field. To add salt after the sha256 password, you can set it to:

```
auth.mongo.auth_query.password_hash = sha256,salt
```

To add salt before the sha256 password, you can set it to:

```
auth.mongo.auth_query.password_hash = salt,sha256
```

To add salt before the bcrypt password, you can set it to:

```
auth.mongo.auth_query.password_hash = salt,bcrypt
```

<br />

### auth.mongo.auth_query.selector

| Type   | Default       |
| ------ | ------------- |
| string | `username=%u` |

##### Description

MongoDB statements are executed during the authentication process. Commands can support following wildcards:

- %u: username
- %c: clientid
- %C: Common Name in client TLS certificate
- %d: Subject in the client's TLS certificate

<br />

### auth.mongo.auth_query.super_query

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### Description

Whether to use SuperUser in authentication.

<br />

### auth.mongo.super_query.collection

| Type   | Default     |
| ------ | ----------- |
| string | `mqtt_user` |

##### Description

If using SuperUser, specify the MongoDB Collection of SuperUser.

<br />

### auth.mongo.super_query.selector

| Type   | Default                    |
| ------ | -------------------------- |
| string | `username=%u, clientid=%c` |

##### Description

If SuperUser is used, specify the MongoDB statement used to query SuperUser.

<br />

### auth.mongo.acl_query

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### Description

Whether to enable the ACL function.

<br />

### auth.mongo.acl_query.collection

| Type   | Default    |
| ------ | ---------- |
| string | `mqtt_acl` |

##### Description

If using the ACL function, specify the MongoDB Collection that queries the ACL rules.

<br />

### auth.mongo.acl_query.selector

| Type   | Default       |
| ------ | ------------- |
| string | `username=%u` |

##### Description

If the ACL function is used, specify the MongoDB statement used to query the ACL rules. It can support multiple ACL statements, and "or" is used to connect multiple statements.

For example, configure the following two access rules:

```
auth.mongo.acl_query.selector.1 = username=%u
auth.mongo.acl_query.selector.2 = username=$all
```

And the username of the client equals 'ilyas'. When querying acl rules, the following MongoDB statement will be executed:

```
db.mqtt_acl.find({$or: [{username: "ilyas"},  {username: "$all"}]});
```

<br />

### auth.mongo.topology.pool_size

| Type    | Default |
| ------- | ------- |
| integer | 1       |

##### Description

MongoDB topology parameters, that set the thread pool size.

<br />

### auth.mongo.topology.max_overflow

| Type    | Default |
| ------- | ------- |
| integer | 0       |

##### Description

MongoDB topology parameter, which determines how many additional worker threads are allowed to be created when all workers in the thread pool are busy.

<br />

### auth.mongo.topology.overflow_ttl

| Type    | Default |
| ------- | ------- |
| integer | 1000    |

##### Description

MongoDB topology parameter, which determines how long to release additional worker threads when a worker is idle. Unit: ms

<br />

### auth.mongo.topology.overflow_check_period

| Type    | Default |
| ------- | ------- |
| integer | 1000    |

##### Description

MongoDB topology parameters, which determines how often to check for idle threads to release additional workers.

<br />

### auth.mongo.topology.local_threshold_ms

| Type    | Default |
| ------- | ------- |
| integer | 1000    |

##### Description

MongoDB topology parameters, which is to select the strategy of the secondary node used to process user requests. The minimum value of the RTT of all nodes is LowestRTT, then only those secondary nodes with RTT <LowestRTT + local_threshold_ms will be selected.

<br />

### auth.mongo.topology.connect_timeout_ms

| Type    | Default |
| ------- | ------- |
| integer | 20000   |

##### Description

MongoDB topology parameter, means MongoDB connection timeout, unit: ms.

<br />

### auth.mongo.topology.socket_timeout_ms

| Type    | Default |
| ------- | ------- |
| integer | 100     |

##### Description

MongoDB topology parameter, that means MongoDB message sending timeout period, unit: ms.

<br />

### auth.mongo.topology.server_selection_timeout_ms

| Type    | Default |
| ------- | ------- |
| integer | 30000   |

##### Description

MongoDB topology parameter, select the timeout period of MongoDB Server, unit: ms.

<br />

### auth.mongo.topology.wait_queue_timeout_ms

| Type    | Default |
| ------- | ------- |
| integer | 1000    |

##### Description

MongoDB topology parameters, that selects the worker's waiting timeout period from the thread pool, unit: ms.

<br />

### auth.mongo.topology.heartbeat_frequency_ms

| Type    | Default |
| ------- | ------- |
| integer | 10000   |

##### Description

MongoDB topology parameters, the interval between topological scans, unit: ms.

<br />

### auth.mongo.topology.min_heartbeat_frequency_ms

| Type    | Default |
| ------- | ------- |
| integer | 1000    |

##### Description

MongoDB topology parameter, the minimum allowed value of `heartbeat_frequency_ms`, unit: milliseconds.

<br />

## [emqx-auth-mysql](https://github.com/emqx/emqx-auth-mysql)

### auth.mysql.server

| Type | Default          |
| ---- | ---------------- |
| ip   | `127.0.0.1:3306` |

##### Description

MySQL server address.

<br />

### auth.mysql.pool

| Type    | Default |
| ------- | ------- |
| integer | 8       |

##### Description

Database connection thread pool size.

<br />

### auth.mysql.username

| Type   | Default |
| ------ | ------- |
| string | -       |

##### Description

MySQL username.

<br />

### auth.mysql.password

| Type   | Default |
| ------ | ------- |
| string | -      |

##### Description

MySQL password.

<br />

### auth.mysql.database

| Type   | Default |
| ------ | ------- |
| string | `mqtt`  |

##### Description

MySQL database name.

<br />

### auth.mysql.query_timeout

| Type     | Default |
| -------- | ------- |
| duration | `5s`    |

##### Description

MySQL data query timeout. The query timeout means user data was not found.

< >

### auth.mysql.auth_query

| Type   | Default                                                        |
| ------ | -------------------------------------------------------------- |
| string | `select password from mqtt_user where username = '%u' limit 1` |

##### Description

The MySQL select statement used during authentication, the selected data will be compared with the password encrypted by the encryption method specified by `auth.mysql.password_hash`, and the client with the same content after the comparison will be allowed to log in. The stored password with salt needs to select the fields corresponding to the salt at the same time, such as `select password, salt from mqtt_user where username = '%u' limit 1`.` Password` and `salt` field names cannot be modified, the table name and the field name in the WHERE clause can change depending on the situation. The WHERE clause supports the following placeholders:

| Placeholder | Description                                                  |
| ----------- | ------------------------------------------------------------ |
| `%u`        | username specified in the CONNECT packet by the MQTT client that will be replaced |
| `%c`        | ClientID specified in the CONNECT packet by the MQTT client that will be replaced |
| `%C`        | Common Name in the client certificate when TLS that will be replaced is connected |
| `%d`        | Subject in the client certificate when TLS that will be replaced is connected |

<br />

### auth.mysql.password_hash

| Type   | Default  |
| ------ | ------- |
| string | `sh256` |

##### Description

The encryption method used for the password stored in the database. The following encryption methods are supported:

- `plain`, both forward and backward salting is supported, such as `salt, plain`
- `md5`, both forward and backward salting is supported
- `sha`, both forward and backward salting is supported
- `sha256`, both forward and backward salting is supported
- `sha512`, both forward and backward salting is supported
- `pbkdf2`, the format is `pbkdf2,<Hashfun>,<Iterations>,<Dklen>`. Among them, `<Hashfun> `is the hash function used, which supports `md4`, `md5`,` ripemd160` `sha`,` sha224`, `sha256`,` sha384`, `sha512`. `<Iterations>`is the number of iterations and `<Dklen>`is the length of the derived key. Example: `pbkdf2, sha256,1000,20`
- `bcrypt`, only forward salting is supported, eg `salt, bcrypt`

<br />

### auth.mysql.super_query

| Type   | Default                                                            |
| ------ | ------------------------------------------------------------------ |
| string | `select is_superuser from mqtt_user where username = '%u' limit 1` |

##### Description

The SQL select statement used for super user authentication. All table names and field names in this statement can be modified as appropriate. If and only if the value of the selected field is `1`, the user is a super user. In the WHERE clause, the supported placeholders are the same as `auth.mysql.auth_query`.

<br />

### auth.mysql.acl_query

| Type   | Default |
| ------ | ------- |
| string | `select allow, ipaddr, username, clientid, access, topic from mqtt_acl where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'` |

##### Description

The SQL selection statement used in ACL verification. All table names and field names in this statement can be modified as appropriate. The placeholders supported in the WHERE clause are as follows:

| Tag | Description                                                  |
| ------ | ------------------------------------------------------------ |
| `%a`   | To be replaced with the client IP address                    |
| `%u`   | To be replaced with the username specified by the MQTT client in the CONNECT packet |
| `%c`   | To be replaced with the client identifier specified by the MQTT client in the CONNECT packet |

<br />

## [emqx-auth-pgsql](https://github.com/emqx/emqx-auth-pgsql)

### auth.pgsql.server

| Type | Default          |
| ---- | ---------------- |
| ip   | `127.0.0.1:5432` |

##### Description

PostgreSQL server address.

<br />

### auth.pgsql.pool

| Type    | Default |
| ------- | ------- |
| integer | 8       |

##### Description

Database connection thread pool size.

<br />

### auth.pgsql.username

| Type   | Default |
| ------ | ------- |
| string | `root`  |

##### Description

PostgreSQL username.

<br />

### auth.pgsql.password

| Type   | Default |
| ------ | ------- |
| string | -       |

##### Description

PostgreSQL password.

<br />

### auth.pgsql.database

| Type   | Default |
| ------ | ------- |
| string | `mqtt`  |

##### Description

PostgreSQL database name.

<br />

### auth.pgsql.encoding

| Type   | Default |
| ------ | ------- |
| string | `utf8`  |

##### Description

PostgreSQL database character encoding format.

<br />

### auth.pgsql.ssl

| Type   | Optional Value  | Default |
| ------ | --------------- | ------- |
| enum   | `true`, `false` | `false` |

##### Description

Whether to enable TLS connection.

<br />

### auth.pgsql.ssl_opts.keyfile

| Type   | Default |
| ------ | ------- |
| string | -       |

##### Description

Client private key file path.

<br />

### auth.pgsql.ssl_opts.certfile

| Type   | Default |
| ------ | ------- |
| string | -       |

##### Description

Client certificate file path.

<br />

### auth.pgsql.ssl_opts.cacertfile

| Type   | Default |
| ------ | ------- |
| string | -       |

##### Description

Client CA certificate file path.

<br />

### auth.pgsql.auth_query

| Type   | Default                                                        |
| ------ | -------------------------------------------------------------- |
| string | `select password from mqtt_user where username = '%u' limit 1` |

##### Description

The SQL selection statement used for authentication, that is the same as `auth.mysql.auth_query`.

<br />

### auth.pgsql.password_hash

| Type   | Default |
| ------ | ------- |
| string | `sh256` |

##### Description

The encryption method used for the password stored in the database, that is the same as `auth.mysql.password_hash`.

<br />

### auth.pgsql.super_query

| Type   | Default |
| ------ | ------- |
| string | `select is_superuser from mqtt_user where username = '%u' limit 1` |

##### Description

The SQL select statement used for super user authentication, that is the same as `auth.mysql.super_query`.

<br />

### auth.pgsql.acl_query

| Type   | Default |
| ------ | ------- |
| string | `select allow, ipaddr, username, clientid, access, topic from mqtt_acl where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'` |

##### Description

The SQL selection statement used in ACL verification,  the same as `auth.mysql.acl_query`.

<br />

## [emqx-auth-redis](https://github.com/emqx/emqx-auth-redis)

### auth.redis.type

| Type     | Optional Value                  | Default   |
| -------- | ------------------------------- | --------- |
| enum     | `single`, `sentinel`, `cluster` | `single`  |

##### Description

Redis Service cluster type:
- `single`：Single node service.
- `sentinel`：sentinel pattern.
- `cluster`：cluster pattern.

<br />

### auth.redis.server

| Type     | Default            |
| -------- | ------------------ |
| string   | `127.0.0.1:6379`   |

##### Description

Redis service addresses, if there are multiple, they are separated by commas. For example, `192.168.0.1:6379, 192.168.0.2:6379`.

<br />

### auth.redis.sentinel

| Type     | Default |
| -------- | ------- |
| string   | -       |

##### Description

The cluster name in Redis sentinel mode. If it is not in `sentinel` mode, no configuration is required.

<br />

### auth.redis.pool

| Type     | Optional Value | Default |
| -------- | -------------- | ------- |
| integer  | > 0            | 8       |

##### Description

Connection pool size.

<br />

### auth.redis.database

| Type     | Default |
| -------- | ------- |
| integer  | 0       |

##### Description

The serial number of the Redis database to be connected.

<br />

### auth.redis.password

| Type     | Default |
| -------- | ------- |
| string   | -       |

##### Description

Redis password.

<br />

### auth.redis.query_timeout

| Type     | Default |
| -------- | ------- |
| duration | `5s`    |

##### Description

Redis query timeout.

<br />

### auth.redis.auth_cmd

| Type     | Default                       |
| -------- | ----------------------------- |
| string   | `HMGET mqtt_user:%u password` |

##### Description

Authentication query commands, available placeholders are:
 - `%u`: client username.
 - `%c`: client ID.
 - `%C`: `cn` of client SSL certificate.
 - `%d`: `dn` of client SSL certificate.

<br />

### auth.redis.password_hash

| Type     | Optional Value                             | Default |
| -------- | ------------------------------------------ | ------- |
| enum     | `plain`, `md5`, `sha`, `sha256`, `bcrypt`  | `plain` |

##### Description

The encoding format of the `password` field stored by Redis.

<br />

### auth.redis.super_cmd

| Type     | Default                          |
| -------- | -------------------------------- |
| string   | `HGET mqtt_user:%u is_superuser` |

##### Description

Authentication query commands for superuser, available placeholders are:

 - `%u`: client username.
 - `%c`: client ID.
 - `%C`: `cn` of client SSL certificate.
 - `%d`: `dn` of client SSL certificate.

<br />

### auth.redis.acl_cmd

| Type     | Default               |
| -------- | --------------------- |
| string   | `HGETALL mqtt_acl:%u` |

##### Description

ACL query commands. Available placeholders are:
 - `%u`: client username.
 - `%c`: client ID.

<br />

## [emqx-auth-username](https://github.com/emqx/emqx-auth-username)

### auth.user.\<Number>.username` & `auth.user.\<Number>.password

| Type   | Default |
| ------ | ------- |
| string | -       |

##### Description

The authentication data of the client, where `auth.user.<Number>.password` is the clear text password.  `auth.user.<Number>.username` and `auth.user.<Number>.password` of the same `<Number>` must appear in pairs. `<Number> `is an integer number used to distinguish authentication data of multiple clients.

<br />

### auth.user.password_hash

| Type | Optional Value                  | Default  |
| ---- | ------------------------------- | -------- |
| enum | `plain`, `md5`, `sha`, `sha256` | `sha256` |

##### Description

Hash algorithm used when the password is stored in the database. The following options are available:

`plain`

The password is stored in clear text.

`md5`

The password is encrypted and stored using the MD5 algorithm.

`sha`

The password is encrypted and stored using the SHA-1 algorithm.

`sha256`

The password is encrypted and stored using the SHA-256 algorithm.

<br />

## [emqx-bridge-mqtt](https://github.com/emqx/emqx-bridge-mqtt)

### bridge.mqtt.aws.address

| Type     | Default          |
| -------- | ---------------- |
| string   | `127.0.0.1:1883` |

##### Description

Bridge address, supports two formats, for example:
- `emqx @ 192.168.0.100`: EMQ X Broker node name, which means that the message of this node is bridged to another EMQ X node.
- `192.168.0.100: 1883`: IP address and port,which means that the message of the node is bridged to another MQTT server through an MQTT connection.

<br />

### bridge.mqtt.aws.proto_ver

| Type     | Optional Value               | Default  |
| -------- | ---------------------------- | -------- |
| enum     | `mqttv3`, `mqttv4`, `mqttv5` | `mqttv4` |

##### Description

The client protocol version of the MQTT bridge.

<br />

### bridge.mqtt.aws.start_type

| Type     | Optional Value    | Default  |
| -------- | ----------------- | -------- |
| eunm     | `manual`, `auto`  | `manual` |

##### Description

Start type:
- `auto`: start automatically with the plugin.
- `manual`: start the bridge manually.

<br />

### bridge.mqtt.aws.bridge_mode

| Type     | Optional Value   | Default |
| -------- | ---------------- | ------- |
| boolean  | `true`, `false`  | `true`  |

##### Description

Whether to enable bridging mode, only MQTT bridging is supported. After being enabled, the MQTT client started by `emqx_bridge_mqtt` will carry a flag bit when sending a connection message, indicating that this is a bridging client.

::: tip Tip
RabbitMQ currently does not support this flag.
:::

<br />

### bridge.mqtt.aws.clientid

| Type     | Default      |
| -------- | ------------ |
| string   | `bridge_aws` |

##### Description

The client ID of the MQTT bridge.

<br />

### bridge.mqtt.aws.clean_start

| Type     | Optional Value   | Default |
| -------- | ---------------- | ------- |
| boolean  | `true`, `false`  | `true`  |

##### Description

The `clean_start` flag of the MQTT bridge. It indicates whether the client connects to the remote MQTT Broker in the manner of `clean session`.

<br />

### bridge.mqtt.aws.username

| Type     | Default |
| -------- | ------- |
| string   | `user`  |

##### Description

The username of the MQTT bridge client.

<br />

### bridge.mqtt.aws.password

| Type     | Default  |
| -------- | -------- |
| string   | `passwd` |

##### Description

The password of the MQTT bridge client.

<br />

### bridge.mqtt.aws.forwards

| Type     | Default             |
| -------- | ------------------- |
| string   | `topic1/#,topic2/#` |

##### Description

Bridge forwarding rules. For example:
- `topic1/#, topic2/#`：`emqx_bridge_mqtt` will forward all topic messages in EMQ X Broker that match `topic1/#`，`topic2/#` .

<br />

### bridge.mqtt.aws.forward_mountpoint

| Type     | Default               |
| -------- | --------------------- |
| string   | `bridge/aws/${node}/` |

##### Description

The prefix of the forwarding topic. When forwarding the message to the target system, it is supported to add a uniform prefix to the topic.

<br />

### bridge.mqtt.aws.subscription.1.topic

| Type     | Default |
| -------- | ------- |
| string   | -       |

##### Description

Topic of the peer system subscribed.

<br />

### bridge.mqtt.aws.subscription.1.qos

| Type     | Optional Value | Default |
| -------- | -------------- | ------- |
| enum     | `0`, `1`, `2`  | `1`     |

##### Description

QoS of the peer system topic subscribed.

<br />

### bridge.mqtt.aws.receive_mountpoint

| Type     | Default        |
| -------- | -------------- |
| string   | `receive/aws/` |

##### Description

The topic prefix of the received message.`emqx_bridge_mqtt` supports adding a unified topic prefix to the message from the peer.

<br />

### bridge.mqtt.aws.ssl

| Type     | Optional Value  | Default |
| -------- | --------------- | ------- |
| boolean  | `true`, `false` | `true`  |

##### Description

Whether the MQTT bridge client enables SSL.

<br />

### bridge.mqtt.aws.cacertfile

| Type     | Default                |
| -------- | ---------------------- |
| string   | `etc/certs/cacert.pem` |

##### Description

The path of the CA certificate file of the MQTT bridge client.

<br />

### bridge.mqtt.aws.certfile

| Type     | Default                     |
| -------- | --------------------------- |
| string   | `etc/certs/client-cert.pem` |

##### Description

The path of the SSL certificate file of the MQTT bridge client.

<br />

### bridge.mqtt.aws.keyfile

| Type     | Default                    |
| -------- | -------------------------- |
| string   | `etc/certs/client-key.pem` |

##### Description

The path of the SSL key file of the MQTT bridge client.

<br />

### bridge.mqtt.aws.ciphers

| Type     | Default                                                     |
| -------- | ----------------------------------------------------------- |
| string   | `ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384` |

##### Description

Cipher suite supported by SSL handshake.

<br />

### bridge.mqtt.aws.psk_ciphers

| Type     | Default                                                                  |
| -------- | ------------------------------------------------------------------------ |
| string   | `PSK-AES128-CBC-SHA,PSK-AES256-CBC-SHA,PSK-3DES-EDE-CBC-SHA,PSK-RC4-SHA` |

##### Description

Cipher suite supported by SSL PSK handshake.

<br />

### bridge.mqtt.aws.keepalive

| Type     | Default |
| -------- | ------- |
| duration | `60s`   |

##### Description

Heartbeat interval of the MQTT bridge client.

<br />

### bridge.mqtt.aws.tls_versions

| Type     | Default                 |
| -------- | ----------------------- |
| string   | `tlsv1.2,tlsv1.1,tlsv1` |

##### Description

The SSL version of the MQTT bridge client.

<br />

### bridge.mqtt.aws.reconnect_interval

| Type     | Default |
| -------- | ------- |
| duration | `30s`   |

##### Description

Reconnection interval.

<br />

### bridge.mqtt.aws.retry_interval

| Type     | Default |
| -------- | ------- |
| duration | `20s`   |

##### Description

QoS 1/2 message retransmission interval.

<br />

### bridge.mqtt.aws.batch_size

| Type     | Default |
| -------- | ------- |
| integer  | 32      |

##### Description

The batch size of the EMQ X bridge. The EMQ X bridge mode of `emqx_bridge_mqtt` supports batch sending of messages to increase throughput.

<br />

### bridge.mqtt.aws.max_inflight_size

| Type     | Default |
| -------- | ------- |
| integer  | 32      |

##### Description

Inflight window size.

<br />

### bridge.mqtt.aws.queue.replayq_dir

| Type     | Default                  |
| -------- | ----------------------- |
| string   | `etc/emqx_aws_bridge/`  |

##### Description

Set the message queue file path. If not configured, only memory storage is used.

<br />

### bridge.mqtt.aws.queue.replayq_seg_bytes

| Type     | Default |
| -------- | ------- |
| bytesize | `10MB`  |

##### Description

The single file size of the message queue stored on disk.

<br />

### bridge.mqtt.aws.queue.max_total_size

| Type     | Default |
| -------- | ------- |
| bytesize | `5GB`   |

##### Description

The maximum allowed message queue storage.

<br />

## [emqx-coap](https://github.com/emqx/emqx-coap)

### coap.port

| Type    | Default |
| ------- | ------- |
| integer | 5683    |

##### Description

Specify the UDP binding port of the CoAP plug-in.

<br />

### coap.enable_stats

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `off`   |

##### Description

Enable or disable CoAP statistics function.

<br />

### coap.dtls.port

| Type    | Default |
| ------- | ------- |
| integer | 5684    |

##### Description

Specify the DTLS binding port of the CoAP plugin.

<br />

### coap.dtls.verify

| Type | Optional Value               | Default       |
| ---- | ---------------------------- | ------------- |
| enum | `verify_peer`, `verify_none` | `verify_peer` |

##### Description

When using DTLS, specify whether to verify the client during the DTLS handshake.

<br />

### coap.dtls.keyfile

| Type   | Default             |
| ------ | ------------------- |
| string | `etc/certs/key.pem` |

##### Description

When using DTLS, specify the DTLS private key file.

<br />

### coap.dtls.certfile

| Type   | Default              |
| ------ | -------------------- |
| string | `etc/certs/cert.pem` |

##### Description

When using DTLS, specify the DTLS certificate file.

<br />

### coap.dtls.cacertfile

| Type   | Default                |
| ------ | ---------------------- |
| string | `etc/certs/cacert.pem` |

##### Description

When using DTLS, specify the CA certificate file for DTLS.

<br />

### coap.dtls.fail_if_no_peer_cert

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### Description

When using DTLS, if the client does not have a certificate during the DTLS handshake, whether to let the handshake fail.

<br />

### coap.dtls.ciphers

| Type | Default |
| ---- | ------- |
| string | `ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384,ECDHE-ECDSA-AES256-SHA384,ECDHE-RSA-AES256-SHA384,ECDHE-ECDSA-DES-CBC3-SHA,ECDH-ECDSA-AES256-GCM-SHA384,ECDH-RSA-AES256-GCM-SHA384,ECDH-ECDSA-AES256-SHA384,ECDH-RSA-AES256-SHA384,DHE-DSS-AES256-GCM-SHA384,DHE-DSS-AES256-SHA256,AES256-GCM-SHA384,AES256-SHA256,ECDHE-ECDSA-AES128-GCM-SHA256,ECDHE-RSA-AES128-GCM-SHA256,ECDHE-ECDSA-AES128-SHA256,ECDHE-RSA-AES128-SHA256,ECDH-ECDSA-AES128-GCM-SHA256,ECDH-RSA-AES128-GCM-SHA256,ECDH-ECDSA-AES128-SHA256,ECDH-RSA-AES128-SHA256,DHE-DSS-AES128-GCM-SHA256,DHE-DSS-AES128-SHA256,AES128-GCM-SHA256,AES128-SHA256,ECDHE-ECDSA-AES256-SHA,ECDHE-RSA-AES256-SHA,DHE-DSS-AES256-SHA,ECDH-ECDSA-AES256-SHA,ECDH-RSA-AES256-SHA,AES256-SHA,ECDHE-ECDSA-AES128-SHA,ECDHE-RSA-AES128-SHA,DHE-DSS-AES128-SHA,ECDH-ECDSA-AES128-SHA,ECDH-RSA-AES128-SHA,AES128-SHA` |

##### Description

When using DTLS, specify the Cipher list supported by the DTLS server.

<br />

## [emqx-dashboard](https://github.com/emqx/emqx-dashboard)

### dashboard.default_user.login` & `dashboard.default_user.password

| Type   | Default |
| ------ | ------- |
| string | -       |

##### Description

Default user authentication data of Dashboard. `dashboard.default_user.login` and `dashboard.default_user.password` must exist at the same time.

<br />

### dashboard.listener.http

| Type    | Default |
| ------- | ------- |
| integer | 18083   |

##### Description

The listening port of the HTTP listener.

<br />

### dashboard.listener.http.acceptors

| Type    | Default |
| ------- | ------- |
| integer | 4       |

##### Description

The number of listening processes this listener will create.

<br />

### dashboard.listener.http.max_clients

| Type    | Default |
| ------- | ------- |
| integer | 512     |

##### Description

The maximum number of connections allowed by this listener at the same time.

<br />

### dashboard.listener.http.inet6

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `ture`, `false` | `false` |

##### Description

Whether to set the socket to allow IPv6 connections.

<br />

### dashboard.listener.http.ipv6_v6only

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `ture`, `false` | `false` |

##### Description

Whether to restrict the socket that only IPv6 can be ued, and prohibit any IPv4 connections. Only applicable to IPv6 sockets, that is, the value of this configuration item has practical significance only when `dashboard.listener.http.inet6` is set to `true`. It should be noted that on some operating systems, such as Windows, the only allowed value for this configuration item is `true`.

<br />

### dashboard.listener.https

| Type    | Default |
| ------- | ------- |
| integer | 18084   |

##### Description

The listening port of the HTTPS listener, **which is disabled by default.**

<br />

### dashboard.listener.https.acceptors

| Type    | Default |
| ------- | ------- |
| integer | 2       |

##### Description

Same as `dashboard.listener.http.acceptors`.

<br />

### dashboard.listener.https.max_clients

| Type    | Default |
| ------- | ------- |
| integer | 512     |

##### Description

Same as `dashboard.listener.http.max_clients`.

<br />

### dashboard.listener.https.inet6

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `ture`, `false` | `false` |

##### Description

Same as `dashboard.listener.http.inet6`.

<br />

### dashboard.listener.https.ipv6_v6only

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `ture`, `false` | `false` |

##### Description

Same as `dashboard.listener.http.ipv6_v6only`.

<br />

### dashboard.listener.https.keyfile

| Type   | Default             |
| ------ | ------------------- |
| string | `etc/certs/key.pem` |

##### Description

Server private key file path.

<br />

### dashboard.listener.https.certfile

| Type   | Default              |
| ------ | -------------------- |
| string | `etc/certs/cert.pem` |

##### Description

Server certificate file path.

<br />

### dashboard.listener.https.cacertfile

| Type   | Default                |
| ------ | ---------------------- |
| string | `etc/certs/cacert.pem` |

##### Description

CA certificate file path.

<br />

### dashboard.listener.https.dhfile

| Type   | Default                   |
| ------ | ------------------------- |
| string | `etc/certs/dh-params.pem` |

##### Description

If a cipher suite exchanged from Diffie Hellman key is used, you can use this configuration item to specify a file path that contains PEM-encoded Diffie Hellman parameters. If not specified, the default parameters are used.

<br />

### dashboard.listener.https.verify

| Type | Optional Value               | Default       |
| ---- | ---------------------------- | ------------- |
| enum | `verify_peer`, `verify_none` | `verify_peer` |

##### Description

`verify_none` means to disable peer certificate verification, and the server will not send a certificate request to the client. `verify_peer` means to enable peer certificate verification, and the server will send a certificate request to the client. When this configuration item is set to `verify_peer`, it usually need to be used together with `dashboard.listener.https.fail_if_no_peer_cert` to specify whether to force the client to provide a certificate.

<br />

### dashboard.listener.https.fail_if_no_peer_cert

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `ture`, `false` | `true`  |

##### Description

It should be used together with `dashboard.listener.https.verify`. If set to `true`, the server will request a certificate from the client. If the client does not provide a certificate, the handshake will fail. If set to `false`, the handshake can be successful even if the terminal does not provide a certificate.

<br />

### dashboard.listener.https.tls_versions

| Type   | Default                 |
| ------ | ----------------------- |
| string | `tlsv1.2,tlsv1.1,tlsv1` |

##### Description

Specify the TLS protocol version supported by the server. The versions are separated by `,`. The supported TLS protocol versions are: `tlsv1.3`, `tlsv1.2`, `tlsv1.1`, `tlsv1`, `sslv3`.

<br />

### dashboard.listener.https.ciphers

| Type   | Default                                                      |
| ------ | ------------------------------------------------------------ |
| string | `ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384,ECDHE-ECDSA-AES256-SHA384,ECDHE-RSA-AES256-SHA384,ECDHE-ECDSA-DES-CBC3-SHA,ECDH-ECDSA-AES256-GCM-SHA384,ECDH-RSA-AES256-GCM-SHA384,ECDH-ECDSA-AES256-SHA384,ECDH-RSA-AES256-SHA384,DHE-DSS-AES256-GCM-SHA384,DHE-DSS-AES256-SHA256,AES256-GCM-SHA384,AES256-SHA256,ECDHE-ECDSA-AES128-GCM-SHA256,ECDHE-RSA-AES128-GCM-SHA256,ECDHE-ECDSA-AES128-SHA256,ECDHE-RSA-AES128-SHA256,ECDH-ECDSA-AES128-GCM-SHA256,ECDH-RSA-AES128-GCM-SHA256,ECDH-ECDSA-AES128-SHA256,ECDH-RSA-AES128-SHA256,DHE-DSS-AES128-GCM-SHA256,DHE-DSS-AES128-SHA256,AES128-GCM-SHA256,AES128-SHA256,ECDHE-ECDSA-AES256-SHA,ECDHE-RSA-AES256-SHA,DHE-DSS-AES256-SHA,ECDH-ECDSA-AES256-SHA,ECDH-RSA-AES256-SHA,AES256-SHA,ECDHE-ECDSA-AES128-SHA,ECDHE-RSA-AES128-SHA,DHE-DSS-AES128-SHA,ECDH-ECDSA-AES128-SHA,ECDH-RSA-AES128-SHA,AES128-SHA` |

##### Description

Specify the cipher suite supported by the server.

<br />

### dashboard.listener.https.secure_renegotiate

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `off`   |

##### Description

Specifies whether to start the secure renegotiation mechanism.

<br />

### dashboard.listener.https.reuse_sessions

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### Description

Specifies whether to enable the session resuing mechanism.

<br />

### dashboard.listener.https.honor_cipher_order

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### Description

If set to `on`, use the server ’s preferences for password selection. If set to `off`, use the client ’s preferences.

<br />

## [emqx-lwm2m](https://github.com/emqx/emqx-lwm2m)

### lwm2m.port

| Type    | Default |
| ------- | ------- |
| integer | 5683    |

##### Description

Specify the UDP port used by LwM2M.

<br />

### lwm2m.lifetime_min

| Type     | Default |
| -------- | ------- |
| duration | `1s`    |

##### Description

Specify the minimum allowed LwM2M lifetime with the unit of second.

<br />

### lwm2m.lifetime_max

| Type     | Default  |
| -------- | -------- |
| duration | `86400s` |

##### Description

Specify the maximum allowed LwM2M lifetime with the unit of second.

<br />

### lwm2m.qmode_time_window

| Type    | Default |
| ------- | ------- |
| integer | 22      |

##### Description

Specifies the window size used in LwM2M Q mode, with the unit of second.

<br />

Within this window period, it can be sent to the Q mode device, and after the window period, the downlink data is cached.

### lwm2m.lb

| Type | Optional Value          | Default     |
| ---- | ----------------------- | ----------- |
| enum | `coaproxy`, `undefined` | `undefined` |

##### Description

Set whether to use coaproxy. `undefined` means not to  use coaproxy.

<br />

### lwm2m.auto_observe

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `off`   |

##### Description

Whether to automatically issue the observe command after device registration.

<br />

### lwm2m.mountpoint

| Type   | Default     |
| ------ | ----------- |
| string | `lwm2m/%e/` |

##### Description

Set the mount point of the LwM2M topic. The following wildcards are supported:

- '%e': Endpoint Name
- '%a': IP Address

<br />

### lwm2m.topics.command

| Type   | Default |
| ------ | ------- |
| string | `dn/#`  |

##### Description

The command line topic that need to be subscribd after the device registration is completed.

<br />

### lwm2m.topics.response

| Type   | Default   |
| ------ | --------- |
| string | `up/resp` |

##### Description

Which topic the device's upstream response needs to be published to.

<br />

### lwm2m.topics.notify

| Type   | Default     |
| ------ | ----------- |
| string | `up/notify` |

##### Description

Which topic the device's upstream report message (notify) needs to be published to.

<br />

### lwm2m.topics.register

| Type   | Default   |
| ------ | --------- |
| string | `up/resp` |

##### Description

Which topic the device's upstream registration message (register) needs to be published to.

<br />

### lwm2m.topics.update

| Type   | Default   |
| ------ | --------- |
| string | `up/resp` |

##### Description

Which topic the device's upstream update message (update) needs to be published to.

<br />

### lwm2m.opts.buffer

| Type     | Default  |
| -------- | -------- |
| bytesize | `1024KB` |

##### Description

UDP tuning parameters, specify the UDP user mode cache size.

<br />

### lwm2m.opts.recbuf

| Type     | Default  |
| -------- | -------- |
| bytesize | `1024KB` |

##### Description

UDP tuning parameters, specify the UDP receiving buffer size.

<br />

### lwm2m.opts.sndbuf

| Type     | Default  |
| -------- | -------- |
| bytesize | `1024KB` |

##### Description

UDP tuning parameters, specify the UDP sending buffer size.

<br />

### lwm2m.opts.read_packets

| Type    | Default |
| ------- | ------- |
| integer | 20      |

##### Description

UDP tuning parameters, specify how many packets to read from UDP socket each time.

<br />

### lwm2m.certfile

| Type   | Default              |
| ------ | -------------------- |
| string | `etc/certs/cert.pem` |

##### Description

Specify the certificate file used by UDP DTLS.

<br />

### lwm2m.keyfile

| Type   | Default             |
| ------ | ------------------- |
| string | `etc/certs/key.pem` |

##### Description

Specify the private key file used by UDP DTLS.

<br />

### lwm2m.xml_dir

| Type | Default         |
| ---- | --------------- |
| dir  | `etc/lwm2m_xml` |

##### Description

Specify the directory where the LwM2M Object definition file is stored.

<br />

## [emqx-management](https://github.com/emqx/emqx-management)

### management.max_row_limit

| Type    | Default |
| ------- | ------- |
| integer | 10000   |

##### Description

Maximum number of records returned during paging query.

<br />

### management.default_application.id

| Type   | Default |
| ------ | ------- |
| string | `admin` |

##### Description

Default AppId.

<br />

### management.default_application.secret

| Type   | Default  |
| ------ | -------- |
| string | `public` |

##### Description

Default AppSecret.

<br />

### management.listener.http

| Type    | Default |
| ------- | ------- |
| integer | 8081    |

##### Description

The listening port of the HTTP listener.

<br />

### management.listener.http.acceptors

| Type    | Default |
| ------- | ------- |
| integer | 2       |

##### Description

The number of listening processes this listener will create.

<br />

### management.listener.http.max_clients

| Type    | Default |
| ------- | ------- |
| integer | 512     |

##### Description

The maximum number of connections allowed by this listener at the same time

<br />

### management.listener.http.backlog

| Type    | Default |
| ------- | ------- |
| integer | 512     |

##### Description

The maximum length of the TCP connection queue. It indicates the maximum number of TCP connection queues that are allowed in the system to undergo three-time handshake.

<br />

### management.listener.http.send_timeout

| Type    | Default |
| ------- | ------- |
| duration | `15s`  |

##### Description

HTTP packet sending timeout.

<br />

### management.listener.http.send_timeout_close

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### Description

Whether to close the connection after the HTTP packet sending is timeout.

<br />

### management.listener.http.inet6

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### Description

Whether to set the socket to allow IPv6 connections.

<br />

### management.listener.http.ipv6_v6only

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### Description

Whether to restrict the socket that only IPv6 can be ued, and prohibit any IPv4 connections. Only applicable to IPv6 sockets, that is, the value of this configuration item has practical significance only when `dashboard.listener.http.inet6` is set to `true`. It should be noted that on some operating systems, such as Windows, the only allowed value for this configuration item is `true`.

<br />

### management.listener.https

| Type    | Default | Example |
| ------- | ------- | ------- |
| integer | -       | 8081    |

##### Description

The listening port of the HTTPS listener.

### management.listener.https.acceptors

| Type    | Default |
| ------- | ------- |
| integer | 2       |

##### Description

The number of listening processes this listener will create.

<br />

### management.listener.https.max_clients

| Type    | Default |
| ------- | ------- |
| integer | 512     |

##### Description

The maximum number of connections allowed by this listener at the same time.

<br />

### management.listener.https.backlog

| Type    | Default |
| ------- | ------- |
| integer | 512     |

##### Description

The maximum length of the TCP connection queue. It indicates the maximum number of TCP connection queues that are allowed in the system to undergo three-time handshake.

<br />

### management.listener.https.send_timeout

| Type    | Default |
| ------- | ------- |
| duration | `15s`  |

##### Description

Timeout for sending HTTPS packets.

<br />

### management.listener.https.send_timeout_close

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### Description

Whether to close the connection after the HTTPS packet sending is timeout.

<br />

### management.listener.https.keyfile

| Type   | Default             |
| ------ | ------------------- |
| string | `etc/certs/key.pem` |

##### Description

Server private key file path.

<br />

### management.listener.https.certfile

| Type   | Default              |
| ------ | -------------------- |
| string | `etc/certs/cert.pem` |

##### Description

Server certificate file path.

<br />

### management.listener.https.cacertfile

| Type   | Default                |
| ------ | ---------------------- |
| string | `etc/certs/cacert.pem` |

##### Description

CA certificate file path.

<br />

### management.listener.https.verify

| Type | Optional Value               | Default       |
| ---- | ---------------------------- | ------------- |
| enum | `verify_peer`, `verify_none` | `verify_peer` |

##### Description

`verify_none` means to disable peer certificate verification, and the server will not send a certificate request to the client. `verify_peer` means to enable peer certificate verification, and the server will send a certificate request to the client. When this configuration item is set to `verify_peer`, it usually need to be used together with `dashboard.listener.https.fail_if_no_peer_cert` to specify whether to force the client to provide a certificate.

<br />

### management.listener.https.fail_if_no_peer_cert

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `ture`, `false` | `true`  |

##### Description

It should be used together with `management.listener.https.verify`. If set to `true`, the server will fail the handshake if the client does not provide a certificate when requesting a certificate from the client. If set to `false`, the handshake can be successful even if the terminal does not provide a certificate.

<br />

### management.listener.https.inet6

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### Description

Whether to set the socket to allow IPv6 connections.

<br />

### management.listener.https.ipv6_v6only

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### Description

Whether to restrict the socket that only IPv6 can be ued, and prohibit any IPv4 connections. Only applicable to IPv6 sockets, that is, the value of this configuration item has practical significance only when `dashboard.listener.http.inet6` is set to `true`. It should be noted that on some operating systems, such as Windows, the only allowed value for this configuration item is `true`.

<br />

## [emqx-reloader](https://github.com/emqx/emqx-reloader)

### reloader.interval

| Type     | Default |
| -------- | ------- |
| duration | `60s`   |

##### Description

How often do hot update all code.

<br />

### reloader.logfile

| Type   | Default        |
| ------ | -------------- |
| string | `reloader.log` |

##### Description

Log files for hot updates of code.

<br />

## [emqx-retainer](https://github.com/emqx/emqx-retainer)

### retainer.storage_type

| Type | Optional Value             | Default |
| ---- | -------------------------- | ------- |
| enum | `ram`, `disc`, `disc_only` | `ram`   |

##### Description

Storage type of the message, the following options are available:

`ram`

Retained messages are only stored in memory.

`disc`

Retained messages are stored in both memory and disk.

`disc_only`

Retained  messages are only stored on disk.

<br />

### retainer.max_retained_messages

| Type    | Default |
| ------- | ------- |
| integer | 0       |

##### Description

Limit of retained messages. Once the number of stored messages reaches the limit, you can replace existing retained messages, but you cannot store retained messages for new topics. 0 means no limit.

<br />

### retainer.max_payload_size

| Type     | Default |
| -------- | ------- |
| bytesize | `1MB`   |

##### Description

The maximum length of Payload allowed to store retained messages. If the Payload exceeds the maximum limit, the retained message can be processed normally, but it will not be stored on the server.

<br />

### retainer.expiry_interval

| Type     | Default |
| -------- | ------- |
| duration | `0`     |

##### Description

The expiration interval of retained messages which is only valid for clients with protocol versions lower than MQTT v5.0. The expiration interval of retained messages for MQTT v5.0 clients will be based on the value of `Message Expiry Interval`. 0 means never expire.

<br />

## [emqx-rule-engine](https://github.com/emqx/emqx-rule-engine)

### rule_engine.ignore_sys_message

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### Description

Ignore system messages ($ SYS). The rule engine will not process system messages if this option is enabled.

<br />

### rule_engine.events.\<event-name>

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `off`   |

##### Description

Set whether to publish event messages. You can specify the QoS of event messages, for example:

```
rule_engine.events.client_connected = on, qos1

```

If this option is enabled, the rules engine will publish system messages using the topic of `$events/\<event-name>`. Supported `<event-name>` are:

- client_connected
- client_disconnected
- session_subscribed
- session_unsubscribed
- message_delivered
- message_acked
- message_dropped

If this option is disabled, event messages will not be published, but event rules can still be used. For example, even if `rule_engine.events.client_connected = off`, the following rules can still be used:

```
SELECT * FROM "$events/client_connected"
```

<br />

## [emqx-sn](https://github.com/emqx/emqx-sn)

### mqtt.sn.port

| Type     | Default |
| -------- | ------- |
| string   | `1884`  |

##### Description

The UDP port that `emqx_sn` listens on.

<br />

### mqtt.sn.advertise_duration

| Type     | Default |
| -------- | ------- |
| duration | `15s`   |

##### Description

ADVERTISE message broadcast interval, unit: second.

<br />

### mqtt.sn.gateway_id

| Type     | Default |
| -------- | ------- |
| integer  | 1       |

##### Description

MQTT-SN gateway ID in ADVERTISE.

<br />

### mqtt.sn.enable_stats

| Type     | Optional Value | Default |
| -------- | -------------- | ------- |
| enum     | `on`, `off`    | `off`   |

##### Description

Whether to enable client status statistics.

<br />

### mqtt.sn.enable_qos3

| Type     | Optional Value | Default |
| -------- | -------------- | ------- |
| enum     | `on`, `off`    | `off`   |

##### Description

Whether to process messages with QoS of -1.

<br />

### mqtt.sn.idle_timeout

| Type     | Default |
| -------- | ------- |
| duration | `30s`   |

##### Description

The idle time after the establishment, if no message is received within this time, the connection will be closed.

<br />

### mqtt.sn.predefined.topic.0

| Type     | Default    |
| -------- | ---------- |
| string   | `reserved` |

##### Description

The predefined mapping of Topic and TopicId. Topics with an Id of 0 are reserved and fixed to `reserved`. For example, the Id of the predefined topic `foo / bar` is `1`:

```
mqtt.sn.predefined.topic.1 = foo/bar
```

<br />

### mqtt.sn.username

| Type     | Default        |
| -------- | -------------- |
| string   | `mqtt_sn_user` |

##### Description

`emqx_sn` username to connect to EMQ X Broker.

<br />

### mqtt.sn.password

| Type     | Default |
| -------- | ------- |
| string   | `abc`   |

##### Description

`emqx_sn` password to connect to EMQ X Broker.

<br />

## [emqx-statsd](https://github.com/emqx/emqx-statsd)

### statsd.push.gateway.server

| Type   | Default                 |
| ------ | ----------------------- |
| string | `http://127.0.0.1:9091` |

##### Description

Specify the URI of the Statsd gateway.

<br />

### statsd.interval

| Type    | Default |
| ------- | ------- |
| integer | 15000   |

##### Description

Specify the collection interval of Statsd data in milliseconds.

<br />

### prometheus.collector.\<N>

| Type   | Default       |
| ------ | ------------- |
| string | `emqx_statsd` |

##### Description

Specify Prometheus Collector.

<br />

## [emqx-stomp](https://github.com/emqx/emqx-stomp)

### stomp.listener

| Type    | Default |
| ------- | ------- |
| integer | 61613   |

##### Description

Specify the local port where the Stomp plugin listens.

<br />

### stomp.listener.acceptors

| Type    | Default |
| ------- | ------- |
| integer | 4       |

##### Description

Specify the size of the thread pool for Stomp service Acceptor 

<br />

### stomp.listener.max_connections

| Type    | Default |
| ------- | ------- |
| integer | 512     |

##### Description

Specify the maximum number of connections supported by the Stomp service.

<br />

### stomp.listener.ssl

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `off`   |

##### Description

Specify whether to use SSL.

<br />

### stomp.listener.keyfile

| Type   | Default             |
| ------ | ------------------- |
| string | `etc/certs/key.pem` |

##### Description

If using SSL, specify the SSL private key file.

<br />

### stomp.listener.certfile

| Type   | Default              |
| ------ | -------------------- |
| string | `etc/certs/cert.pem` |

##### Description

If using SSL, specify the SSL certificate file.

<br />

### stomp.listener.cacertfile

| Type   | Default                |
| ------ | ---------------------- |
| string | `etc/certs/cacert.pem` |

##### Description

If using SSL, specify the CA certificate file for SSL.

<br />

### stomp.listener.dhfile

| Type   | Default                   |
| ------ | ------------------------- |
| string | `etc/certs/dh-params.pem` |

##### Description

If using SSL, specify the key file used by the Ephemeral Diffie-Hellman algorithm.

<br />

### stomp.listener.verify

| Type | Optional Value               | Default       |
| ---- | ---------------------------- | ------------- |
| enum | `verify_peer`, `verify_none` | `verify_peer` |

##### Description

If using SSL, specify whether to verify the client during the handshake.

<br />

### stomp.listener.fail_if_no_peer_cert

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### Description

Specify whether the handshake fail if SSL is used, and the client does not have a certificate during the SSL handshake.

<br />

### stomp.listener.tls_versions

| Type   | Default                 |
| ------ | ----------------------- |
| string | `tlsv1.2,tlsv1.1,tlsv1` |

##### Description

If using SSL, specify the list of SSL versions supported by the server.

<br />

### stomp.listener.handshake_timeout

| Type     | Default |
| -------- | ------- |
| duration | `15s`   |

##### Description

If using SSL, specify the timeout period for the SSL handshake process.

<br />

### stomp.listener.ciphers

| Type | Default |
| ---- | ------- |
| string | `ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384,ECDHE-ECDSA-AES256-SHA384,ECDHE-RSA-AES256-SHA384,ECDHE-ECDSA-DES-CBC3-SHA,ECDH-ECDSA-AES256-GCM-SHA384,ECDH-RSA-AES256-GCM-SHA384,ECDH-ECDSA-AES256-SHA384,ECDH-RSA-AES256-SHA384,DHE-DSS-AES256-GCM-SHA384,DHE-DSS-AES256-SHA256,AES256-GCM-SHA384,AES256-SHA256,ECDHE-ECDSA-AES128-GCM-SHA256,ECDHE-RSA-AES128-GCM-SHA256,ECDHE-ECDSA-AES128-SHA256,ECDHE-RSA-AES128-SHA256,ECDH-ECDSA-AES128-GCM-SHA256,ECDH-RSA-AES128-GCM-SHA256,ECDH-ECDSA-AES128-SHA256,ECDH-RSA-AES128-SHA256,DHE-DSS-AES128-GCM-SHA256,DHE-DSS-AES128-SHA256,AES128-GCM-SHA256,AES128-SHA256,ECDHE-ECDSA-AES256-SHA,ECDHE-RSA-AES256-SHA,DHE-DSS-AES256-SHA,ECDH-ECDSA-AES256-SHA,ECDH-RSA-AES256-SHA,AES256-SHA,ECDHE-ECDSA-AES128-SHA,ECDHE-RSA-AES128-SHA,DHE-DSS-AES128-SHA,ECDH-ECDSA-AES128-SHA,ECDH-RSA-AES128-SHA,AES128-SHA` |

##### Description

If using SSL, specify the Cipher list supported by the server

<br />

### stomp.listener.secure_renegotiate

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `off`   |

##### Description

If using SSL, specify whether to reject the renegotiation request if the client does not follow RFC 5746.

<br />

### stomp.listener.reuse_sessions

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### Description

If using SSL, specify whether to support SSL session reuse.

<br />

### stomp.listener.honor_cipher_order

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### Description

If using SSL, specify whether to use the server's preferences to select Ciphers.

<br />

### stomp.default_user.login

| Type   | Default |
| ------ | ------- |
| string | `guest` |

##### Description

Specify the Username used by the Stomp plugin to log in.

<br />

### stomp.default_user.passcode

| Type   | Default |
| ------ | ------- |
| string | `guest` |

##### Description

Specify the password used for Stomp plugin login.

<br />

### stomp.allow_anonymous

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### Description

Whether to allow anonymous login.

<br />

### stomp.frame.max_headers

| Type    | Default |
| ------- | ------- |
| integer | 10      |

##### Description

Specify the maximum number of Stomp headers

<br />

### stomp.frame.max_header_length

| Type    | Default |
| ------- | ------- |
| integer | 1024    |

##### Description

Specify the maximum Stomp header length

<br />

### stomp.frame.max_body_length

| Type    | Default |
| ------- | ------- |
| integer | 8192    |

##### Description

Specify Stomp maximum message body length.

<br />

## [emqx-web-hook](https://github.com/emqx/emqx-web-hook)

### web.hook.api.url

| Type   | Default |
| ------ | ------- |
| string | -       |

##### Description

`emqx_web_hook` Forwarding web server address.

<br />

### web.hook.encode_payload

| Type     | Optional Value      | Default |
| -------- | ------------------- | ------- |
| enum     | `base62`, `base64`  | -       |

##### Description

The encoding format of the Payload field in the PUBLISH message.

<br />

### web.hook.rule.client.connect.1

| Type     | Default                           |
| -------- | --------------------------------- |
| string   | `{"action": "on_client_connect"}` |

##### Description

Forward the `on_client_connect` event.

<br />

### web.hook.rule.client.connack.1

| Type     | Default                           |
| -------- | --------------------------------- |
| string   | `{"action": "on_client_connack"}` |

##### Description

Forward the `on_client_connack` event.

<br />

### web.hook.rule.client.connected.1

| Type     | Default                             |
| -------- | ----------------------------------- |
| string   | `{"action": "on_client_connected"}` |

##### Description

Forward the `on_client_connected` event.

<br />

### web.hook.rule.client.disconnected.1

| Type     | Default                                |
| -------- | -------------------------------------- |
| string   | `{"action": "on_client_disconnected"}` |

##### Description

Forward the `on_client_disconnected` event.

<br />

### web.hook.rule.client.subscribe.1

| Type     | Default                             |
| -------- | ----------------------------------- |
| string   | `{"action": "on_client_subscribe"}` |

##### Description

Forward the `on_client_subscribe` event.

<br />

### web.hook.rule.client.unsubscribe.1

| Type     | Default                               |
| -------- | ------------------------------------- |
| string   | `{"action": "on_client_unsubscribe"}` |

##### Description

Forward the `on_client_unsubscribe` event.

<br />

### web.hook.rule.session.subscribed.1

| Type     | Default                               |
| -------- | ------------------------------------- |
| string   | `{"action": "on_session_subscribed"}` |

##### Description

Forward the `on_client_subscribe` event.

<br />

### web.hook.rule.session.unsubscribed.1

| Type     | Default                                 |
| -------- | --------------------------------------- |
| string   | `{"action": "on_session_unsubscribed"}` |

##### Description

Forward the `on_session_unsubscribe` event.

<br />

### web.hook.rule.session.terminated.1

| Type     | Default                               |
| -------- | ------------------------------------- |
| string   | `{"action": "on_session_terminated"}` |

##### Description

Forward the `on_session_terminated` event.

<br />

### web.hook.rule.message.publish.1

| Type     | Default                            |
| -------- | ---------------------------------- |
| string   | `{"action": "on_message_publish"}` |

##### Description

Forward the `on_client_publish` event.

<br />

### web.hook.rule.message.delivered.1

| Type     | Default                              |
| -------- | ------------------------------------ |
| string   | `{"action": "on_message_delivered"}` |

##### Description

Forward the `on_message_delivered` event.

<br />

### web.hook.rule.message.acked.1

| Type     | Default                          |
| -------- | -------------------------------- |
| string   | `{"action": "on_message_acked"}` |

##### Description

Forward the `on_message_acked` event.
