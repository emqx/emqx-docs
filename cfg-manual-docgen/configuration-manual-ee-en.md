# Configuration Manual

EMQX Configuration File Manual.

## Node and Cookie

The Erlang/OTP platform application is composed of distributed Erlang nodes (processes). Each Erlang node (process) needs to be assigned a node name for mutual communication between nodes. All Erlang nodes (processes) in communication are authenticated by a shared cookie.

**node.name**

  *Type*: `string`

  *Default*: `emqx@127.0.0.1`

  Unique name of the EMQX node. It must follow <code>%name%@FQDN</code> or
<code>%name%@IPv4</code> format.


**node.cookie**

  *Type*: `string`

  Secret cookie is a random string that should be the same on all nodes in
the given EMQX cluster, but unique per EMQX cluster. It is used to prevent EMQX nodes that
belong to different clusters from accidentally connecting to each other.


**node.max_ports**

  *Type*: `integer`

  *Default*: `1048576`

  *Optional*: `1024-134217727`

  Maximum number of simultaneously open files and sockets for this Erlang system.
For more information, see: https://www.erlang.org/doc/man/erl.html


**node.dist_buffer_size**

  *Type*: `integer`

  *Default*: `8192`

  *Optional*: `1-2097151`

  Erlang's distribution buffer busy limit in kilobytes.


**node.data_dir**

  *Type*: `string`

  Path to the persistent data directory.<br/>
Possible auto-created subdirectories are:<br/>
- `mnesia/<node_name>`: EMQX's built-in database directory.<br/>
For example, `mnesia/emqx@127.0.0.1`.<br/>
There should be only one such subdirectory.<br/>
Meaning, in case the node is to be renamed (to e.g. `emqx@10.0.1.1`),<br/>
the old dir should be deleted first.<br/>
- `configs`: Generated configs at boot time, and cluster/local override configs.<br/>
- `patches`: Hot-patch beam files are to be placed here.<br/>
- `trace`: Trace log files.<br/>

**NOTE**: One data dir cannot be shared by two or more EMQX nodes.


**node.global_gc_interval**

  *Type*: `disabled | duration`

  *Default*: `15m`

  Periodic garbage collection interval. Set to <code>disabled</code> to have it disabled.


**node.role**

  *Type*: `enum`

  *Default*: `core`

  *Optional*: `core | replicant`

  Select a node role.<br/>
<code>core</code> nodes provide durability of the data, and take care of writes.
It is recommended to place core nodes in different racks or different availability zones.<br/>
<code>replicant</code> nodes are ephemeral worker nodes. Removing them from the cluster
doesn't affect database redundancy<br/>
It is recommended to have more replicant nodes than core nodes.<br/>
Note: this parameter only takes effect when the <code>backend</code> is set
to <code>rlog</code>.



## RPC


EMQX uses a library called <code>gen_rpc</code> for inter-broker communication.<br/>
Most of the time the default config should work,
but in case you need to do performance fine-tuning or experiment a bit,
this is where to look.

**rpc.mode**

  *Type*: `enum`

  *Default*: `async`

  *Optional*: `sync | async`

  In <code>sync</code> mode the sending side waits for the ack from the receiving side.


**rpc.protocol**

  *Type*: `enum`

  *Default*: `tcp`

  *Optional*: `tcp | ssl`

  Transport protocol used for inter-broker communication


**rpc.async_batch_size**

  *Type*: `integer`

  *Default*: `256`

  The maximum number of batch messages sent in asynchronous mode.
      Note that this configuration does not work in synchronous mode.


**rpc.port_discovery**

  *Type*: `enum`

  *Default*: `stateless`

  *Optional*: `manual | stateless`

  <code>manual</code>: discover ports by <code>tcp_server_port</code>.<br/>
<code>stateless</code>: discover ports in a stateless manner, using the following algorithm.
If node name is <code>emqxN@127.0.0.1</code>, where the N is an integer,
then the listening port will be 5370 + N.


**rpc.tcp_server_port**

  *Type*: `integer`

  *Default*: `5369`

  Listening port used by RPC local service.<br/>
Note that this config only takes effect when rpc.port_discovery is set to manual.


**rpc.ssl_server_port**

  *Type*: `integer`

  *Default*: `5369`

  Listening port used by RPC local service.<br/>
Note that this config only takes effect when rpc.port_discovery is set to manual
and <code>driver</code> is set to <code>ssl</code>.


**rpc.tcp_client_num**

  *Type*: `integer`

  *Default*: `10`

  *Optional*: `1-256`

  Set the maximum number of RPC communication channels initiated by this node to each remote node.


**rpc.connect_timeout**

  *Type*: `duration`

  *Default*: `5s`

  Timeout for establishing an RPC connection.


**rpc.certfile**

  *Type*: `file`

  Path to TLS certificate file used to validate identity of the cluster nodes.
Note that this config only takes effect when <code>rpc.driver</code> is set to <code>ssl</code>.


**rpc.keyfile**

  *Type*: `file`

  Path to the private key file for the <code>rpc.certfile</code>.<br/>
Note: contents of this file are secret, so it's necessary to set permissions to 600.


**rpc.cacertfile**

  *Type*: `file`

  Path to certification authority TLS certificate file used to validate <code>rpc.certfile</code>.<br/>
Note: certificates of all nodes in the cluster must be signed by the same CA.


**rpc.send_timeout**

  *Type*: `duration`

  *Default*: `5s`

  Timeout for sending the RPC request.


**rpc.authentication_timeout**

  *Type*: `duration`

  *Default*: `5s`

  Timeout for the remote node authentication.


**rpc.call_receive_timeout**

  *Type*: `duration`

  *Default*: `15s`

  Timeout for the reply to a synchronous RPC.


**rpc.socket_keepalive_idle**

  *Type*: `timeout_duration_s`

  *Default*: `15m`

  How long the connections between the brokers should remain open after the last message is sent.


**rpc.socket_keepalive_interval**

  *Type*: `timeout_duration_s`

  *Default*: `75s`

  The interval between keepalive messages.


**rpc.socket_keepalive_count**

  *Type*: `integer`

  *Default*: `9`

  How many times the keepalive probe message can fail to receive a reply
until the RPC connection is considered lost.


**rpc.socket_sndbuf**

  *Type*: `bytesize`

  *Default*: `1MB`

  TCP tuning parameters. TCP sending buffer size.


**rpc.socket_recbuf**

  *Type*: `bytesize`

  *Default*: `1MB`

  TCP tuning parameters. TCP receiving buffer size.


**rpc.socket_buffer**

  *Type*: `bytesize`

  *Default*: `1MB`

  TCP tuning parameters. Socket buffer size in user mode.


**rpc.insecure_fallback**

  *Type*: `boolean`

  *Default*: `true`

  Enable compatibility with old RPC authentication.



## Cluster Setup


EMQX nodes can form a cluster to scale up the total capacity.<br/>
      Here holds the configs to instruct how individual nodes can discover each other.

**cluster.name**

  *Type*: `atom`

  *Default*: `emqxcl`

  Human-friendly name of the EMQX cluster.


**cluster.discovery_strategy**

  *Type*: `enum`

  *Default*: `manual`

  *Optional*: `manual | static | dns | etcd | k8s | mcast`

  Service discovery method for the cluster nodes. Possible values are:
- manual: Use <code>emqx ctl cluster</code> command to manage cluster.<br/>
- static: Configure static nodes list by setting <code>seeds</code> in config file.<br/>
- dns: Use DNS A record to discover peer nodes.<br/>
- etcd: Use etcd to discover peer nodes.<br/>
- k8s: Use Kubernetes API to discover peer pods.
- mcast: Deprecated since 5.1, will be removed in 5.2.
  This supports discovery via UDP multicast.


**cluster.autoclean**

  *Type*: `duration`

  *Default*: `24h`

  Remove disconnected nodes from the cluster after this interval.


**cluster.autoheal**

  *Type*: `boolean`

  *Default*: `true`

  If <code>true</code>, the node will try to heal network partitions automatically.


**cluster.proto_dist**

  *Type*: `enum`

  *Default*: `inet_tcp`

  *Optional*: `inet_tcp | inet6_tcp | inet_tls`

  The Erlang distribution protocol for the cluster.<br/>
- inet_tcp: IPv4 TCP <br/>
- inet_tls: IPv4 TLS, works together with <code>etc/ssl_dist.conf</code>


**cluster.static**

  *Type*: `cluster_static`


**cluster.dns**

  *Type*: `cluster_dns`


**cluster.etcd**

  *Type*: `cluster_etcd`


**cluster.k8s**

  *Type*: `cluster_k8s`



## Cluster Autodiscovery

EMQX supports node discovery and autocluster with various strategies:
see [Create and manage clusters](../deploy/cluster/create-cluster.md)。

| Strategy | Description                     |
| -------- | ------------------------------- |
| manual   | Create cluster manually         |
| static   | Autocluster by static node list |
| mcast    | Autocluster by UDP Multicast    |
| dns      | Autocluster by DNS A Record     |
| etcd     | Autocluster using etcd          |
| k8s      | Autocluster on Kubernetes       |

### Create cluster manually

This is the default configuration of clustering, nodes join a cluster by executing ./bin/emqx_ctl join \<Node> CLI command:

```bash
cluster.discovery = manual
```

### Autocluster by static node list


Service discovery via static nodes.
The new node joins the cluster by connecting to one of the bootstrap nodes.

**cluster.static.seeds**

  *Type*: `comma_separated_atoms | array`

  *Default*: `[]`

  List EMQX node names in the static cluster. See <code>node.name</code>.



### Autocluster by DNS Record


Service discovery via DNS SRV records.

**cluster.dns.name**

  *Type*: `string`

  *Default*: `localhost`

  The domain name from which to discover peer EMQX nodes' IP addresses.
Applicable when <code>cluster.discovery_strategy = dns</code>


**cluster.dns.record_type**

  *Type*: `enum`

  *Default*: `a`

  *Optional*: `a | srv`

  DNS record type.



### Autocluster using etcd


Service discovery using 'etcd' service.

**cluster.etcd.server**

  *Type*: `comma_separated_list`

  List of endpoint URLs of the etcd cluster


**cluster.etcd.prefix**

  *Type*: `string`

  *Default*: `emqxcl`

  Key prefix used for EMQX service discovery.


**cluster.etcd.node_ttl**

  *Type*: `duration`

  *Default*: `1m`

  Expiration time of the etcd key associated with the node.
It is refreshed automatically, as long as the node is alive.


**cluster.etcd.ssl_options**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  Options for the TLS connection to the etcd cluster.



### Autocluster on Kubernetes


Service discovery via Kubernetes API server.

**cluster.k8s.apiserver**

  *Type*: `string`

  *Default*: `https://kubernetes.default.svc:443`

  Kubernetes API endpoint URL.


**cluster.k8s.service_name**

  *Type*: `string`

  *Default*: `emqx`

  EMQX broker service name.


**cluster.k8s.address_type**

  *Type*: `enum`

  *Default*: `ip`

  *Optional*: `ip | dns | hostname`

  Address type used for connecting to the discovered nodes.
Setting <code>cluster.k8s.address_type</code> to <code>ip</code> will
make EMQX to discover IP addresses of peer nodes from Kubernetes API.


**cluster.k8s.namespace**

  *Type*: `string`

  *Default*: `default`

  Kubernetes namespace.


**cluster.k8s.suffix**

  *Type*: `string`

  *Default*: `pod.local`

  Node name suffix.<br/>
Note: this parameter is only relevant when <code>address_type</code> is <code>dns</code>
or <code>hostname</code>.



## Log

Configure the log output location, log level, log file storage path, and parameters such as log rotation and overload protection.

### File Output Log


Log handler that prints log events to files.

**log_file_handler.path**

  *Type*: `file`

  *Default*: `${EMQX_LOG_DIR}/emqx.log`

  Name the log file.


**log_file_handler.rotation_count**

  *Type*: `integer`

  *Default*: `10`

  *Optional*: `1-128`

  Maximum number of log files.


**log_file_handler.rotation_size**

  *Type*: `infinity | bytesize`

  *Default*: `50MB`

  This parameter controls log file rotation. The value `infinity` means the log file will grow indefinitely, otherwise the log file will be rotated once it reaches `rotation_size` in bytes.


**log_file_handler.level**

  *Type*: `log_level`

  *Default*: `warning`

  The log level for the current log handler.
Defaults to warning.


**log_file_handler.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable this log handler.


**log_file_handler.formatter**

  *Type*: `enum`

  *Default*: `text`

  *Optional*: `text | json`

  Choose log formatter. <code>text</code> for free text, and <code>json</code> for structured logging.


**log_file_handler.time_offset**

  *Type*: `string`

  *Default*: `system`

  The time offset to be used when formatting the timestamp.
Can be one of:
  - <code>system</code>: the time offset used by the local system
  - <code>utc</code>: the UTC time offset
  - <code>+-[hh]:[mm]</code>: user specified time offset, such as "-02:00" or "+00:00"
Defaults to: <code>system</code>.



### Console Output Log


Log handler that prints log events to the EMQX console.

**log.console.level**

  *Type*: `log_level`

  *Default*: `warning`

  The log level for the current log handler.
Defaults to warning.


**log.console.enable**

  *Type*: `boolean`

  *Default*: `false`

  Enable this log handler.


**log.console.formatter**

  *Type*: `enum`

  *Default*: `text`

  *Optional*: `text | json`

  Choose log formatter. <code>text</code> for free text, and <code>json</code> for structured logging.


**log.console.time_offset**

  *Type*: `string`

  *Default*: `system`

  The time offset to be used when formatting the timestamp.
Can be one of:
  - <code>system</code>: the time offset used by the local system
  - <code>utc</code>: the UTC time offset
  - <code>+-[hh]:[mm]</code>: user specified time offset, such as "-02:00" or "+00:00"
Defaults to: <code>system</code>.



<!-- ### Log rotation

log_rotation

 -->

<!-- ### Log burst limit

log_burst_limit -->

<!-- ### Log overload kill

log_overload_kill -->

{% emqxee %}

## License


License provisioned as a string.

**license.key**

  *Type*: `string`

  *Default*: `MjIwMTExCjAKMTAKRXZhbHVhdGlvbgpjb250YWN0QGVtcXguaW8KZGVmYXVsdAoyMDIzMDEwOQoxODI1CjEwMAo=.MEUCIG62t8W15g05f1cKx3tA3YgJoR0dmyHOPCdbUxBGxgKKAiEAhHKh8dUwhU+OxNEaOn8mgRDtiT3R8RZooqy6dEsOmDI=`

  License string


**license.connection_low_watermark**

  *Type*: `percent`

  *Default*: `75%`

  Low watermark limit below which license connection quota usage alarms are deactivated


**license.connection_high_watermark**

  *Type*: `percent`

  *Default*: `80%`

  High watermark limit above which license connection quota usage alarms are activated



{% endemqxee %}

## MQTT/TCP Listener - 1883

EMQX supports the creation of multiple listeners, and the default MQTT/TCP listener port is `1883`.

**listeners.tcp.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable listener.


**listeners.tcp.$name.bind**

  *Type*: `ip_port`

  *Default*: `1883`

  IP address and port for the listening socket.


**listeners.tcp.$name.acceptors**

  *Type*: `pos_integer`

  *Default*: `16`

  The size of the listener's receiving pool.


**listeners.tcp.$name.max_connections**

  *Type*: `infinity | pos_integer`

  *Default*: `infinity`

  The maximum number of concurrent connections allowed by the listener.


**listeners.tcp.$name.mountpoint**

  *Type*: `string`

  *Default*: `""`

  When publishing or subscribing, prefix all topics with a mountpoint string.
The prefixed string will be removed from the topic name when the message
is delivered to the subscriber. The mountpoint is a way that users can use
to implement isolation of message routing between different listeners.
For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint`
set to `some_tenant`, then the client actually subscribes to the topic
`some_tenant/t`. Similarly, if another client B (connected to the same listener
as the client A) sends a message to topic `t`, the message is routed
to all the clients subscribed `some_tenant/t`, so client A will receive the
message, with topic name `t`.<br/>
Set to `""` to disable the feature.<br/>

Variables in mountpoint string:
  - <code>${clientid}</code>: clientid
  - <code>${username}</code>: username


**listeners.tcp.$name.enable_authn**

  *Type*: `enum`

  *Default*: `true`

  *Optional*: `true | false | quick_deny_anonymous`

  Set <code>true</code> (default) to enable client authentication on this listener, the authentication
process goes through the configured authentication chain.
When set to <code>false</code> to allow any clients with or without authentication information such as username or password to log in.
When set to <code>quick_deny_anonymous</code>, it behaves like when set to <code>true</code>, but clients will be
denied immediately without going through any authenticators if <code>username</code> is not provided. This is useful to fence off
anonymous clients early.


**listeners.tcp.$name.max_conn_rate**

  *Type*: `rate`

  Maximum connection rate.<br/>
This is used to limit the connection rate for this listener,
once the limit is reached, new connections will be deferred or refused


**listeners.tcp.$name.messages_rate**

  *Type*: `rate`

  Messages publish rate.<br/>
This is used to limit the inbound message numbers for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.


**listeners.tcp.$name.bytes_rate**

  *Type*: `rate`

  Data publish rate.<br/>
This is used to limit the inbound bytes rate for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.


**listeners.tcp.$name.access_rules**

  *Type*: `array`

  *Default*: `["allow all"]`

  The access control rules for this listener.<br/>See: https://github.com/emqtt/esockd#allowdeny


**listeners.tcp.$name.proxy_protocol**

  *Type*: `boolean`

  *Default*: `false`

  Enable the Proxy Protocol V1/2 if the EMQX cluster is deployed behind HAProxy or Nginx.<br/>
See: https://www.haproxy.com/blog/haproxy/proxy-protocol/


**listeners.tcp.$name.proxy_protocol_timeout**

  *Type*: `duration`

  *Default*: `3s`

  Timeout for proxy protocol. EMQX will close the TCP connection if proxy protocol packet is not received within the timeout.


**listeners.tcp.$name.authentication**

  *Type*: `array`

  *Default*: `[]`

  Default authentication configs for all MQTT listeners.

For per-listener overrides see <code>authentication</code> in listener configs

This option can be configured with:
<ul>
  <li><code>[]</code>: The default value, it allows *ALL* logins</li>
  <li>one: For example <code>{enable:true,backend:"built_in_database",mechanism="password_based"}</code></li>
  <li>chain: An array of structs.</li>
</ul>

When a chain is configured, the login credentials are checked against the backends per the configured order, until an 'allow' or 'deny' decision can be made.

If there is no decision after a full chain exhaustion, the login is rejected.


**listeners.tcp.$name.tcp_options**

  *Type*: [broker:tcp_opts](#tcp_opts)



## MQTT/SSL Listener - 8883


Settings for the MQTT over SSL listener.

**listeners.ssl.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable listener.


**listeners.ssl.$name.bind**

  *Type*: `ip_port`

  *Default*: `8883`

  IP address and port for the listening socket.


**listeners.ssl.$name.acceptors**

  *Type*: `pos_integer`

  *Default*: `16`

  The size of the listener's receiving pool.


**listeners.ssl.$name.max_connections**

  *Type*: `infinity | pos_integer`

  *Default*: `infinity`

  The maximum number of concurrent connections allowed by the listener.


**listeners.ssl.$name.mountpoint**

  *Type*: `string`

  *Default*: `""`

  When publishing or subscribing, prefix all topics with a mountpoint string.
The prefixed string will be removed from the topic name when the message
is delivered to the subscriber. The mountpoint is a way that users can use
to implement isolation of message routing between different listeners.
For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint`
set to `some_tenant`, then the client actually subscribes to the topic
`some_tenant/t`. Similarly, if another client B (connected to the same listener
as the client A) sends a message to topic `t`, the message is routed
to all the clients subscribed `some_tenant/t`, so client A will receive the
message, with topic name `t`.<br/>
Set to `""` to disable the feature.<br/>

Variables in mountpoint string:
  - <code>${clientid}</code>: clientid
  - <code>${username}</code>: username


**listeners.ssl.$name.enable_authn**

  *Type*: `enum`

  *Default*: `true`

  *Optional*: `true | false | quick_deny_anonymous`

  Set <code>true</code> (default) to enable client authentication on this listener, the authentication
process goes through the configured authentication chain.
When set to <code>false</code> to allow any clients with or without authentication information such as username or password to log in.
When set to <code>quick_deny_anonymous</code>, it behaves like when set to <code>true</code>, but clients will be
denied immediately without going through any authenticators if <code>username</code> is not provided. This is useful to fence off
anonymous clients early.


**listeners.ssl.$name.max_conn_rate**

  *Type*: `rate`

  Maximum connection rate.<br/>
This is used to limit the connection rate for this listener,
once the limit is reached, new connections will be deferred or refused


**listeners.ssl.$name.messages_rate**

  *Type*: `rate`

  Messages publish rate.<br/>
This is used to limit the inbound message numbers for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.


**listeners.ssl.$name.bytes_rate**

  *Type*: `rate`

  Data publish rate.<br/>
This is used to limit the inbound bytes rate for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.


**listeners.ssl.$name.access_rules**

  *Type*: `array`

  *Default*: `["allow all"]`

  The access control rules for this listener.<br/>See: https://github.com/emqtt/esockd#allowdeny


**listeners.ssl.$name.proxy_protocol**

  *Type*: `boolean`

  *Default*: `false`

  Enable the Proxy Protocol V1/2 if the EMQX cluster is deployed behind HAProxy or Nginx.<br/>
See: https://www.haproxy.com/blog/haproxy/proxy-protocol/


**listeners.ssl.$name.proxy_protocol_timeout**

  *Type*: `duration`

  *Default*: `3s`

  Timeout for proxy protocol. EMQX will close the TCP connection if proxy protocol packet is not received within the timeout.


**listeners.ssl.$name.authentication**

  *Type*: `array`

  *Default*: `[]`

  Default authentication configs for all MQTT listeners.

For per-listener overrides see <code>authentication</code> in listener configs

This option can be configured with:
<ul>
  <li><code>[]</code>: The default value, it allows *ALL* logins</li>
  <li>one: For example <code>{enable:true,backend:"built_in_database",mechanism="password_based"}</code></li>
  <li>chain: An array of structs.</li>
</ul>

When a chain is configured, the login credentials are checked against the backends per the configured order, until an 'allow' or 'deny' decision can be made.

If there is no decision after a full chain exhaustion, the login is rejected.


**listeners.ssl.$name.tcp_options**

  *Type*: [broker:tcp_opts](#tcp_opts)


**listeners.ssl.$name.ssl_options**

  *Type*: [listener_ssl_opts](#ssl-tls-configuration-for-the-listener)



## MQTT Over QUIC/UDP Listener - 14567

Set the MQTT over QUIC UDP listener, which is not enabled by default. And this feature is not available in some operating systems.

For details, please refer to [MQTT over QUIC Quick Start](../mqtt-over-quic/getting-started.md).


Settings for the MQTT over QUIC listener.

**listeners.quic.$name.ciphers**

  *Type*: `array`

  *Default*: `["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256","TLS_CHACHA20_POLY1305_SHA256"]`

  This config holds TLS cipher suite names separated by comma,
or as an array of strings. e.g.
<code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> or
<code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>.
<br/>
Ciphers (and their ordering) define the way in which the
client and server encrypts information over the network connection.
Selecting a good cipher suite is critical for the
application's data security, confidentiality and performance.

The names should be in OpenSSL string format (not RFC format).
All default values and examples provided by EMQX config
documentation are all in OpenSSL format.<br/>

NOTE: Certain cipher suites are only compatible with
specific TLS <code>versions</code> ('tlsv1.1', 'tlsv1.2' or 'tlsv1.3')
incompatible cipher suites will be silently dropped.
For instance, if only 'tlsv1.3' is given in the <code>versions</code>,
configuring cipher suites for other versions will have no effect.
<br/>

NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config<br/>
If PSK cipher suites are intended, 'tlsv1.3' should be disabled from <code>versions</code>.<br/>
PSK cipher suites: <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code><br/>

NOTE: QUIC listener supports only 'tlsv1.3' ciphers


**listeners.quic.$name.ssl_options**

  *Type*: `broker:listener_quic_ssl_opts`

  TLS options for QUIC transport


**listeners.quic.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable listener.


**listeners.quic.$name.bind**

  *Type*: `ip_port`

  *Default*: `14567`

  IP address and port for the listening socket.


**listeners.quic.$name.acceptors**

  *Type*: `pos_integer`

  *Default*: `16`

  The size of the listener's receiving pool.


**listeners.quic.$name.max_connections**

  *Type*: `infinity | pos_integer`

  *Default*: `infinity`

  The maximum number of concurrent connections allowed by the listener.


**listeners.quic.$name.mountpoint**

  *Type*: `string`

  *Default*: `""`

  When publishing or subscribing, prefix all topics with a mountpoint string.
The prefixed string will be removed from the topic name when the message
is delivered to the subscriber. The mountpoint is a way that users can use
to implement isolation of message routing between different listeners.
For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint`
set to `some_tenant`, then the client actually subscribes to the topic
`some_tenant/t`. Similarly, if another client B (connected to the same listener
as the client A) sends a message to topic `t`, the message is routed
to all the clients subscribed `some_tenant/t`, so client A will receive the
message, with topic name `t`.<br/>
Set to `""` to disable the feature.<br/>

Variables in mountpoint string:
  - <code>${clientid}</code>: clientid
  - <code>${username}</code>: username


**listeners.quic.$name.enable_authn**

  *Type*: `enum`

  *Default*: `true`

  *Optional*: `true | false | quick_deny_anonymous`

  Set <code>true</code> (default) to enable client authentication on this listener, the authentication
process goes through the configured authentication chain.
When set to <code>false</code> to allow any clients with or without authentication information such as username or password to log in.
When set to <code>quick_deny_anonymous</code>, it behaves like when set to <code>true</code>, but clients will be
denied immediately without going through any authenticators if <code>username</code> is not provided. This is useful to fence off
anonymous clients early.


**listeners.quic.$name.max_conn_rate**

  *Type*: `rate`

  Maximum connection rate.<br/>
This is used to limit the connection rate for this listener,
once the limit is reached, new connections will be deferred or refused


**listeners.quic.$name.messages_rate**

  *Type*: `rate`

  Messages publish rate.<br/>
This is used to limit the inbound message numbers for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.


**listeners.quic.$name.bytes_rate**

  *Type*: `rate`

  Data publish rate.<br/>
This is used to limit the inbound bytes rate for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.




TLS options for QUIC transport.

**listeners.quic.$name.ssl_options.cacertfile**

  *Type*: `string`

  *Default*: `${EMQX_ETC_DIR}/certs/cacert.pem`

  Trusted PEM format CA certificates bundle file.<br/>
The certificates in this file are used to verify the TLS peer's certificates.
Append new certificates to the file if new CAs are to be trusted.
There is no need to restart EMQX to have the updated file loaded, because
the system regularly checks if file has been updated (and reload).<br/>
NOTE: invalidating (deleting) a certificate from the file will not affect
already established connections.


**listeners.quic.$name.ssl_options.certfile**

  *Type*: `string`

  *Default*: `${EMQX_ETC_DIR}/certs/cert.pem`

  PEM format certificates chain file.<br/>
The certificates in this file should be in reversed order of the certificate
issue chain. That is, the host's certificate should be placed in the beginning
of the file, followed by the immediate issuer certificate and so on.
Although the root CA certificate is optional, it should be placed at the end of
the file if it is to be added.


**listeners.quic.$name.ssl_options.keyfile**

  *Type*: `string`

  *Default*: `${EMQX_ETC_DIR}/certs/key.pem`

  PEM format private key file.


**listeners.quic.$name.ssl_options.verify**

  *Type*: `enum`

  *Default*: `verify_none`

  *Optional*: `verify_peer | verify_none`

  Enable or disable peer verification.


**listeners.quic.$name.ssl_options.password**

  *Type*: `string`

  String containing the user's password. Only used if the private key file is password-protected.



## MQTT/WebSocket Listener - 8083


Settings for the MQTT over WebSocket listener.

**listeners.ws.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable listener.


**listeners.ws.$name.bind**

  *Type*: `ip_port`

  *Default*: `8083`

  IP address and port for the listening socket.


**listeners.ws.$name.acceptors**

  *Type*: `pos_integer`

  *Default*: `16`

  The size of the listener's receiving pool.


**listeners.ws.$name.max_connections**

  *Type*: `infinity | pos_integer`

  *Default*: `infinity`

  The maximum number of concurrent connections allowed by the listener.


**listeners.ws.$name.mountpoint**

  *Type*: `string`

  *Default*: `""`

  When publishing or subscribing, prefix all topics with a mountpoint string.
The prefixed string will be removed from the topic name when the message
is delivered to the subscriber. The mountpoint is a way that users can use
to implement isolation of message routing between different listeners.
For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint`
set to `some_tenant`, then the client actually subscribes to the topic
`some_tenant/t`. Similarly, if another client B (connected to the same listener
as the client A) sends a message to topic `t`, the message is routed
to all the clients subscribed `some_tenant/t`, so client A will receive the
message, with topic name `t`.<br/>
Set to `""` to disable the feature.<br/>

Variables in mountpoint string:
  - <code>${clientid}</code>: clientid
  - <code>${username}</code>: username


**listeners.ws.$name.enable_authn**

  *Type*: `enum`

  *Default*: `true`

  *Optional*: `true | false | quick_deny_anonymous`

  Set <code>true</code> (default) to enable client authentication on this listener, the authentication
process goes through the configured authentication chain.
When set to <code>false</code> to allow any clients with or without authentication information such as username or password to log in.
When set to <code>quick_deny_anonymous</code>, it behaves like when set to <code>true</code>, but clients will be
denied immediately without going through any authenticators if <code>username</code> is not provided. This is useful to fence off
anonymous clients early.


**listeners.ws.$name.max_conn_rate**

  *Type*: `rate`

  Maximum connection rate.<br/>
This is used to limit the connection rate for this listener,
once the limit is reached, new connections will be deferred or refused


**listeners.ws.$name.messages_rate**

  *Type*: `rate`

  Messages publish rate.<br/>
This is used to limit the inbound message numbers for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.


**listeners.ws.$name.bytes_rate**

  *Type*: `rate`

  Data publish rate.<br/>
This is used to limit the inbound bytes rate for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.


**listeners.ws.$name.access_rules**

  *Type*: `array`

  *Default*: `["allow all"]`

  The access control rules for this listener.<br/>See: https://github.com/emqtt/esockd#allowdeny


**listeners.ws.$name.proxy_protocol**

  *Type*: `boolean`

  *Default*: `false`

  Enable the Proxy Protocol V1/2 if the EMQX cluster is deployed behind HAProxy or Nginx.<br/>
See: https://www.haproxy.com/blog/haproxy/proxy-protocol/


**listeners.ws.$name.proxy_protocol_timeout**

  *Type*: `duration`

  *Default*: `3s`

  Timeout for proxy protocol. EMQX will close the TCP connection if proxy protocol packet is not received within the timeout.


**listeners.ws.$name.authentication**

  *Type*: `array`

  *Default*: `[]`

  Default authentication configs for all MQTT listeners.

For per-listener overrides see <code>authentication</code> in listener configs

This option can be configured with:
<ul>
  <li><code>[]</code>: The default value, it allows *ALL* logins</li>
  <li>one: For example <code>{enable:true,backend:"built_in_database",mechanism="password_based"}</code></li>
  <li>chain: An array of structs.</li>
</ul>

When a chain is configured, the login credentials are checked against the backends per the configured order, until an 'allow' or 'deny' decision can be made.

If there is no decision after a full chain exhaustion, the login is rejected.


**listeners.ws.$name.tcp_options**

  *Type*: [broker:tcp_opts](#tcp_opts)


**listeners.ws.$name.websocket**

  *Type*: [broker:ws_opts](#ws_opts)



## MQTT/WebSocket with SSL Listener - 8084


Settings for the MQTT over WebSocket/SSL listener.

**listeners.wss.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable listener.


**listeners.wss.$name.bind**

  *Type*: `ip_port`

  *Default*: `8084`

  IP address and port for the listening socket.


**listeners.wss.$name.acceptors**

  *Type*: `pos_integer`

  *Default*: `16`

  The size of the listener's receiving pool.


**listeners.wss.$name.max_connections**

  *Type*: `infinity | pos_integer`

  *Default*: `infinity`

  The maximum number of concurrent connections allowed by the listener.


**listeners.wss.$name.mountpoint**

  *Type*: `string`

  *Default*: `""`

  When publishing or subscribing, prefix all topics with a mountpoint string.
The prefixed string will be removed from the topic name when the message
is delivered to the subscriber. The mountpoint is a way that users can use
to implement isolation of message routing between different listeners.
For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint`
set to `some_tenant`, then the client actually subscribes to the topic
`some_tenant/t`. Similarly, if another client B (connected to the same listener
as the client A) sends a message to topic `t`, the message is routed
to all the clients subscribed `some_tenant/t`, so client A will receive the
message, with topic name `t`.<br/>
Set to `""` to disable the feature.<br/>

Variables in mountpoint string:
  - <code>${clientid}</code>: clientid
  - <code>${username}</code>: username


**listeners.wss.$name.enable_authn**

  *Type*: `enum`

  *Default*: `true`

  *Optional*: `true | false | quick_deny_anonymous`

  Set <code>true</code> (default) to enable client authentication on this listener, the authentication
process goes through the configured authentication chain.
When set to <code>false</code> to allow any clients with or without authentication information such as username or password to log in.
When set to <code>quick_deny_anonymous</code>, it behaves like when set to <code>true</code>, but clients will be
denied immediately without going through any authenticators if <code>username</code> is not provided. This is useful to fence off
anonymous clients early.


**listeners.wss.$name.max_conn_rate**

  *Type*: `rate`

  Maximum connection rate.<br/>
This is used to limit the connection rate for this listener,
once the limit is reached, new connections will be deferred or refused


**listeners.wss.$name.messages_rate**

  *Type*: `rate`

  Messages publish rate.<br/>
This is used to limit the inbound message numbers for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.


**listeners.wss.$name.bytes_rate**

  *Type*: `rate`

  Data publish rate.<br/>
This is used to limit the inbound bytes rate for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.


**listeners.wss.$name.access_rules**

  *Type*: `array`

  *Default*: `["allow all"]`

  The access control rules for this listener.<br/>See: https://github.com/emqtt/esockd#allowdeny


**listeners.wss.$name.proxy_protocol**

  *Type*: `boolean`

  *Default*: `false`

  Enable the Proxy Protocol V1/2 if the EMQX cluster is deployed behind HAProxy or Nginx.<br/>
See: https://www.haproxy.com/blog/haproxy/proxy-protocol/


**listeners.wss.$name.proxy_protocol_timeout**

  *Type*: `duration`

  *Default*: `3s`

  Timeout for proxy protocol. EMQX will close the TCP connection if proxy protocol packet is not received within the timeout.


**listeners.wss.$name.authentication**

  *Type*: `array`

  *Default*: `[]`

  Default authentication configs for all MQTT listeners.

For per-listener overrides see <code>authentication</code> in listener configs

This option can be configured with:
<ul>
  <li><code>[]</code>: The default value, it allows *ALL* logins</li>
  <li>one: For example <code>{enable:true,backend:"built_in_database",mechanism="password_based"}</code></li>
  <li>chain: An array of structs.</li>
</ul>

When a chain is configured, the login credentials are checked against the backends per the configured order, until an 'allow' or 'deny' decision can be made.

If there is no decision after a full chain exhaustion, the login is rejected.


**listeners.wss.$name.tcp_options**

  *Type*: [broker:tcp_opts](#tcp_opts)


**listeners.wss.$name.ssl_options**

  *Type*: [broker:listener_wss_opts](#listener_wss_opts)


**listeners.wss.$name.websocket**

  *Type*: [broker:ws_opts](#ws_opts)



## MQTT Basic Parameters

Global MQTT configuration parameters.


Global MQTT configuration.

**mqtt.idle_timeout**

  *Type*: `infinity | duration`

  *Default*: `15s`

  Configure the duration of time that a connection can remain idle (i.e., without any data transfer) before being:
  - Automatically disconnected  if no CONNECT package is received from the client yet.
  - Put into hibernation mode to save resources if some CONNECT packages are already received.
Note: Please set the parameter with caution as long idle time will lead to resource waste.


**mqtt.max_packet_size**

  *Type*: `bytesize`

  *Default*: `1MB`

  Maximum MQTT packet size allowed. Default: 1 MB, Maximum: 256 MB


**mqtt.max_clientid_len**

  *Type*: `integer`

  *Default*: `65535`

  *Optional*: `23-65535`

  Maximum allowed length of MQTT Client ID.


**mqtt.max_topic_levels**

  *Type*: `integer`

  *Default*: `128`

  *Optional*: `1-65535`

  Maximum topic levels allowed.


**mqtt.max_topic_alias**

  *Type*: `integer`

  *Default*: `65535`

  *Optional*: `0-65535`

  Maximum topic alias, 0 means no topic alias supported.


**mqtt.retain_available**

  *Type*: `boolean`

  *Default*: `true`

  Whether to enable support for MQTT retained message.


**mqtt.wildcard_subscription**

  *Type*: `boolean`

  *Default*: `true`

  Whether to enable support for MQTT wildcard subscription.


**mqtt.shared_subscription**

  *Type*: `boolean`

  *Default*: `true`

  Whether to enable support for MQTT shared subscription.


**mqtt.shared_subscription_strategy**

  *Type*: `enum`

  *Default*: `round_robin`

  *Optional*: `random | round_robin | round_robin_per_group | sticky | local | hash_topic | hash_clientid`

  Dispatch strategy for shared subscription.
  - `random`: dispatch the message to a random selected subscriber
  - `round_robin`: select the subscribers in a round-robin manner
  - `round_robin_per_group`: select the subscribers in round-robin fashion within each shared subscriber group
  - `local`: select random local subscriber otherwise select random cluster-wide
  - `sticky`: always use the last selected subscriber to dispatch, until the subscriber disconnects.
  - `hash_clientid`: select the subscribers by hashing the `clientIds`
  - `hash_topic`: select the subscribers by hashing the source topic


**mqtt.exclusive_subscription**

  *Type*: `boolean`

  *Default*: `false`

  Whether to enable support for MQTT exclusive subscription.


**mqtt.ignore_loop_deliver**

  *Type*: `boolean`

  *Default*: `false`

  Whether the messages sent by the MQTT v3.1.1/v3.1.0 client will be looped back to the publisher itself, similar to <code>No Local</code> in MQTT 5.0.


**mqtt.strict_mode**

  *Type*: `boolean`

  *Default*: `false`

  Whether to parse MQTT messages in strict mode.
In strict mode, invalid utf8 strings in for example client ID, topic name, etc. will cause the client to be disconnected.


**mqtt.response_information**

  *Type*: `string`

  *Default*: `""`

  UTF-8 string, for creating the response topic, for example, if set to <code>reqrsp/</code>, the publisher/subscriber will communicate using the topic prefix <code>reqrsp/</code>.
To disable this feature, input <code>""</code> in the text box below. Only applicable to MQTT 5.0 clients.


**mqtt.server_keepalive**

  *Type*: `pos_integer | disabled`

  *Default*: `disabled`

  The keep alive duration required by EMQX. To use the setting from the client side, choose disabled from the drop-down list. Only applicable to MQTT 5.0 clients.


**mqtt.keepalive_multiplier**

  *Type*: `number`

  *Default*: `1.5`

  Keep-Alive Timeout = Keep-Alive interval × Keep-Alive Multiplier.
The default value 1.5 is following the MQTT 5.0 specification. This multiplier is adjustable, providing system administrators flexibility for tailoring to their specific needs. For instance, if a client's 10-second Keep-Alive interval PINGREQ gets delayed by an extra 10 seconds, changing the multiplier to 2 lets EMQX tolerate this delay.


**mqtt.retry_interval**

  *Type*: `duration`

  *Default*: `30s`

  Retry interval for QoS 1/2 message delivering.


**mqtt.use_username_as_clientid**

  *Type*: `boolean`

  *Default*: `false`

  Whether to use Username as Client ID.
This setting takes effect later than <code>Use Peer Certificate as Username</code> and <code>Use peer certificate as Client ID</code>.


**mqtt.peer_cert_as_username**

  *Type*: `enum`

  *Default*: `disabled`

  *Optional*: `disabled | cn | dn | crt | pem | md5`

  Use the CN, DN field in the peer certificate or the entire certificate content as Username. Only works for the TLS connection.
Supported configurations are the following:
- <code>cn</code>: CN field of the certificate
- <code>dn</code>: DN field of the certificate
- <code>crt</code>: Content of the <code>DER</code> or <code>PEM</code> certificate
- <code>pem</code>: Convert <code>DER</code> certificate content to <code>PEM</code> format and use as Username
- <code>md5</code>: MD5 value of the <code>DER</code> or <code>PEM</code> certificate


**mqtt.peer_cert_as_clientid**

  *Type*: `enum`

  *Default*: `disabled`

  *Optional*: `disabled | cn | dn | crt | pem | md5`

  Use the CN, DN field in the peer certificate or the entire certificate content as Client ID. Only works for the TLS connection.
Supported configurations are the following:
- <code>cn</code>: CN field of the certificate
- <code>dn</code>: DN field of the certificate
- <code>crt</code>: <code>DER</code> or <code>PEM</code> certificate
- <code>pem</code>: Convert <code>DER</code> certificate content to <code>PEM</code> format and use as Client ID
- <code>md5</code>: MD5 value of the <code>DER</code> or <code>PEM</code> certificate


**mqtt.session_expiry_interval**

  *Type*: `duration`

  *Default*: `2h`

  Specifies how long the session will expire after the connection is disconnected, only for non-MQTT 5.0 connections.


**mqtt.max_awaiting_rel**

  *Type*: `non_neg_integer | infinity`

  *Default*: `100`

  For each publisher session, the maximum number of outstanding QoS 2 messages pending on the client to send PUBREL. After reaching this limit, new QoS 2 PUBLISH requests will be rejected with `147(0x93)` until either PUBREL is received or timed out.


**mqtt.max_qos_allowed**

  *Type*: `qos`

  *Default*: `2`

  Maximum QoS allowed.


**mqtt.mqueue_priorities**

  *Type*: `disabled | map`

  *Default*: `disabled`

  Topic priorities. Priority number [1-255]
There's no priority table by default, hence all messages are treated equal.

**NOTE**: Comma and equal signs are not allowed for priority topic names.
**NOTE**: Messages for topics not in the priority table are treated as either highest or lowest priority depending on the configured value for <code>mqtt.mqueue_default_priority</code>.

**Examples**:
To configure <code>"topic/1" > "topic/2"</code>:
<code>mqueue_priorities: {"topic/1": 10, "topic/2": 8}</code>


**mqtt.mqueue_default_priority**

  *Type*: `enum`

  *Default*: `lowest`

  *Optional*: `highest | lowest`

  Default topic priority, which will be used by topics not in <code>Topic Priorities</code> (<code>mqueue_priorities</code>).


**mqtt.mqueue_store_qos0**

  *Type*: `boolean`

  *Default*: `true`

  Specifies whether to store QoS 0 messages in the message queue while the connection is down but the session remains.


**mqtt.max_mqueue_len**

  *Type*: `non_neg_integer | infinity`

  *Default*: `1000`

  Maximum queue length. Enqueued messages when persistent client disconnected, or inflight window is full.


**mqtt.max_inflight**

  *Type*: `integer`

  *Default*: `32`

  *Optional*: `1-65535`

  Maximum number of QoS 1 and QoS 2 messages that are allowed to be delivered simultaneously before completing the acknowledgment.


**mqtt.max_subscriptions**

  *Type*: `1..inf | infinity`

  *Default*: `infinity`

  Maximum number of subscriptions allowed per client.


**mqtt.upgrade_qos**

  *Type*: `boolean`

  *Default*: `false`

  Force upgrade of QoS level according to subscription.


**mqtt.await_rel_timeout**

  *Type*: `duration`

  *Default*: `300s`

  For client to broker QoS 2 message, the time limit for the broker to wait before the `PUBREL` message is received. The wait is aborted after timed out, meaning the packet ID is freed for new `PUBLISH` requests. Receiving a stale `PUBREL` causes a warning level log. Note, the message is delivered to subscribers before entering the wait for PUBREL.



<!-- TODO zone 的处理 -->

<!-- #topology# -->

### Retainer


Configuration related to handling `PUBLISH` packets with a `retain` flag set to 1.

**retainer.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable retainer feature


**retainer.msg_expiry_interval**

  *Type*: `duration_ms`

  *Default*: `0s`

  Message retention time. This config is only applicable for messages without the Message Expiry Interval message property.
0 means message will never expire.


**retainer.msg_clear_interval**

  *Type*: `timeout_duration_ms`

  *Default*: `0s`

  Interval for EMQX to scan expired messages and delete them. Never scan if the value is 0.


**retainer.max_payload_size**

  *Type*: `bytesize`

  *Default*: `1MB`

  Maximum retained message size.


**retainer.stop_publish_clear_msg**

  *Type*: `boolean`

  *Default*: `false`

  When the retained flag of the `PUBLISH` message is set and Payload is empty,
whether to continue to publish the message.
See:
http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718038


**retainer.delivery_rate**

  *Type*: `rate`

  The maximum rate of delivering retained messages


**retainer.backend**

  *Type*: `retainer:mnesia_config`

  Settings for the database storing the retained messages.



<!-- retainer:flow_control -->


Configuration of the internal database storing retained messages.

**retainer.backend.type**

  *Type*: `built_in_database`

  *Default*: `built_in_database`

  Backend type.


**retainer.backend.storage_type**

  *Type*: `enum`

  *Default*: `ram`

  *Optional*: `ram | disc`

  Specifies whether the messages are stored in RAM or persisted on disc.


**retainer.backend.max_retained_messages**

  *Type*: `non_neg_integer`

  *Default*: `0`

  Maximum number of retained messages. 0 means no limit.


**retainer.backend.index_specs**

  *Type*: `[[integer]]`

  *Default*: `[[1,2,3],[1,3],[2,3],[3]]`

  Retainer index specifications: list of arrays of positive ascending integers. Each array specifies an index. Numbers in an index specification are 1-based word positions in topics. Words from specified positions will be used for indexing.<br/>For example, it is good to have <code>[2, 4]</code> index to optimize <code>+/X/+/Y/...</code> topic wildcard subscriptions.



### Shared subscription

You can set to enable or disable shared subscription configuration via `mqtt.shared_subscription` or `zone.$name.shared_subscription` item.

<!-- broker:shared_subscription_group@ -->

### System topics


The EMQX Broker periodically publishes its own status, message statistics,
client online and offline events to the system topic starting with `$SYS/`.

The following options control the behavior of `$SYS` topics.

**sys_topics.sys_msg_interval**

  *Type*: `disabled | duration`

  *Default*: `1m`

  Time interval for publishing following system messages:
  - `$SYS/brokers`
  - `$SYS/brokers/<node>/version`
  - `$SYS/brokers/<node>/sysdescr`
  - `$SYS/brokers/<node>/stats/<name>`
  - `$SYS/brokers/<node>/metrics/<name>`


**sys_topics.sys_heartbeat_interval**

  *Type*: `disabled | duration`

  *Default*: `30s`

  Time interval for publishing following heartbeat messages:
  - `$SYS/brokers/<node>/uptime`
  - `$SYS/brokers/<node>/datetime`


**sys_topics.sys_event_messages**

  *Type*: `broker:event_names`

  Client events messages.



## MQTT Adds-on

### Delayed publish


Settings for the delayed module.

**delayed.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable this feature


**delayed.max_delayed_messages**

  *Type*: `integer`

  *Default*: `0`

  Maximum number of delayed messages (0 is no limit).



<!-- ### Topic rewrite

modules:rewrite@ -->

<!-- ### Auto subscribe

auto_subscribe@

auto_subscribe:topic@ -->

<!-- ## Log Trace

broker:trace@ -->

{% emqxee %}

## MQTT File Transfer

### File transfer settings


File transfer settings

**file_transfer.enable**

  *Type*: `boolean`

  *Default*: `false`

  Enable the File Transfer feature.<br/>
Enabling File Transfer implies reserving special MQTT topics in order to serve the protocol.<br/>
This toggle also affects the availability of the File Transfer REST API and
storage-dependent background activities (e.g. garbage collection).


**file_transfer.init_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `10s`

  Timeout for EMQX to initialize the file transfer.<br/>
After reaching the timeout (e.g. due to system is overloaded), the PUBACK message for `init` will contain error code (0x80).


**file_transfer.store_segment_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5m`

  Timeout for storing a file segment.<br/>
After reaching the timeout (e.g. due to system overloaded), the PUBACK message will contain error code (0x80).


**file_transfer.assemble_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5m`

  Timeout for assembling and exporting file segments into a final file.<br/>
After reaching the timeout (e.g. due to system is overloaded), the PUBACK message for `fin` will contain error code (0x80)


**file_transfer.storage**

  *Type*: `file_transfer:storage_backend`

  *Default*: `{"local":{}}`

  Storage settings for file transfer.




File transfer local storage settings

**file_transfer.storage.local.segments**

  *Type*: `file_transfer:local_storage_segments`

  *Default*: `{"gc":{}}`

  Settings for local segments storage, which include uploaded transfer fragments and temporary data.


**file_transfer.storage.local.exporter**

  *Type*: `file_transfer:local_storage_exporter_backend`

  *Default*: `{"local":{}}`

  Exporter for the local file system storage backend.<br/>
Exporter defines where and how fully transferred and assembled files are stored.


**file_transfer.storage.local.enable**

  *Type*: `boolean`

  *Default*: `true`

  Whether to enable this backend.




File transfer local segments storage settings

**file_transfer.storage.local.segments.root**

  *Type*: `string`

  File system path to keep uploaded fragments and temporary data.


**file_transfer.storage.local.segments.gc**

  *Type*: `file_transfer:local_storage_segments_gc`

  Garbage collection settings for the intermediate and temporary files in the local file system.




Garbage collection settings for the File transfer local segments storage

**file_transfer.storage.local.segments.gc.interval**

  *Type*: `timeout_duration_ms`

  *Default*: `1h`

  Interval of periodic garbage collection.


**file_transfer.storage.local.segments.gc.maximum_segments_ttl**

  *Type*: `duration_s`

  *Default*: `24h`

  Maximum TTL of a segment kept in the local file system.<br/>
This is a hard limit: no segment will outlive this TTL, even if some file transfer specifies a
TTL more than that.


**file_transfer.storage.local.segments.gc.minimum_segments_ttl**

  *Type*: `duration_s`

  *Default*: `5m`

  Minimum TTL of a segment kept in the local file system.<br/>
This is a hard limit: no segment will be garbage collected before reaching this TTL,
even if some file transfer specifies a TTL less than that.



### Export files to local storage


Local Exporter settings for the File transfer local storage backend

**file_transfer.storage.local.exporter.local.root**

  *Type*: `string`

  Directory where the uploaded files are kept.


**file_transfer.storage.local.exporter.local.enable**

  *Type*: `boolean`

  *Default*: `true`

  Whether to enable this backend.




Exporter for the local file system storage backend

**file_transfer.storage.local.exporter.local**

  *Type*: `file_transfer:local_storage_exporter`

  Exporter to the local file system.


**file_transfer.storage.local.exporter.s3**

  *Type*: `file_transfer:s3_exporter`

  Exporter to the S3 API compatible object storage.



### Export files to S3 storage


S3 Exporter settings for the File transfer local storage backend

**file_transfer.storage.local.exporter.s3.access_key_id**

  *Type*: `string`

  The access key ID of the S3 bucket.


**file_transfer.storage.local.exporter.s3.secret_access_key**

  *Type*: `emqx_s3_schema:secret_access_key`

  The secret access key of the S3 bucket.


**file_transfer.storage.local.exporter.s3.bucket**

  *Type*: `string`

  The name of the S3 bucket.


**file_transfer.storage.local.exporter.s3.host**

  *Type*: `string`

  The host of the S3 endpoint.


**file_transfer.storage.local.exporter.s3.port**

  *Type*: `pos_integer`

  The port of the S3 endpoint.


**file_transfer.storage.local.exporter.s3.url_expire_time**

  *Type*: `duration_s`

  *Default*: `1h`

  The time in seconds for which the signed URLs to the S3 objects are valid.


**file_transfer.storage.local.exporter.s3.min_part_size**

  *Type*: `bytesize`

  *Default*: `5mb`

  The minimum part size for multipart uploads.<br/>
Uploaded data will be accumulated in memory until this size is reached.


**file_transfer.storage.local.exporter.s3.max_part_size**

  *Type*: `bytesize`

  *Default*: `5gb`

  The maximum part size for multipart uploads.<br/>
S3 uploader won't try to upload parts larger than this size.


**file_transfer.storage.local.exporter.s3.acl**

  *Type*: `enum`

  *Optional*: `private | public_read | public_read_write | authenticated_read | bucket_owner_read | bucket_owner_full_control`

  The ACL to use for the uploaded objects.


**file_transfer.storage.local.exporter.s3.transport_options**

  *Type*: `s3:transport_options`

  Options for the HTTP transport layer used by the S3 client.


**file_transfer.storage.local.exporter.s3.enable**

  *Type*: `boolean`

  *Default*: `true`

  Whether to enable this backend.




Storage backend settings for file transfer

**file_transfer.storage.local**

  *Type*: `file_transfer:local_storage`

  Local file system backend to store uploaded fragments and temporary data.




Options for the HTTP transport layer used by the S3 client

**file_transfer.storage.local.exporter.s3.transport_options.ipv6_probe**

  *Type*: `boolean`

  *Default*: `false`

  Whether to probe for IPv6 support.


**file_transfer.storage.local.exporter.s3.transport_options.connect_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  The timeout when connecting to the HTTP server.


**file_transfer.storage.local.exporter.s3.transport_options.pool_type**

  *Type*: `emqx_bridge_http_connector:pool_type`

  *Default*: `random`

  The type of the pool. Can be one of `random`, `hash`.


**file_transfer.storage.local.exporter.s3.transport_options.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  The pool size.


**file_transfer.storage.local.exporter.s3.transport_options.enable_pipelining**

  *Type*: `pos_integer`

  *Default*: `100`

  A positive integer. Whether to send HTTP requests continuously, when set to 1, it means that after each HTTP request is sent, you need to wait for the server to return and then continue to send the next request.


**file_transfer.storage.local.exporter.s3.transport_options.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.


**file_transfer.storage.local.exporter.s3.transport_options.headers**

  *Type*: `map`

  List of HTTP headers.


**file_transfer.storage.local.exporter.s3.transport_options.max_retries**

  *Type*: `non_neg_integer`

  Max retry times if error on sending request.


**file_transfer.storage.local.exporter.s3.transport_options.request_timeout**

  *Type*: `timeout_duration_ms`

  HTTP request timeout.



{% endemqxee %}


## Integration With Prometheus


EMQX's Prometheus scraping endpoint is enabled by default without authentication.
You can inspect it with a `curl` command like this: `curl -f "127.0.0.1:18083/api/v5/prometheus/stats"`<br/>
The 'enable' flag is used to turn on and off for the push-gateway integration.

**prometheus.push_gateway_server**

  *Type*: `string`

  *Default*: `http://127.0.0.1:9091`

  URL of Prometheus server. Pushgateway is optional, should not be configured if prometheus is to scrape EMQX.


**prometheus.interval**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  Data reporting interval


**prometheus.headers**

  *Type*: `[{string, string()}]`

  *Default*: `{}`

  An HTTP Headers when pushing to Push Gateway.<br/>
For example, <code> { Authorization = "some-authz-tokens"}</code>


**prometheus.job_name**

  *Type*: `string`

  *Default*: `${name}/instance/${name}~${host}`

  Job Name that is pushed to the Push Gateway. Available variables:<br/>
- ${name}: Name of EMQX node.<br/>
- ${host}: Host name of EMQX node.<br/>
For example, when the EMQX node name is <code>emqx@127.0.0.1</code> then the <code>name</code> variable takes value <code>emqx</code> and the <code>host</code> variable takes value <code>127.0.0.1</code>.<br/>
Default value is: <code>${name}/instance/${name}~${host}</code>


**prometheus.enable**

  *Type*: `boolean`

  *Default*: `false`

  Turn Prometheus data pushing on or off


**prometheus.vm_dist_collector**

  *Type*: `enum`

  *Default*: `disabled`

  *Optional*: `disabled | enabled`

  Enable or disable VM distribution collector, collects information about the sockets and processes involved in the Erlang distribution mechanism.


**prometheus.mnesia_collector**

  *Type*: `enum`

  *Default*: `disabled`

  *Optional*: `enabled | disabled`

  Enable or disable Mnesia metrics collector


**prometheus.vm_statistics_collector**

  *Type*: `enum`

  *Default*: `disabled`

  *Optional*: `enabled | disabled`

  Enable or disable VM statistics collector.


**prometheus.vm_system_info_collector**

  *Type*: `enum`

  *Default*: `disabled`

  *Optional*: `enabled | disabled`

  Enable or disable VM system info collector.


**prometheus.vm_memory_collector**

  *Type*: `enum`

  *Default*: `disabled`

  *Optional*: `enabled | disabled`

  Enable or disable VM memory metrics collector.


**prometheus.vm_msacc_collector**

  *Type*: `enum`

  *Default*: `disabled`

  *Optional*: `enabled | disabled`

  Enable or disable VM microstate accounting metrics collector.



## Integration With OpenTelemetry


Open Telemetry Toolkit configuration

**opentelemetry.exporter**

  *Type*: `opentelemetry:exporter`

  Open Telemetry Exporter


**opentelemetry.enable**

  *Type*: `boolean`

  *Default*: `false`

  Enable or disable open telemetry metrics




Open Telemetry Exporter

**opentelemetry.exporter.endpoint**

  *Type*: `url`

  *Default*: `http://localhost:4317`

  Open Telemetry Exporter Endpoint


**opentelemetry.exporter.interval**

  *Type*: `timeout_duration_ms`

  *Default*: `10s`

  The interval of sending metrics to Open Telemetry Endpoint




<!-- ## Integration with StatsD

statsd@ -->

## Slow subscriptions

Slow subscription message latency threshold and statistics policy configuration.

**slow_subs.enable**

  *Type*: `boolean`

  *Default*: `false`

  Enable this feature


**slow_subs.threshold**

  *Type*: `duration_ms`

  *Default*: `500ms`

  The latency threshold for statistics


**slow_subs.expire_interval**

  *Type*: `duration_ms`

  *Default*: `300s`

  The eviction time of the record, which in the statistics record table


**slow_subs.top_k_num**

  *Type*: `pos_integer`

  *Default*: `10`

  The maximum number of records in the slow subscription statistics record table


**slow_subs.stats_type**

  *Type*: `enum`

  *Default*: `whole`

  *Optional*: `whole | internal | response`

  The method to calculate the latency



<!-- ## Topic metrics

Configure the topics that require statistics for detailed message flow data.

modules:topic_metrics@ -->

## Alarms and Monitoring


Settings for the alarms.

**alarm.actions**

  *Type*: `array`

  *Default*: `["log","publish"]`

  The actions triggered when the alarm is activated.<br/>Currently, the following actions are supported: <code>log</code> and <code>publish</code>.
<code>log</code> is to write the alarm to log (console or file).
<code>publish</code> is to publish the alarm as an MQTT message to the system topics:
<code>$SYS/brokers/emqx@xx.xx.xx.x/alarms/activate</code> and
<code>$SYS/brokers/emqx@xx.xx.xx.x/alarms/deactivate</code>


**alarm.size_limit**

  *Type*: `integer`

  *Default*: `1000`

  *Optional*: `1-3000`

  The maximum total number of deactivated alarms to keep as history.<br/>When this limit is exceeded, the oldest deactivated alarms are deleted to cap the total number.


**alarm.validity_period**

  *Type*: `duration`

  *Default*: `24h`

  Retention time of deactivated alarms. Alarms are not deleted immediately
when deactivated, but after the retention time.



### Alarm Threshold

<!-- #broker:sysmon# -->


This part of the configuration is responsible for monitoring
 the host OS health, such as free memory, disk space, CPU load, etc.

**sysmon.os.cpu_check_interval**

  *Type*: `duration`

  *Default*: `60s`

  The time interval for the periodic CPU check. Disabled on Windows platform.


**sysmon.os.cpu_high_watermark**

  *Type*: `percent`

  *Default*: `80%`

  The threshold, as percentage of system CPU load,
 for how much system cpu can be used before the corresponding alarm is raised. Disabled on Windows platform


**sysmon.os.cpu_low_watermark**

  *Type*: `percent`

  *Default*: `60%`

  The threshold, as percentage of system CPU load,
 for how much system cpu can be used before the corresponding alarm is cleared. Disabled on Windows platform


**sysmon.os.mem_check_interval**

  *Type*: `disabled | duration`

  *Default*: `disabled`

  The time interval for the periodic memory check. Disabled on Windows platform.


**sysmon.os.sysmem_high_watermark**

  *Type*: `percent`

  *Default*: `70%`

  The threshold, as percentage of system memory,
 for how much system memory can be allocated before the corresponding alarm is raised. Disabled on Windows platform


**sysmon.os.procmem_high_watermark**

  *Type*: `percent`

  *Default*: `5%`

  The threshold, as percentage of system memory,
 for how much system memory can be allocated by one Erlang process before
 the corresponding alarm is raised. Disabled on Windows platform.



<!-- broker:sysmon_top@ -->


This part of the configuration is responsible for collecting
 BEAM VM events, such as long garbage collection, traffic congestion in the inter-broker
 communication, etc.

**sysmon.vm.process_check_interval**

  *Type*: `duration`

  *Default*: `30s`

  The time interval for the periodic process limit check.


**sysmon.vm.process_high_watermark**

  *Type*: `percent`

  *Default*: `80%`

  The threshold, as percentage of processes, for how many
 processes can simultaneously exist at the local node before the corresponding
 alarm is raised.


**sysmon.vm.process_low_watermark**

  *Type*: `percent`

  *Default*: `60%`

  The threshold, as percentage of processes, for how many
 processes can simultaneously exist at the local node before the corresponding
 alarm is cleared.


**sysmon.vm.long_gc**

  *Type*: `disabled | duration`

  *Default*: `disabled`

  When an Erlang process spends long time to perform garbage collection, a warning level <code>long_gc</code> log is emitted,
and an MQTT message is published to the system topic <code>$SYS/sysmon/long_gc</code>.


**sysmon.vm.long_schedule**

  *Type*: `disabled | duration`

  *Default*: `240ms`

  When the Erlang VM detect a task scheduled for too long, a warning level 'long_schedule' log is emitted,
and an MQTT message is published to the system topic <code>$SYS/sysmon/long_schedule</code>.


**sysmon.vm.large_heap**

  *Type*: `disabled | bytesize`

  *Default*: `32MB`

  When an Erlang process consumed a large amount of memory for its heap space,
the system will write a warning level <code>large_heap</code> log, and an MQTT message is published to
the system topic <code>$SYS/sysmon/large_heap</code>.


**sysmon.vm.busy_dist_port**

  *Type*: `boolean`

  *Default*: `true`

  When the RPC connection used to communicate with other nodes in the cluster is overloaded,
there will be a <code>busy_dist_port</code> warning log,
and an MQTT message is published to system topic <code>$SYS/sysmon/busy_dist_port</code>.


**sysmon.vm.busy_port**

  *Type*: `boolean`

  *Default*: `true`

  When a port (e.g. TCP socket) is overloaded, there will be a <code>busy_port</code> warning log,
and an MQTT message is published to the system topic <code>$SYS/sysmon/busy_port</code>.



## Rate Limit

For an introduction to rate limiting and its use, please refer to [rate limiting](../rate-limit/rate-limit.md).

<!-- ## Overload Protection

broker:overload_protection@ -->

## Performance optimization

<!-- ### broker_perf

broker:broker_perf@ -->

### force_gc


Force garbage collection in MQTT connection process after
 they process certain number of messages or bytes of data.

**force_gc.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable forced garbage collection.


**force_gc.count**

  *Type*: `integer`

  *Default*: `16000`

  *Optional*: `0-inf`

  GC the process after this many received messages.


**force_gc.bytes**

  *Type*: `bytesize`

  *Default*: `16MB`

  GC the process after specified number of bytes have passed through.



### force_shutdown


When the process message queue length, or the memory bytes
reaches a certain value, the process is forced to close.

Note: "message queue" here refers to the "message mailbox"
of the Erlang process, not the `mqueue` of QoS 1 and QoS 2.

**force_shutdown.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable `force_shutdown` feature.


**force_shutdown.max_mailbox_size**

  *Type*: `integer`

  *Default*: `1000`

  *Optional*: `0-inf`

  In EMQX, each online client corresponds to an individual Erlang process. The configuration value establishes a mailbox size limit for these processes. If the mailbox size surpasses this limit, the client will be automatically terminated.


**force_shutdown.max_heap_size**

  *Type*: `wordsize`

  *Default*: `32MB`

  Total heap size



<!-- ### conn_congestion

broker:conn_congestion@ -->

### flapping_detect


This config controls the allowed maximum number of `CONNECT` packets received
from the same clientid in a time frame defined by `window_time`.
After the limit is reached, successive `CONNECT` requests are forbidden
(banned) until the end of the time period defined by `ban_time`.

**flapping_detect.enable**

  *Type*: `boolean`

  *Default*: `false`

  Enable flapping connection detection feature.


**flapping_detect.window_time**

  *Type*: `duration`

  *Default*: `1m`

  The time window for flapping detection.


**flapping_detect.max_count**

  *Type*: `non_neg_integer`

  *Default*: `15`

  The maximum number of disconnects allowed for a MQTT Client in `window_time`


**flapping_detect.ban_time**

  *Type*: `duration`

  *Default*: `5m`

  How long the flapping clientid will be banned.



<!-- ### stats

broker:stats@ -->

<!-- {% emqxce %} -->

<!-- ## Telemetry -->

<!-- modules:telemetry@ -->

<!-- {% endemqxce %} -->

<!-- ## zone 配置 -->

<!-- #zone:overload_protection# -->

## Dashboard


Configuration for EMQX dashboard.

**dashboard.listeners**

  *Type*: `dashboard:listeners`

  HTTP(s) listeners are identified by their protocol type and are
used to serve dashboard UI and restful HTTP API.
Listeners must have a unique combination of port number and IP address.
For example, an HTTP listener can listen on all configured IP addresses
on a given port for a machine by specifying the IP address 0.0.0.0.
Alternatively, the HTTP listener can specify a unique IP address for each listener,
but use the same port.


**dashboard.token_expired_time**

  *Type*: `duration`

  *Default*: `60m`

  JWT token expiration time. Default is 60 minutes


**dashboard.cors**

  *Type*: `boolean`

  *Default*: `false`

  Support Cross-Origin Resource Sharing (CORS).
Allows a server to indicate any origins (domain, scheme, or port) other than
its own from which a browser should permit loading resources.




Configuration for the dashboard listener (plaintext).

**dashboard.listeners.http.bind**

  *Type*: `ip_port`

  *Default*: `0`

  Port without IP(18083) or port with specified IP(127.0.0.1:18083).
Disabled when setting bind to `0`.


**dashboard.listeners.http.num_acceptors**

  *Type*: `integer`

  *Default*: `8`

  Socket acceptor pool size for TCP protocols. Default is the number of schedulers online


**dashboard.listeners.http.max_connections**

  *Type*: `integer`

  *Default*: `512`

  Maximum number of simultaneous connections.


**dashboard.listeners.http.backlog**

  *Type*: `integer`

  *Default*: `1024`

  Defines the maximum length that the queue of pending connections can grow to.


**dashboard.listeners.http.send_timeout**

  *Type*: `duration`

  *Default*: `10s`

  Send timeout for the socket.


**dashboard.listeners.http.inet6**

  *Type*: `boolean`

  *Default*: `false`

  Enable IPv6 support, default is false, which means IPv4 only.


**dashboard.listeners.http.ipv6_v6only**

  *Type*: `boolean`

  *Default*: `false`

  Disable IPv4-to-IPv6 mapping for the listener.
The configuration is only valid when the inet6 is true.


**dashboard.listeners.http.proxy_header**

  *Type*: `boolean`

  *Default*: `false`

  Enable support for `HAProxy` header. Be aware once enabled regular HTTP requests can't be handled anymore.




Configuration for the dashboard listener (TLS).

**dashboard.listeners.https.bind**

  *Type*: `ip_port`

  *Default*: `0`

  Port without IP(18083) or port with specified IP(127.0.0.1:18083).
Disabled when setting bind to `0`.


**dashboard.listeners.https.ssl_options**

  *Type*: `dashboard:ssl_options`

  SSL/TLS options for the dashboard listener.


**dashboard.listeners.https.num_acceptors**

  *Type*: `integer`

  *Default*: `8`

  Socket acceptor pool size for TCP protocols. Default is the number of schedulers online


**dashboard.listeners.https.max_connections**

  *Type*: `integer`

  *Default*: `512`

  Maximum number of simultaneous connections.


**dashboard.listeners.https.backlog**

  *Type*: `integer`

  *Default*: `1024`

  Defines the maximum length that the queue of pending connections can grow to.


**dashboard.listeners.https.send_timeout**

  *Type*: `duration`

  *Default*: `10s`

  Send timeout for the socket.


**dashboard.listeners.https.inet6**

  *Type*: `boolean`

  *Default*: `false`

  Enable IPv6 support, default is false, which means IPv4 only.


**dashboard.listeners.https.ipv6_v6only**

  *Type*: `boolean`

  *Default*: `false`

  Disable IPv4-to-IPv6 mapping for the listener.
The configuration is only valid when the inet6 is true.


**dashboard.listeners.https.proxy_header**

  *Type*: `boolean`

  *Default*: `false`

  Enable support for `HAProxy` header. Be aware once enabled regular HTTP requests can't be handled anymore.




Configuration for the dashboard listener.

**dashboard.listeners.http**

  *Type*: `dashboard:http`

  TCP listeners


**dashboard.listeners.https**

  *Type*: `dashboard:https`

  SSL listeners




SSL/TLS options for the dashboard listener.

**dashboard.listeners.https.ssl_options.cacertfile**

  *Type*: `string`

  *Default*: `${EMQX_ETC_DIR}/certs/cacert.pem`

  Trusted PEM format CA certificates bundle file.<br/>
The certificates in this file are used to verify the TLS peer's certificates.
Append new certificates to the file if new CAs are to be trusted.
There is no need to restart EMQX to have the updated file loaded, because
the system regularly checks if file has been updated (and reload).<br/>
NOTE: invalidating (deleting) a certificate from the file will not affect
already established connections.


**dashboard.listeners.https.ssl_options.cacerts**

  *Type*: `boolean`

  Deprecated since 5.1.4.


**dashboard.listeners.https.ssl_options.certfile**

  *Type*: `string`

  *Default*: `${EMQX_ETC_DIR}/certs/cert.pem`

  PEM format certificates chain file.<br/>
The certificates in this file should be in reversed order of the certificate
issue chain. That is, the host's certificate should be placed in the beginning
of the file, followed by the immediate issuer certificate and so on.
Although the root CA certificate is optional, it should be placed at the end of
the file if it is to be added.


**dashboard.listeners.https.ssl_options.keyfile**

  *Type*: `string`

  *Default*: `${EMQX_ETC_DIR}/certs/key.pem`

  PEM format private key file.


**dashboard.listeners.https.ssl_options.verify**

  *Type*: `enum`

  *Default*: `verify_none`

  *Optional*: `verify_peer | verify_none`

  Enable or disable peer verification.


**dashboard.listeners.https.ssl_options.reuse_sessions**

  *Type*: `boolean`

  *Default*: `true`

  Enable TLS session reuse.<br/>
Has no effect when TLS version is configured (or negotiated) to 1.3


**dashboard.listeners.https.ssl_options.depth**

  *Type*: `non_neg_integer`

  *Default*: `10`

  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br/>
if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br/>
if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.


**dashboard.listeners.https.ssl_options.password**

  *Type*: `string`

  String containing the user's password. Only used if the private key file is password-protected.


**dashboard.listeners.https.ssl_options.versions**

  *Type*: `array`

  *Default*: `["tlsv1.3","tlsv1.2"]`

  All TLS/DTLS versions to be supported.<br/>
NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config.<br/>
In case PSK cipher suites are intended, make sure to configure
<code>['tlsv1.2', 'tlsv1.1']</code> here.


**dashboard.listeners.https.ssl_options.ciphers**

  *Type*: `array`

  *Default*: `[]`

  This config holds TLS cipher suite names separated by comma,
or as an array of strings. e.g.
<code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> or
<code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>.
<br/>
Ciphers (and their ordering) define the way in which the
client and server encrypts information over the network connection.
Selecting a good cipher suite is critical for the
application's data security, confidentiality and performance.

The names should be in OpenSSL string format (not RFC format).
All default values and examples provided by EMQX config
documentation are all in OpenSSL format.<br/>

NOTE: Certain cipher suites are only compatible with
specific TLS <code>versions</code> ('tlsv1.1', 'tlsv1.2' or 'tlsv1.3')
incompatible cipher suites will be silently dropped.
For instance, if only 'tlsv1.3' is given in the <code>versions</code>,
configuring cipher suites for other versions will have no effect.
<br/>

NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config<br/>
If PSK cipher suites are intended, 'tlsv1.3' should be disabled from <code>versions</code>.<br/>
PSK cipher suites: <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>


**dashboard.listeners.https.ssl_options.secure_renegotiate**

  *Type*: `boolean`

  *Default*: `true`

  SSL parameter renegotiation is a feature that allows a client and a server
to renegotiate the parameters of the SSL connection on the fly.
RFC 5746 defines a more secure way of doing this. By enabling secure renegotiation,
you drop support for the insecure renegotiation, prone to MitM attacks.<br/>
Has no effect when TLS version is configured (or negotiated) to 1.3


**dashboard.listeners.https.ssl_options.log_level**

  *Type*: `enum`

  *Default*: `notice`

  *Optional*: `emergency | alert | critical | error | warning | notice | info | debug | none | all`

  Log level for SSL communication. Default is 'notice'. Set to 'debug' to inspect TLS handshake messages.


**dashboard.listeners.https.ssl_options.hibernate_after**

  *Type*: `duration`

  *Default*: `5s`

  Hibernate the SSL process after idling for amount of time reducing its memory footprint.


**dashboard.listeners.https.ssl_options.dhfile**

  *Type*: `string`

  Path to a file containing PEM-encoded Diffie-Hellman parameters
to be used by the server if a cipher suite using Diffie-Hellman
key exchange is negotiated. If not specified, default parameters
are used.<br/>
NOTE: The <code>dhfile</code> option is not supported by TLS 1.3.


**dashboard.listeners.https.ssl_options.honor_cipher_order**

  *Type*: `boolean`

  *Default*: `true`

  An important security setting, it forces the cipher to be set based
 on the server-specified order instead of the client-specified order,
 hence enforcing the (usually more properly configured) security
 ordering of the server administrator.


**dashboard.listeners.https.ssl_options.client_renegotiation**

  *Type*: `boolean`

  *Default*: `true`

  In protocols that support client-initiated renegotiation,
the cost of resources of such an operation is higher for the server than the client.
This can act as a vector for denial of service attacks.
The SSL application already takes measures to counter-act such attempts,
but client-initiated renegotiation can be strictly disabled by setting this option to false.
The default value is true. Note that disabling renegotiation can result in
long-lived connections becoming unusable due to limits on
the number of messages the underlying cipher suite can encipher.<br/>
Has no effect when TLS version is configured (or negotiated) to 1.3


**dashboard.listeners.https.ssl_options.handshake_timeout**

  *Type*: `duration`

  *Default*: `15s`

  Maximum time duration allowed for the handshake to complete



## API Keys


API Key, can be used to request API other than the management API key and the Dashboard user management API

**api_key.bootstrap_file**

  *Type*: `string`

  *Default*: `""`

  The bootstrap file provides API keys for EMQX.
EMQX will load these keys on startup to authorize API requests.
It contains key-value pairs in the format:`api_key:api_secret`.
Each line specifies an API key and its associated secret.



## Events Topic


Enable or disable client lifecycle event publishing.

The following options affect MQTT clients as well as
gateway clients. The types of the clients
are distinguished by the topic prefix:

- For the MQTT clients, the format is:
`$SYS/broker/<node>/clients/<clientid>/<event>`
- For the Gateway clients, it is
`$SYS/broker/<node>/gateway/<gateway-name>/clients/<clientid>/<event>`


**sys_topics.sys_event_messages.client_connected**

  *Type*: `boolean`

  *Default*: `true`

  Enable to publish client connected event messages


**sys_topics.sys_event_messages.client_disconnected**

  *Type*: `boolean`

  *Default*: `true`

  Enable to publish client disconnected event messages.


**sys_topics.sys_event_messages.client_subscribed**

  *Type*: `boolean`

  *Default*: `false`

  Enable to publish event message that client subscribed a topic successfully.


**sys_topics.sys_event_messages.client_unsubscribed**

  *Type*: `boolean`

  *Default*: `false`

  Enable to publish event message that client unsubscribed a topic successfully.



## Authentication - Password-based

### Built-in Database 


Configuration of authenticator using built-in database as data source.

**authn:builtin_db.mechanism**

  *Type*: `password_based`

  Authentication mechanism.


**authn:builtin_db.backend**

  *Type*: `built_in_database`

  Backend type.


**authn:builtin_db.user_id_type**

  *Type*: `enum`

  *Default*: `username`

  *Optional*: `clientid | username`

  Specify whether to use `clientid` or `username` for authentication.


**authn:builtin_db.password_hash_algorithm**

  *Type*: [authn-hash:bcrypt_rw](#authn-hash:bcrypt_rw) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *Default*: `{"salt_position":"prefix","name":"sha256"}`

  Options for password hash creation and verification.


**authn:builtin_db.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.



### MySQL


Configuration of authenticator using MySQL as authentication data source.

**authn:mysql.mechanism**

  *Type*: `password_based`

  Authentication mechanism.


**authn:mysql.backend**

  *Type*: `mysql`

  Backend type.


**authn:mysql.password_hash_algorithm**

  *Type*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *Default*: `{"salt_position":"prefix","name":"sha256"}`

  Options for password hash verification.


**authn:mysql.query**

  *Type*: `string`

  SQL used to query data for authentication, such as password hash.


**authn:mysql.query_timeout**

  *Type*: `duration_ms`

  *Default*: `5s`

  Timeout for the SQL query.


**authn:mysql.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.


**authn:mysql.server**

  *Type*: `string`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The MySQL default port 3306 is used if `[:Port]` is not specified.


**authn:mysql.database**

  *Type*: `string`

  Database name.


**authn:mysql.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authn:mysql.username**

  *Type*: `string`

  *Default*: `root`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**authn:mysql.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**authn:mysql.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.


**authn:mysql.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



### MongoDB

#### MongoDB Single Node


Configuration of authenticator using MongoDB (Standalone) as authentication data source.

**authn:mongo_single.mechanism**

  *Type*: `password_based`

  Authentication mechanism.


**authn:mongo_single.backend**

  *Type*: `mongodb`

  Backend type.


**authn:mongo_single.collection**

  *Type*: `string`

  Collection used to store authentication data.


**authn:mongo_single.filter**

  *Type*: `map`

  *Default*: `{}`

  Conditional expression that defines the filter condition in the query.
Filter supports the following placeholders:
- <code>${username}</code>: Will be replaced at runtime with <code>Username</code> used by the client when connecting
- <code>${clientid}</code>: Will be replaced at runtime with <code>Client ID</code> used by the client when connecting


**authn:mongo_single.password_hash_field**

  *Type*: `string`

  *Default*: `password_hash`

  Document field that contains password hash.


**authn:mongo_single.salt_field**

  *Type*: `string`

  *Default*: `salt`

  Document field that contains the password salt.


**authn:mongo_single.is_superuser_field**

  *Type*: `string`

  *Default*: `is_superuser`

  Document field that defines if the user has superuser privileges.


**authn:mongo_single.password_hash_algorithm**

  *Type*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *Default*: `{"salt_position":"prefix","name":"sha256"}`

  Options for password hash verification.


**authn:mongo_single.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.


**authn:mongo_single.mongo_type**

  *Type*: `single`

  *Default*: `single`

  Standalone instance. Must be set to 'single' when MongoDB server is running in standalone mode.


**authn:mongo_single.server**

  *Type*: `string`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The MongoDB default port 27017 is used if `[:Port]` is not specified.


**authn:mongo_single.w_mode**

  *Type*: `enum`

  *Default*: `unsafe`

  *Optional*: `unsafe | safe`

  Write mode.


**authn:mongo_single.srv_record**

  *Type*: `boolean`

  *Default*: `false`

  Use DNS SRV record.


**authn:mongo_single.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authn:mongo_single.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**authn:mongo_single.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**authn:mongo_single.use_legacy_protocol**

  *Type*: `enum`

  *Default*: `auto`

  *Optional*: `auto | true | false`

  Whether to use MongoDB's legacy protocol for communicating with the database.  The default is to attempt to automatically determine if the newer protocol is supported.


**authn:mongo_single.auth_source**

  *Type*: `string`

  Database name associated with the user's credentials.


**authn:mongo_single.database**

  *Type*: `string`

  Database name.


**authn:mongo_single.topology**

  *Type*: `topology`


**authn:mongo_single.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



#### MongoDB Replica Set


Configuration of authenticator using MongoDB (Replica Set) as authentication data source.

**authn:mongo_rs.mechanism**

  *Type*: `password_based`

  Authentication mechanism.


**authn:mongo_rs.backend**

  *Type*: `mongodb`

  Backend type.


**authn:mongo_rs.collection**

  *Type*: `string`

  Collection used to store authentication data.


**authn:mongo_rs.filter**

  *Type*: `map`

  *Default*: `{}`

  Conditional expression that defines the filter condition in the query.
Filter supports the following placeholders:
- <code>${username}</code>: Will be replaced at runtime with <code>Username</code> used by the client when connecting
- <code>${clientid}</code>: Will be replaced at runtime with <code>Client ID</code> used by the client when connecting


**authn:mongo_rs.password_hash_field**

  *Type*: `string`

  *Default*: `password_hash`

  Document field that contains password hash.


**authn:mongo_rs.salt_field**

  *Type*: `string`

  *Default*: `salt`

  Document field that contains the password salt.


**authn:mongo_rs.is_superuser_field**

  *Type*: `string`

  *Default*: `is_superuser`

  Document field that defines if the user has superuser privileges.


**authn:mongo_rs.password_hash_algorithm**

  *Type*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *Default*: `{"salt_position":"prefix","name":"sha256"}`

  Options for password hash verification.


**authn:mongo_rs.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.


**authn:mongo_rs.mongo_type**

  *Type*: `rs`

  *Default*: `rs`

  Replica set. Must be set to 'rs' when MongoDB server is running in 'replica set' mode.


**authn:mongo_rs.servers**

  *Type*: `string`

  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
A host entry has the following form: `Host[:Port]`.
The MongoDB default port 27017 is used if `[:Port]` is not specified.


**authn:mongo_rs.w_mode**

  *Type*: `enum`

  *Default*: `unsafe`

  *Optional*: `unsafe | safe`

  Write mode.


**authn:mongo_rs.r_mode**

  *Type*: `enum`

  *Default*: `master`

  *Optional*: `master | slave_ok`

  Read mode.


**authn:mongo_rs.replica_set_name**

  *Type*: `string`

  Name of the replica set.


**authn:mongo_rs.srv_record**

  *Type*: `boolean`

  *Default*: `false`

  Use DNS SRV record.


**authn:mongo_rs.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authn:mongo_rs.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**authn:mongo_rs.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**authn:mongo_rs.use_legacy_protocol**

  *Type*: `enum`

  *Default*: `auto`

  *Optional*: `auto | true | false`

  Whether to use MongoDB's legacy protocol for communicating with the database.  The default is to attempt to automatically determine if the newer protocol is supported.


**authn:mongo_rs.auth_source**

  *Type*: `string`

  Database name associated with the user's credentials.


**authn:mongo_rs.database**

  *Type*: `string`

  Database name.


**authn:mongo_rs.topology**

  *Type*: `topology`


**authn:mongo_rs.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.

" 

#### MongoDB Sharded Cluster


Configuration of authenticator using MongoDB (Sharded Cluster) as authentication data source.

**authn:mongo_sharded.mechanism**

  *Type*: `password_based`

  Authentication mechanism.


**authn:mongo_sharded.backend**

  *Type*: `mongodb`

  Backend type.


**authn:mongo_sharded.collection**

  *Type*: `string`

  Collection used to store authentication data.


**authn:mongo_sharded.filter**

  *Type*: `map`

  *Default*: `{}`

  Conditional expression that defines the filter condition in the query.
Filter supports the following placeholders:
- <code>${username}</code>: Will be replaced at runtime with <code>Username</code> used by the client when connecting
- <code>${clientid}</code>: Will be replaced at runtime with <code>Client ID</code> used by the client when connecting


**authn:mongo_sharded.password_hash_field**

  *Type*: `string`

  *Default*: `password_hash`

  Document field that contains password hash.


**authn:mongo_sharded.salt_field**

  *Type*: `string`

  *Default*: `salt`

  Document field that contains the password salt.


**authn:mongo_sharded.is_superuser_field**

  *Type*: `string`

  *Default*: `is_superuser`

  Document field that defines if the user has superuser privileges.


**authn:mongo_sharded.password_hash_algorithm**

  *Type*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *Default*: `{"salt_position":"prefix","name":"sha256"}`

  Options for password hash verification.


**authn:mongo_sharded.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.


**authn:mongo_sharded.mongo_type**

  *Type*: `sharded`

  *Default*: `sharded`

  Sharded cluster. Must be set to 'sharded' when MongoDB server is running in 'sharded' mode.


**authn:mongo_sharded.servers**

  *Type*: `string`

  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
A host entry has the following form: `Host[:Port]`.
The MongoDB default port 27017 is used if `[:Port]` is not specified.


**authn:mongo_sharded.w_mode**

  *Type*: `enum`

  *Default*: `unsafe`

  *Optional*: `unsafe | safe`

  Write mode.


**authn:mongo_sharded.srv_record**

  *Type*: `boolean`

  *Default*: `false`

  Use DNS SRV record.


**authn:mongo_sharded.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authn:mongo_sharded.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**authn:mongo_sharded.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**authn:mongo_sharded.use_legacy_protocol**

  *Type*: `enum`

  *Default*: `auto`

  *Optional*: `auto | true | false`

  Whether to use MongoDB's legacy protocol for communicating with the database.  The default is to attempt to automatically determine if the newer protocol is supported.


**authn:mongo_sharded.auth_source**

  *Type*: `string`

  Database name associated with the user's credentials.


**authn:mongo_sharded.database**

  *Type*: `string`

  Database name.


**authn:mongo_sharded.topology**

  *Type*: `topology`


**authn:mongo_sharded.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



### PostgreSQL


Configuration of authenticator using PostgreSQL as authentication data source.

**authn:postgresql.mechanism**

  *Type*: `password_based`

  Authentication mechanism.


**authn:postgresql.backend**

  *Type*: `postgresql`

  Backend type.


**authn:postgresql.password_hash_algorithm**

  *Type*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *Default*: `{"salt_position":"prefix","name":"sha256"}`

  Options for password hash verification.


**authn:postgresql.query**

  *Type*: `string`

  SQL used to query data for authentication, such as password hash.


**authn:postgresql.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.


**authn:postgresql.server**

  *Type*: `string`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The PostgreSQL default port 5432 is used if `[:Port]` is not specified.


**authn:postgresql.database**

  *Type*: `string`

  Database name.


**authn:postgresql.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authn:postgresql.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**authn:postgresql.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**authn:postgresql.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.


**authn:postgresql.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



### Redis

#### Redis Single Node


Configuration of authenticator using Redis (Standalone) as authentication data source.

**authn:redis_single.mechanism**

  *Type*: `password_based`

  Authentication mechanism.


**authn:redis_single.backend**

  *Type*: `redis`

  Backend type.


**authn:redis_single.cmd**

  *Type*: `string`

  The Redis Command used to query data for authentication such as password hash, currently only supports <code>HGET</code> and <code>HMGET</code>.


**authn:redis_single.password_hash_algorithm**

  *Type*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *Default*: `{"salt_position":"prefix","name":"sha256"}`

  Options for password hash verification.


**authn:redis_single.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.


**authn:redis_single.server**

  *Type*: `string`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The Redis default port 6379 is used if `[:Port]` is not specified.


**authn:redis_single.redis_type**

  *Type*: `single`

  *Default*: `single`

  Single mode. Must be set to 'single' when Redis server is running in single mode.


**authn:redis_single.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authn:redis_single.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**authn:redis_single.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**authn:redis_single.database**

  *Type*: `non_neg_integer`

  *Default*: `0`

  Redis database ID.


**authn:redis_single.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.


**authn:redis_single.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



#### Redis Cluster 


Configuration of authenticator using Redis (Cluster) as authentication data source.

**authn:redis_cluster.mechanism**

  *Type*: `password_based`

  Authentication mechanism.


**authn:redis_cluster.backend**

  *Type*: `redis`

  Backend type.


**authn:redis_cluster.cmd**

  *Type*: `string`

  The Redis Command used to query data for authentication such as password hash, currently only supports <code>HGET</code> and <code>HMGET</code>.


**authn:redis_cluster.password_hash_algorithm**

  *Type*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *Default*: `{"salt_position":"prefix","name":"sha256"}`

  Options for password hash verification.


**authn:redis_cluster.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.


**authn:redis_cluster.servers**

  *Type*: `string`

  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
A host entry has the following form: `Host[:Port]`.
The Redis default port 6379 is used if `[:Port]` is not specified.


**authn:redis_cluster.redis_type**

  *Type*: `cluster`

  *Default*: `cluster`

  Cluster mode. Must be set to 'cluster' when Redis server is running in clustered mode.


**authn:redis_cluster.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authn:redis_cluster.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**authn:redis_cluster.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**authn:redis_cluster.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.


**authn:redis_cluster.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



#### Redis Sentinel 


Configuration of authenticator using Redis (Sentinel) as authentication data source.

**authn:redis_sentinel.mechanism**

  *Type*: `password_based`

  Authentication mechanism.


**authn:redis_sentinel.backend**

  *Type*: `redis`

  Backend type.


**authn:redis_sentinel.cmd**

  *Type*: `string`

  The Redis Command used to query data for authentication such as password hash, currently only supports <code>HGET</code> and <code>HMGET</code>.


**authn:redis_sentinel.password_hash_algorithm**

  *Type*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *Default*: `{"salt_position":"prefix","name":"sha256"}`

  Options for password hash verification.


**authn:redis_sentinel.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.


**authn:redis_sentinel.servers**

  *Type*: `string`

  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
A host entry has the following form: `Host[:Port]`.
The Redis default port 6379 is used if `[:Port]` is not specified.


**authn:redis_sentinel.redis_type**

  *Type*: `sentinel`

  *Default*: `sentinel`

  Sentinel mode. Must be set to 'sentinel' when Redis server is running in sentinel mode.


**authn:redis_sentinel.sentinel**

  *Type*: `string`

  The cluster name in Redis sentinel mode.


**authn:redis_sentinel.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authn:redis_sentinel.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**authn:redis_sentinel.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**authn:redis_sentinel.database**

  *Type*: `non_neg_integer`

  *Default*: `0`

  Redis database ID.


**authn:redis_sentinel.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.


**authn:redis_sentinel.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



### HTTP Service

#### HTTP GET Method


Configuration of authenticator using HTTP Server as authentication service (Using GET request).

**authn:http_get.method**

  *Type*: `get`

  HTTP request method.


**authn:http_get.headers**

  *Type*: `map`

  *Default*: `{"keep-alive":"timeout=30, max=1000","connection":"keep-alive","cache-control":"no-cache","accept":"application/json"}`

  List of HTTP headers (without <code>content-type</code>).


**authn:http_get.mechanism**

  *Type*: `password_based`

  Authentication mechanism.


**authn:http_get.backend**

  *Type*: `http`

  Backend type.


**authn:http_get.url**

  *Type*: `string`

  URL of the HTTP server.


**authn:http_get.body**

  *Type*: `#{term => binary()}`

  HTTP request body.


**authn:http_get.request_timeout**

  *Type*: `duration_ms`

  *Default*: `5s`

  HTTP request timeout.


**authn:http_get.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.


**authn:http_get.connect_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  The timeout when connecting to the HTTP server.


**authn:http_get.enable_pipelining**

  *Type*: `pos_integer`

  *Default*: `100`

  A positive integer. Whether to send HTTP requests continuously, when set to 1, it means that after each HTTP request is sent, you need to wait for the server to return and then continue to send the next request.


**authn:http_get.max_retries**

  *Type*: `non_neg_integer`

  Deprecated since 5.0.4.


**authn:http_get.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  The pool size.


**authn:http_get.request**

  *Type*: `connector-http:request`

  Configure HTTP request parameters.


**authn:http_get.retry_interval**

  *Type*: `timeout_duration`

  Deprecated since 5.0.4.


**authn:http_get.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



#### HTTP POST Method


Configuration of authenticator using HTTP Server as authentication service (Using POST request).

**authn:http_post.method**

  *Type*: `post`

  HTTP request method.


**authn:http_post.headers**

  *Type*: `map`

  *Default*: `{"keep-alive":"timeout=30, max=1000","content-type":"application/json","connection":"keep-alive","cache-control":"no-cache","accept":"application/json"}`

  List of HTTP Headers.


**authn:http_post.mechanism**

  *Type*: `password_based`

  Authentication mechanism.


**authn:http_post.backend**

  *Type*: `http`

  Backend type.


**authn:http_post.url**

  *Type*: `string`

  URL of the HTTP server.


**authn:http_post.body**

  *Type*: `#{term => binary()}`

  HTTP request body.


**authn:http_post.request_timeout**

  *Type*: `duration_ms`

  *Default*: `5s`

  HTTP request timeout.


**authn:http_post.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.


**authn:http_post.connect_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  The timeout when connecting to the HTTP server.


**authn:http_post.enable_pipelining**

  *Type*: `pos_integer`

  *Default*: `100`

  A positive integer. Whether to send HTTP requests continuously, when set to 1, it means that after each HTTP request is sent, you need to wait for the server to return and then continue to send the next request.


**authn:http_post.max_retries**

  *Type*: `non_neg_integer`

  Deprecated since 5.0.4.


**authn:http_post.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  The pool size.


**authn:http_post.request**

  *Type*: `connector-http:request`

  Configure HTTP request parameters.


**authn:http_post.retry_interval**

  *Type*: `timeout_duration`

  Deprecated since 5.0.4.


**authn:http_post.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



{% emqxee %}

### LDAP


Configuration of authenticator using LDAP as authentication data source.

**authn:ldap.mechanism**

  *Type*: `password_based`

  Authentication mechanism.


**authn:ldap.backend**

  *Type*: `ldap`

  Backend type.


**authn:ldap.password_attribute**

  *Type*: `string`

  *Default*: `userPassword`

  Indicates which attribute is used to represent the user's password.


**authn:ldap.is_superuser_attribute**

  *Type*: `string`

  *Default*: `isSuperuser`

  Indicates which attribute is used to represent whether the user is a superuser.


**authn:ldap.query_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  Timeout for the LDAP query.


**authn:ldap.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.


**authn:ldap.server**

  *Type*: `string`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The LDAP default port 389 is used if `[:Port]` is not specified.


**authn:ldap.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authn:ldap.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**authn:ldap.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**authn:ldap.base_dn**

  *Type*: `string`

  The name of the base object entry (or possibly the root) relative to
which the Search is to be performed.


**authn:ldap.filter**

  *Type*: `string`

  *Default*: `(objectClass=mqttUser)`

  The filter that defines the conditions that must be fulfilled in order
for the Search to match a given entry.<br />
The syntax of the filter follows RFC 4515 and also supports placeholders.


**authn:ldap.request_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  Sets the maximum time in milliseconds that is used for each individual request.


**authn:ldap.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



{% endemqxee %}

### Appendix: Hash Config for Credentials


Settings for simple algorithms.

**authn-hash:simple.name**

  *Type*: `enum`

  *Optional*: `plain | md5 | sha | sha256 | sha512`

  Simple password hashing algorithm.


**authn-hash:simple.salt_position**

  *Type*: `enum`

  *Default*: `prefix`

  *Optional*: `disable | prefix | suffix`

  Salt position for PLAIN, MD5, SHA, SHA256 and SHA512 algorithms.




Settings for bcrypt password hashing algorithm.

**authn-hash:bcrypt.name**

  *Type*: `bcrypt`

  BCRYPT password hashing.




Settings for bcrypt password hashing algorithm (for DB backends with write capability).

**authn-hash:bcrypt_rw.name**

  *Type*: `bcrypt`

  BCRYPT password hashing.


**authn-hash:bcrypt_rw.salt_rounds**

  *Type*: `integer`

  *Default*: `10`

  *Optional*: `5-10`

  Work factor for BCRYPT password generation.




Settings for PBKDF2 password hashing algorithm.

**authn-hash:pbkdf2.name**

  *Type*: `pbkdf2`

  PBKDF2 password hashing.


**authn-hash:pbkdf2.mac_fun**

  *Type*: `enum`

  *Optional*: `md4 | md5 | ripemd160 | sha | sha224 | sha256 | sha384 | sha512`

  Specifies mac_fun for PBKDF2 hashing algorithm.


**authn-hash:pbkdf2.iterations**

  *Type*: `integer`

  Iteration count for PBKDF2 hashing algorithm.


**authn-hash:pbkdf2.dk_length**

  *Type*: `integer`

  Derived length for PBKDF2 hashing algorithm. If not specified, calculated automatically based on `mac_fun`.



## Authentication - JWT


Configuration when the JWT for authentication is issued using the HMAC algorithm.

**authn:jwt_hmac.algorithm**

  *Type*: `enum`

  *Optional*: `hmac-based`

  JWT signing algorithm, Supports HMAC (configured as <code>hmac-based</code>) and RSA, ECDSA (configured as <code>public-key</code>).


**authn:jwt_hmac.secret**

  *Type*: `string`

  The key to verify the JWT using HMAC algorithm.


**authn:jwt_hmac.secret_base64_encoded**

  *Type*: `boolean`

  *Default*: `false`

  Whether secret is base64 encoded.


**authn:jwt_hmac.mechanism**

  *Type*: `jwt`

  Authentication mechanism.


**authn:jwt_hmac.acl_claim_name**

  *Type*: `string`

  *Default*: `acl`

  JWT claim name to use for getting ACL rules.


**authn:jwt_hmac.verify_claims**

  *Type*: `[term]`

  *Default*: `[]`

  A list of custom claims to validate, which is a list of name/value pairs.
Values can use the following placeholders:
- <code>${username}</code>: Will be replaced at runtime with <code>Username</code> used by the client when connecting
- <code>${clientid}</code>: Will be replaced at runtime with <code>Client ID</code> used by the client when connecting
Authentication will verify that the value of claims in the JWT (taken from the Password field) matches what is required in <code>verify_claims</code>.


**authn:jwt_hmac.from**

  *Type*: `enum`

  *Default*: `password`

  *Optional*: `username | password`

  Field to take JWT from.


**authn:jwt_hmac.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.



Configuration when JWTs used for authentication need to be fetched from the JWKS endpoint.

**authn:jwt_jwks.use_jwks**

  *Type*: `enum`

  *Optional*: `true`

  Whether to use JWKS.


**authn:jwt_jwks.endpoint**

  *Type*: `string`

  JWKS endpoint, it's a read-only endpoint that returns the server's public key set in the JWKS format.


**authn:jwt_jwks.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authn:jwt_jwks.refresh_interval**

  *Type*: `integer`

  *Default*: `300`

  JWKS refresh interval.


**authn:jwt_jwks.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL options.


**authn:jwt_jwks.mechanism**

  *Type*: `jwt`

  Authentication mechanism.


**authn:jwt_jwks.acl_claim_name**

  *Type*: `string`

  *Default*: `acl`

  JWT claim name to use for getting ACL rules.


**authn:jwt_jwks.verify_claims**

  *Type*: `[term]`

  *Default*: `[]`

  A list of custom claims to validate, which is a list of name/value pairs.
Values can use the following placeholders:
- <code>${username}</code>: Will be replaced at runtime with <code>Username</code> used by the client when connecting
- <code>${clientid}</code>: Will be replaced at runtime with <code>Client ID</code> used by the client when connecting
Authentication will verify that the value of claims in the JWT (taken from the Password field) matches what is required in <code>verify_claims</code>.


**authn:jwt_jwks.from**

  *Type*: `enum`

  *Default*: `password`

  *Optional*: `username | password`

  Field to take JWT from.


**authn:jwt_jwks.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.

" 

Configuration when the JWT for authentication is issued using RSA or ECDSA algorithm.

**authn:jwt_public_key.algorithm**

  *Type*: `enum`

  *Optional*: `public-key`

  JWT signing algorithm, Supports HMAC (configured as <code>hmac-based</code>) and RSA, ECDSA (configured as <code>public-key</code>).


**authn:jwt_public_key.public_key**

  *Type*: `string`

  The public key used to verify the JWT.


**authn:jwt_public_key.mechanism**

  *Type*: `jwt`

  Authentication mechanism.


**authn:jwt_public_key.acl_claim_name**

  *Type*: `string`

  *Default*: `acl`

  JWT claim name to use for getting ACL rules.


**authn:jwt_public_key.verify_claims**

  *Type*: `[term]`

  *Default*: `[]`

  A list of custom claims to validate, which is a list of name/value pairs.
Values can use the following placeholders:
- <code>${username}</code>: Will be replaced at runtime with <code>Username</code> used by the client when connecting
- <code>${clientid}</code>: Will be replaced at runtime with <code>Client ID</code> used by the client when connecting
Authentication will verify that the value of claims in the JWT (taken from the Password field) matches what is required in <code>verify_claims</code>.


**authn:jwt_public_key.from**

  *Type*: `enum`

  *Default*: `password`

  *Optional*: `username | password`

  Field to take JWT from.


**authn:jwt_public_key.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.



## Authentication - Enhanced


Settings for Salted Challenge Response Authentication Mechanism
(SCRAM) authentication.

**authn:scram.mechanism**

  *Type*: `scram`

  Authentication mechanism.


**authn:scram.backend**

  *Type*: `built_in_database`

  Backend type.


**authn:scram.algorithm**

  *Type*: `enum`

  *Default*: `sha256`

  *Optional*: `sha256 | sha512`

  Hashing algorithm.


**authn:scram.iteration_count**

  *Type*: `non_neg_integer`

  *Default*: `4096`

  Iteration count.


**authn:scram.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.



## Authentication - PSK


PSK stands for 'Pre-Shared Keys'.
This config to enable TLS-PSK authentication.

Important! Make sure the SSL listener with only <code>tlsv1.2</code> enabled, and also PSK cipher suites
configured, such as <code>RSA-PSK-AES256-GCM-SHA384</code>.

See listener SSL options config for more details.

The IDs and secrets can be provided from a file which is configurable by the <code>init_file</code> field.

**psk_authentication.enable**

  *Type*: `boolean`

  *Default*: `false`

  Whether to enable TLS PSK support


**psk_authentication.init_file**

  *Type*: `string`

  If init_file is specified, EMQX will import PSKs from the file into the built-in database at startup for use by the runtime.
The file has to be structured line-by-line, each line must be in the format of <code>PSKIdentity:SharedSecret</code>.
For example: <code>mydevice1:c2VjcmV0</code>


**psk_authentication.separator**

  *Type*: `string`

  *Default*: `:`

  The separator between <code>PSKIdentity</code> and <code>SharedSecret</code> in the PSK file


**psk_authentication.chunk_size**

  *Type*: `integer`

  *Default*: `50`

  The size of each chunk used to import to the built-in database from PSK file



## Authorization

### Authorization Settings


Settings that control client authorization.

**authorization.no_match**

  *Type*: `enum`

  *Default*: `allow`

  *Optional*: `allow | deny`

  Default access control action if the user or client matches no ACL rules,
or if no such user or client is found by the configurable authorization
sources such as built_in_database, an HTTP API, or a query against PostgreSQL.
Find more details in 'authorization.sources' config.


**authorization.deny_action**

  *Type*: `enum`

  *Default*: `ignore`

  *Optional*: `ignore | disconnect`

  The action when the authorization check rejects an operation.


**authorization.cache**

  *Type*: `broker:authz_cache`


**authorization.sources**

  *Type*: `array`

  *Default*: `[{"type":"file","path":"${EMQX_ETC_DIR}/acl.conf","enable":true}]`

  Authorization data sources.<br/>
An array of authorization (ACL) data providers.
It is designed as an array, not a hash-map, so the sources can be
ordered to form a chain of access controls.<br/>

When authorizing a 'publish' or 'subscribe' action, the configured
sources are checked in order. When checking an ACL source,
in case the client (identified by username or client ID) is not found,
it moves on to the next source. And it stops immediately
once an 'allow' or 'deny' decision is returned.<br/>

If the client is not found in any of the sources,
the default action configured in 'authorization.no_match' is applied.<br/>

NOTE:
The source elements are identified by their 'type'.
It is NOT allowed to configure two or more sources of the same type.



Settings for the authorization cache.

**authorization.cache.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable the authorization cache.


**authorization.cache.max_size**

  *Type*: `integer`

  *Default*: `32`

  *Optional*: `1-1048576`

  Maximum number of cached items.


**authorization.cache.ttl**

  *Type*: `duration`

  *Default*: `1m`

  Time to live for the cached data.



### ACL File


Authorization using a static file.

**authorization.sources.$INDEX.type**

  *Type*: `file`

  Backend type.


**authorization.sources.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this ACL provider


**authorization.sources.$INDEX.path**

  *Type*: `string`

  Path to the file which contains the ACL rules.
If the file provisioned before starting EMQX node,
it can be placed anywhere as long as EMQX has read access to it.
That is, EMQX will treat it as read only.

In case the rule-set is created or updated from EMQX Dashboard or HTTP API,
a new file will be created and placed in `authz` subdirectory inside EMQX's `data_dir`,
and the old file will not be used anymore.



### Built-in Database


Authorization using a built-in database (mnesia).

**authorization.sources.$INDEX.type**

  *Type*: `built_in_database`

  Backend type.


**authorization.sources.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this ACL provider



### MySQL


Authorization using a MySQL database.

**authorization.sources.$INDEX.type**

  *Type*: `mysql`

  Backend type.


**authorization.sources.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this ACL provider


**authorization.sources.$INDEX.server**

  *Type*: `string`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The MySQL default port 3306 is used if `[:Port]` is not specified.


**authorization.sources.$INDEX.database**

  *Type*: `string`

  Database name.


**authorization.sources.$INDEX.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authorization.sources.$INDEX.username**

  *Type*: `string`

  *Default*: `root`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**authorization.sources.$INDEX.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**authorization.sources.$INDEX.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.


**authorization.sources.$INDEX.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.


**authorization.sources.$INDEX.prepare_statement**

  *Type*: `map`

  Key-value list of SQL prepared statements.


**authorization.sources.$INDEX.query**

  *Type*: `string`

  Database query used to retrieve authorization data.



### PostgreSQL


Authorization using a PostgreSQL database.

**authorization.sources.$INDEX.type**

  *Type*: `postgresql`

  Backend type.


**authorization.sources.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this ACL provider


**authorization.sources.$INDEX.server**

  *Type*: `string`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The PostgreSQL default port 5432 is used if `[:Port]` is not specified.


**authorization.sources.$INDEX.database**

  *Type*: `string`

  Database name.


**authorization.sources.$INDEX.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authorization.sources.$INDEX.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**authorization.sources.$INDEX.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**authorization.sources.$INDEX.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.


**authorization.sources.$INDEX.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.


**authorization.sources.$INDEX.prepare_statement**

  *Type*: `map`

  Key-value list of SQL prepared statements.


**authorization.sources.$INDEX.query**

  *Type*: `string`

  Database query used to retrieve authorization data.

" 

### MongoDB

#### MongoDB Single Node


Authorization using a single MongoDB instance.

**authorization.sources.$INDEX.type**

  *Type*: `mongodb`

  Backend type.


**authorization.sources.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this ACL provider


**authorization.sources.$INDEX.collection**

  *Type*: `string`

  `MongoDB` collection containing the authorization data.


**authorization.sources.$INDEX.filter**

  *Type*: `map`

  *Default*: `{}`

  Conditional expression that defines the filter condition in the query.
Filter supports the following placeholders<br/>
 - <code>${username}</code>: Will be replaced at runtime with <code>Username</code> used by the client when connecting<br/>
 - <code>${clientid}</code>: Will be replaced at runtime with <code>Client ID</code> used by the client when connecting


**authorization.sources.$INDEX.mongo_type**

  *Type*: `single`

  *Default*: `single`

  Standalone instance. Must be set to 'single' when MongoDB server is running in standalone mode.


**authorization.sources.$INDEX.server**

  *Type*: `string`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The MongoDB default port 27017 is used if `[:Port]` is not specified.


**authorization.sources.$INDEX.w_mode**

  *Type*: `enum`

  *Default*: `unsafe`

  *Optional*: `unsafe | safe`

  Write mode.


**authorization.sources.$INDEX.srv_record**

  *Type*: `boolean`

  *Default*: `false`

  Use DNS SRV record.


**authorization.sources.$INDEX.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authorization.sources.$INDEX.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**authorization.sources.$INDEX.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**authorization.sources.$INDEX.use_legacy_protocol**

  *Type*: `enum`

  *Default*: `auto`

  *Optional*: `auto | true | false`

  Whether to use MongoDB's legacy protocol for communicating with the database.  The default is to attempt to automatically determine if the newer protocol is supported.


**authorization.sources.$INDEX.auth_source**

  *Type*: `string`

  Database name associated with the user's credentials.


**authorization.sources.$INDEX.database**

  *Type*: `string`

  Database name.


**authorization.sources.$INDEX.topology**

  *Type*: `topology`


**authorization.sources.$INDEX.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



#### MongoDB Replica Set


Authorization using a MongoDB replica set.

**authorization.sources.$INDEX.type**

  *Type*: `mongodb`

  Backend type.


**authorization.sources.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this ACL provider


**authorization.sources.$INDEX.collection**

  *Type*: `string`

  `MongoDB` collection containing the authorization data.


**authorization.sources.$INDEX.filter**

  *Type*: `map`

  *Default*: `{}`

  Conditional expression that defines the filter condition in the query.
Filter supports the following placeholders<br/>
 - <code>${username}</code>: Will be replaced at runtime with <code>Username</code> used by the client when connecting<br/>
 - <code>${clientid}</code>: Will be replaced at runtime with <code>Client ID</code> used by the client when connecting


**authorization.sources.$INDEX.mongo_type**

  *Type*: `rs`

  *Default*: `rs`

  Replica set. Must be set to 'rs' when MongoDB server is running in 'replica set' mode.


**authorization.sources.$INDEX.servers**

  *Type*: `string`

  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
A host entry has the following form: `Host[:Port]`.
The MongoDB default port 27017 is used if `[:Port]` is not specified.


**authorization.sources.$INDEX.w_mode**

  *Type*: `enum`

  *Default*: `unsafe`

  *Optional*: `unsafe | safe`

  Write mode.


**authorization.sources.$INDEX.r_mode**

  *Type*: `enum`

  *Default*: `master`

  *Optional*: `master | slave_ok`

  Read mode.


**authorization.sources.$INDEX.replica_set_name**

  *Type*: `string`

  Name of the replica set.


**authorization.sources.$INDEX.srv_record**

  *Type*: `boolean`

  *Default*: `false`

  Use DNS SRV record.


**authorization.sources.$INDEX.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authorization.sources.$INDEX.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**authorization.sources.$INDEX.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**authorization.sources.$INDEX.use_legacy_protocol**

  *Type*: `enum`

  *Default*: `auto`

  *Optional*: `auto | true | false`

  Whether to use MongoDB's legacy protocol for communicating with the database.  The default is to attempt to automatically determine if the newer protocol is supported.


**authorization.sources.$INDEX.auth_source**

  *Type*: `string`

  Database name associated with the user's credentials.


**authorization.sources.$INDEX.database**

  *Type*: `string`

  Database name.


**authorization.sources.$INDEX.topology**

  *Type*: `topology`


**authorization.sources.$INDEX.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



#### MongoDB Sharded Cluster


Authorization using a sharded MongoDB cluster.

**authorization.sources.$INDEX.type**

  *Type*: `mongodb`

  Backend type.


**authorization.sources.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this ACL provider


**authorization.sources.$INDEX.collection**

  *Type*: `string`

  `MongoDB` collection containing the authorization data.


**authorization.sources.$INDEX.filter**

  *Type*: `map`

  *Default*: `{}`

  Conditional expression that defines the filter condition in the query.
Filter supports the following placeholders<br/>
 - <code>${username}</code>: Will be replaced at runtime with <code>Username</code> used by the client when connecting<br/>
 - <code>${clientid}</code>: Will be replaced at runtime with <code>Client ID</code> used by the client when connecting


**authorization.sources.$INDEX.mongo_type**

  *Type*: `sharded`

  *Default*: `sharded`

  Sharded cluster. Must be set to 'sharded' when MongoDB server is running in 'sharded' mode.


**authorization.sources.$INDEX.servers**

  *Type*: `string`

  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
A host entry has the following form: `Host[:Port]`.
The MongoDB default port 27017 is used if `[:Port]` is not specified.


**authorization.sources.$INDEX.w_mode**

  *Type*: `enum`

  *Default*: `unsafe`

  *Optional*: `unsafe | safe`

  Write mode.


**authorization.sources.$INDEX.srv_record**

  *Type*: `boolean`

  *Default*: `false`

  Use DNS SRV record.


**authorization.sources.$INDEX.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authorization.sources.$INDEX.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**authorization.sources.$INDEX.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**authorization.sources.$INDEX.use_legacy_protocol**

  *Type*: `enum`

  *Default*: `auto`

  *Optional*: `auto | true | false`

  Whether to use MongoDB's legacy protocol for communicating with the database.  The default is to attempt to automatically determine if the newer protocol is supported.


**authorization.sources.$INDEX.auth_source**

  *Type*: `string`

  Database name associated with the user's credentials.


**authorization.sources.$INDEX.database**

  *Type*: `string`

  Database name.


**authorization.sources.$INDEX.topology**

  *Type*: `topology`


**authorization.sources.$INDEX.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



### Redis

#### Redis Single Node 


Authorization using a single Redis instance.

**authorization.sources.$INDEX.type**

  *Type*: `redis`

  Backend type.


**authorization.sources.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this ACL provider


**authorization.sources.$INDEX.server**

  *Type*: `string`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The Redis default port 6379 is used if `[:Port]` is not specified.


**authorization.sources.$INDEX.redis_type**

  *Type*: `single`

  *Default*: `single`

  Single mode. Must be set to 'single' when Redis server is running in single mode.


**authorization.sources.$INDEX.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authorization.sources.$INDEX.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**authorization.sources.$INDEX.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**authorization.sources.$INDEX.database**

  *Type*: `non_neg_integer`

  *Default*: `0`

  Redis database ID.


**authorization.sources.$INDEX.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.


**authorization.sources.$INDEX.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.


**authorization.sources.$INDEX.cmd**

  *Type*: `string`

  Database query used to retrieve authorization data.



#### Redis Cluster


Authorization using a Redis cluster.

**authorization.sources.$INDEX.type**

  *Type*: `redis`

  Backend type.


**authorization.sources.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this ACL provider


**authorization.sources.$INDEX.servers**

  *Type*: `string`

  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
A host entry has the following form: `Host[:Port]`.
The Redis default port 6379 is used if `[:Port]` is not specified.


**authorization.sources.$INDEX.redis_type**

  *Type*: `cluster`

  *Default*: `cluster`

  Cluster mode. Must be set to 'cluster' when Redis server is running in clustered mode.


**authorization.sources.$INDEX.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authorization.sources.$INDEX.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**authorization.sources.$INDEX.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**authorization.sources.$INDEX.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.


**authorization.sources.$INDEX.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.


**authorization.sources.$INDEX.cmd**

  *Type*: `string`

  Database query used to retrieve authorization data.

" 

#### Redis Sentinel


Authorization using a Redis Sentinel.

**authorization.sources.$INDEX.type**

  *Type*: `redis`

  Backend type.


**authorization.sources.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this ACL provider


**authorization.sources.$INDEX.servers**

  *Type*: `string`

  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
A host entry has the following form: `Host[:Port]`.
The Redis default port 6379 is used if `[:Port]` is not specified.


**authorization.sources.$INDEX.redis_type**

  *Type*: `sentinel`

  *Default*: `sentinel`

  Sentinel mode. Must be set to 'sentinel' when Redis server is running in sentinel mode.


**authorization.sources.$INDEX.sentinel**

  *Type*: `string`

  The cluster name in Redis sentinel mode.


**authorization.sources.$INDEX.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authorization.sources.$INDEX.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**authorization.sources.$INDEX.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**authorization.sources.$INDEX.database**

  *Type*: `non_neg_integer`

  *Default*: `0`

  Redis database ID.


**authorization.sources.$INDEX.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.


**authorization.sources.$INDEX.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.


**authorization.sources.$INDEX.cmd**

  *Type*: `string`

  Database query used to retrieve authorization data.



{% emqxee %}

### LDAP


AuthZ with LDAP

**authorization.sources.$INDEX.type**

  *Type*: `ldap`

  Backend type.


**authorization.sources.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this ACL provider


**authorization.sources.$INDEX.publish_attribute**

  *Type*: `string`

  *Default*: `mqttPublishTopic`

  Indicates which attribute is used to represent the allowed topics list of the `publish`.


**authorization.sources.$INDEX.subscribe_attribute**

  *Type*: `string`

  *Default*: `mqttSubscriptionTopic`

  Indicates which attribute is used to represent the allowed topics list of the `subscribe`.


**authorization.sources.$INDEX.all_attribute**

  *Type*: `string`

  *Default*: `mqttPubSubTopic`

  Indicates which attribute is used to represent the both allowed topics list of  `publish` and `subscribe`.


**authorization.sources.$INDEX.query_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  Timeout for the LDAP query.


**authorization.sources.$INDEX.server**

  *Type*: `string`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The LDAP default port 389 is used if `[:Port]` is not specified.


**authorization.sources.$INDEX.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authorization.sources.$INDEX.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**authorization.sources.$INDEX.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**authorization.sources.$INDEX.base_dn**

  *Type*: `string`

  The name of the base object entry (or possibly the root) relative to
which the Search is to be performed.


**authorization.sources.$INDEX.filter**

  *Type*: `string`

  *Default*: `(objectClass=mqttUser)`

  The filter that defines the conditions that must be fulfilled in order
for the Search to match a given entry.<br />
The syntax of the filter follows RFC 4515 and also supports placeholders.


**authorization.sources.$INDEX.request_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  Sets the maximum time in milliseconds that is used for each individual request.


**authorization.sources.$INDEX.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



{% endemqxee %}

### HTTP Application

#### HTTP GET Method


Authorization using an external HTTP server (via GET requests).

**authorization.sources.$INDEX.type**

  *Type*: `http`

  Backend type.


**authorization.sources.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this ACL provider


**authorization.sources.$INDEX.url**

  *Type*: `string`

  URL of the auth server.


**authorization.sources.$INDEX.request_timeout**

  *Type*: `string`

  *Default*: `30s`

  HTTP request timeout.


**authorization.sources.$INDEX.body**

  *Type*: `map`

  HTTP request body.


**authorization.sources.$INDEX.connect_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  The timeout when connecting to the HTTP server.


**authorization.sources.$INDEX.enable_pipelining**

  *Type*: `pos_integer`

  *Default*: `100`

  A positive integer. Whether to send HTTP requests continuously, when set to 1, it means that after each HTTP request is sent, you need to wait for the server to return and then continue to send the next request.


**authorization.sources.$INDEX.max_retries**

  *Type*: `non_neg_integer`

  Deprecated since 5.0.4.


**authorization.sources.$INDEX.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  The pool size.


**authorization.sources.$INDEX.request**

  *Type*: `connector-http:request`

  Configure HTTP request parameters.


**authorization.sources.$INDEX.retry_interval**

  *Type*: `timeout_duration`

  Deprecated since 5.0.4.


**authorization.sources.$INDEX.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.


**authorization.sources.$INDEX.method**

  *Type*: `get`

  HTTP method.


**authorization.sources.$INDEX.headers**

  *Type*: `[{binary, binary()}]`

  *Default*: `{"keep-alive":"timeout=30, max=1000","connection":"keep-alive","cache-control":"no-cache","accept":"application/json"}`

  List of HTTP headers (without <code>content-type</code>).



#### HTTP POST Method 


Authorization using an external HTTP server (via POST requests).

**authorization.sources.$INDEX.type**

  *Type*: `http`

  Backend type.


**authorization.sources.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this ACL provider


**authorization.sources.$INDEX.url**

  *Type*: `string`

  URL of the auth server.


**authorization.sources.$INDEX.request_timeout**

  *Type*: `string`

  *Default*: `30s`

  HTTP request timeout.


**authorization.sources.$INDEX.body**

  *Type*: `map`

  HTTP request body.


**authorization.sources.$INDEX.connect_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  The timeout when connecting to the HTTP server.


**authorization.sources.$INDEX.enable_pipelining**

  *Type*: `pos_integer`

  *Default*: `100`

  A positive integer. Whether to send HTTP requests continuously, when set to 1, it means that after each HTTP request is sent, you need to wait for the server to return and then continue to send the next request.


**authorization.sources.$INDEX.max_retries**

  *Type*: `non_neg_integer`

  Deprecated since 5.0.4.


**authorization.sources.$INDEX.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  The pool size.


**authorization.sources.$INDEX.request**

  *Type*: `connector-http:request`

  Configure HTTP request parameters.


**authorization.sources.$INDEX.retry_interval**

  *Type*: `timeout_duration`

  Deprecated since 5.0.4.


**authorization.sources.$INDEX.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.


**authorization.sources.$INDEX.method**

  *Type*: `post`

  HTTP method.


**authorization.sources.$INDEX.headers**

  *Type*: `[{binary, binary()}]`

  *Default*: `{"keep-alive":"timeout=30, max=1000","content-type":"application/json","connection":"keep-alive","cache-control":"no-cache","accept":"application/json"}`

  List of HTTP Headers.



## Schema Registry


Schema registry configurations.

**schema_registry.schemas**

  *Type*: `name`

  *Default*: `{}`

  Registered schemas.



### Protobuf


[Protocol Buffers](https://protobuf.dev/) serialization format.

**schema_registry.schemas.$name.type**

  *Type*: `protobuf`

  Schema type.


**schema_registry.schemas.$name.source**

  *Type*: `string`

  Source text for the schema.


**schema_registry.schemas.$name.description**

  *Type*: `string`

  *Default*: `""`

  A description for this schema.



### Avro


[Apache Avro](https://avro.apache.org/) serialization format.

**schema_registry.schemas.$name.type**

  *Type*: `avro`

  Schema type.


**schema_registry.schemas.$name.source**

  *Type*: `json_binary`

  Source text for the schema.


**schema_registry.schemas.$name.description**

  *Type*: `string`

  *Default*: `""`

  A description for this schema.



## Data Bridge

### MQTT


The config for MQTT Bridges.

**bridges.mqtt.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable this bridge


**bridges.mqtt.$name.resource_opts**

  *Type*: `bridge_mqtt:creation_opts`

  *Default*: `{}`

  Resource options.


**bridges.mqtt.$name.mode**

  *Type*: `enum`

  *Optional*: `cluster_shareload`

  Deprecated since v5.1.0 & e5.1.0.


**bridges.mqtt.$name.server**

  *Type*: `string`

  The host and port of the remote MQTT broker


**bridges.mqtt.$name.clientid_prefix**

  *Type*: `string`

  Optional prefix to prepend to the clientid used by egress bridges.


**bridges.mqtt.$name.reconnect_interval**

  *Type*: `string`

  Deprecated since v5.0.16.


**bridges.mqtt.$name.proto_ver**

  *Type*: `enum`

  *Default*: `v4`

  *Optional*: `v3 | v4 | v5`

  The MQTT protocol version


**bridges.mqtt.$name.bridge_mode**

  *Type*: `boolean`

  *Default*: `false`

  If enable bridge mode.
NOTE: This setting is only for MQTT protocol version older than 5.0, and the remote MQTT
broker MUST support this feature.
If bridge_mode is set to true, the bridge will indicate to the remote broker that it is a bridge not an ordinary client.
This means that loop detection will be more effective and that retained messages will be propagated correctly.


**bridges.mqtt.$name.username**

  *Type*: `string`

  The username of the MQTT protocol


**bridges.mqtt.$name.password**

  *Type*: `string`

  The password of the MQTT protocol


**bridges.mqtt.$name.clean_start**

  *Type*: `boolean`

  *Default*: `true`

  Whether to start a clean session when reconnecting a remote broker for ingress bridge


**bridges.mqtt.$name.keepalive**

  *Type*: `string`

  *Default*: `300s`

  MQTT Keepalive. Time interval is a string that contains a number followed by time unit:<br/>- `ms` for milliseconds,
- `s` for seconds,
- `m` for minutes,
- `h` for hours;
<br/>or combination of whereof: `1h5m0s`


**bridges.mqtt.$name.retry_interval**

  *Type*: `string`

  *Default*: `15s`

  Message retry interval. Delay for the MQTT bridge to retry sending the QoS1/QoS2 messages in case of ACK not received. Time interval is a string that contains a number followed by time unit:<br/>- `ms` for milliseconds,
- `s` for seconds,
- `m` for minutes,
- `h` for hours;
<br/>or combination of whereof: `1h5m0s`


**bridges.mqtt.$name.max_inflight**

  *Type*: `non_neg_integer`

  *Default*: `32`

  Max inflight (sent, but un-acked) messages of the MQTT protocol


**bridges.mqtt.$name.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.


**bridges.mqtt.$name.ingress**

  *Type*: `connector-mqtt:ingress`

  The ingress config defines how this bridge receive messages from the remote MQTT broker, and then
        send them to the local broker.<br/>
        Template with variables is allowed in 'remote.qos', 'local.topic', 'local.qos', 'local.retain', 'local.payload'.<br/>
        NOTE: if this bridge is used as the input of a rule, and also 'local.topic' is
        configured, then messages got from the remote broker will be sent to both the 'local.topic' and
        the rule.


**bridges.mqtt.$name.egress**

  *Type*: `connector-mqtt:egress`

  The egress config defines how this bridge forwards messages from the local broker to the remote broker.<br/>
Template with variables is allowed in 'remote.topic', 'local.qos', 'local.retain', 'local.payload'.<br/>
NOTE: if this bridge is used as the action of a rule, and also 'local.topic'
is configured, then both the data got from the rule and the MQTT messages that matches
'local.topic' will be forwarded.




Creation options.

**bridges.mqtt.$name.resource_opts.worker_pool_size**

  *Type*: `integer`

  *Default*: `16`

  *Optional*: `1-1024`

  The number of buffer workers. Only applicable for egress type bridges.
For bridges only have ingress direction data flow, it can be set to 0 otherwise must be greater than 0.


**bridges.mqtt.$name.resource_opts.health_check_interval**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  Health check interval.


**bridges.mqtt.$name.resource_opts.start_after_created**

  *Type*: `boolean`

  *Default*: `true`

  Whether start the resource right after created.


**bridges.mqtt.$name.resource_opts.start_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  Time interval to wait for an auto-started resource to become healthy before responding resource creation requests.


**bridges.mqtt.$name.resource_opts.auto_restart_interval**

  *Type*: `infinity | duration_ms`

  Deprecated since 5.1.0.


**bridges.mqtt.$name.resource_opts.query_mode**

  *Type*: `enum`

  *Default*: `async`

  *Optional*: `sync | async`

  Query mode. Optional 'sync/async', default 'async'.


**bridges.mqtt.$name.resource_opts.request_ttl**

  *Type*: `timeout_duration_ms | infinity`

  *Default*: `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.


**bridges.mqtt.$name.resource_opts.inflight_window**

  *Type*: `pos_integer`

  *Default*: `100`

  Query inflight window. When query_mode is set to async, this config has to be set to 1 if messages from the same MQTT client have to be strictly ordered.


**bridges.mqtt.$name.resource_opts.enable_queue**

  *Type*: `boolean`

  Deprecated since v5.0.14.


**bridges.mqtt.$name.resource_opts.max_buffer_bytes**

  *Type*: `bytesize`

  *Default*: `256MB`

  Maximum number of bytes to buffer for each buffer worker.



### WebHook


Configuration for an HTTP bridge.

**bridges.webhook.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable this bridge


**bridges.webhook.$name.resource_opts**

  *Type*: `bridge_webhook:creation_opts`

  *Default*: `{}`

  Resource options.


**bridges.webhook.$name.connect_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  The timeout when connecting to the HTTP server.


**bridges.webhook.$name.retry_interval**

  *Type*: `timeout_duration`

  Deprecated since 5.0.4.


**bridges.webhook.$name.pool_type**

  *Type*: `emqx_bridge_http_connector:pool_type`

  *Default*: `random`

  The type of the pool. Can be one of `random`, `hash`.


**bridges.webhook.$name.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  The pool size.


**bridges.webhook.$name.enable_pipelining**

  *Type*: `pos_integer`

  *Default*: `100`

  A positive integer. Whether to send HTTP requests continuously, when set to 1, it means that after each HTTP request is sent, you need to wait for the server to return and then continue to send the next request.


**bridges.webhook.$name.request**

  *Type*: `connector-http:request`

  Configure HTTP request parameters.


**bridges.webhook.$name.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.


**bridges.webhook.$name.url**

  *Type*: `string`

  The URL of the HTTP Bridge.<br/>
Template with variables is allowed in the path, but variables cannot be used in the scheme, host,
or port part.<br/>
For example, <code> http://localhost:9901/${topic} </code> is allowed, but
<code> http://${host}:9901/message </code> or <code> http://localhost:${port}/message </code>
is not allowed.


**bridges.webhook.$name.direction**

  *Type*: `egress`

  Deprecated since 5.0.12.


**bridges.webhook.$name.local_topic**

  *Type*: `string`

  The MQTT topic filter to be forwarded to the HTTP server. All MQTT 'PUBLISH' messages with the topic
matching the local_topic will be forwarded.<br/>
NOTE: if this bridge is used as the action of a rule (EMQX rule engine), and also local_topic is
configured, then both the data got from the rule and the MQTT messages that match local_topic
will be forwarded.


**bridges.webhook.$name.method**

  *Type*: `enum`

  *Default*: `post`

  *Optional*: `post | put | get | delete`

  The method of the HTTP request. All the available methods are: post, put, get, delete.<br/>
Template with variables is allowed.


**bridges.webhook.$name.headers**

  *Type*: `map`

  *Default*: `{"keep-alive":"timeout=5","content-type":"application/json","connection":"keep-alive","cache-control":"no-cache","accept":"application/json"}`

  The headers of the HTTP request.<br/>
Template with variables is allowed.


**bridges.webhook.$name.body**

  *Type*: `string`

  The body of the HTTP request.<br/>
If not provided, the body will be a JSON object of all the available fields.<br/>
There, 'all the available fields' means the context of a MQTT message when
this webhook is triggered by receiving a MQTT message (the `local_topic` is set),
or the context of the event when this webhook is triggered by a rule (i.e. this
webhook is used as an action of a rule).<br/>
Template with variables is allowed.


**bridges.webhook.$name.max_retries**

  *Type*: `non_neg_integer`

  *Default*: `2`

  HTTP request max retry times if failed.


**bridges.webhook.$name.request_timeout**

  *Type*: `duration_ms`

  Deprecated since v5.0.26.




Creation options.

**bridges.webhook.$name.resource_opts.worker_pool_size**

  *Type*: `integer`

  *Default*: `16`

  *Optional*: `1-1024`

  The number of buffer workers. Only applicable for egress type bridges.
For bridges only have ingress direction data flow, it can be set to 0 otherwise must be greater than 0.


**bridges.webhook.$name.resource_opts.health_check_interval**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  Health check interval.


**bridges.webhook.$name.resource_opts.start_after_created**

  *Type*: `boolean`

  *Default*: `true`

  Whether start the resource right after created.


**bridges.webhook.$name.resource_opts.start_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  Time interval to wait for an auto-started resource to become healthy before responding resource creation requests.


**bridges.webhook.$name.resource_opts.auto_restart_interval**

  *Type*: `infinity | duration_ms`

  Deprecated since 5.1.0.


**bridges.webhook.$name.resource_opts.query_mode**

  *Type*: `enum`

  *Default*: `async`

  *Optional*: `sync | async`

  Query mode. Optional 'sync/async', default 'async'.


**bridges.webhook.$name.resource_opts.request_ttl**

  *Type*: `timeout_duration_ms | infinity`

  *Default*: `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.


**bridges.webhook.$name.resource_opts.inflight_window**

  *Type*: `pos_integer`

  *Default*: `100`

  Query inflight window. When query_mode is set to async, this config has to be set to 1 if messages from the same MQTT client have to be strictly ordered.


**bridges.webhook.$name.resource_opts.enable_queue**

  *Type*: `boolean`

  Deprecated since v5.0.14.


**bridges.webhook.$name.resource_opts.max_buffer_bytes**

  *Type*: `bytesize`

  *Default*: `256MB`

  Maximum number of bytes to buffer for each buffer worker.



<!-- ### Data Bridge Connector

  @connector-http:request@

  @connector-mqtt:egress@

  @connector-mqtt:egress_local@

  @connector-mqtt:egress_remote@

  @connector-mqtt:ingress@

  @connector-mqtt:ingress_local@

  @connector-mqtt:ingress_remote@ -->

{% emqxee %}

### Kafka


Use GSSAPI/Kerberos authentication.

**bridge_kafka:auth_gssapi_kerberos.kerberos_principal**

  *Type*: `string`

  SASL GSSAPI authentication Kerberos principal. For example <code>client_name@MY.KERBEROS.REALM.MYDOMAIN.COM</code>, NOTE: The realm in use has to be configured in /etc/krb5.conf in EMQX nodes.


**bridge_kafka:auth_gssapi_kerberos.kerberos_keytab_file**

  *Type*: `string`

  SASL GSSAPI authentication Kerberos keytab file path. NOTE: This file has to be placed in EMQX nodes, and the EMQX service runner user requires read permission.




Username/password based authentication.

**bridge_kafka:auth_username_password.mechanism**

  *Type*: `enum`

  *Optional*: `plain | scram_sha_256 | scram_sha_512`

  SASL authentication mechanism.


**bridge_kafka:auth_username_password.username**

  *Type*: `string`

  SASL authentication username.


**bridge_kafka:auth_username_password.password**

  *Type*: `string`

  SASL authentication password.




Kafka consumer configs.

**bridges.kafka_consumer.$name.kafka.max_batch_bytes**

  *Type*: `bytesize`

  *Default*: `896KB`

  Set how many bytes to pull from Kafka in each fetch request. Please note that if the configured value is smaller than the message size in Kafka, it may negatively impact the fetch performance.


**bridges.kafka_consumer.$name.kafka.offset_reset_policy**

  *Type*: `enum`

  *Default*: `latest`

  *Optional*: `latest | earliest`

  Defines from which offset a consumer should start fetching when there is no commit history or when the commit history becomes invalid.


**bridges.kafka_consumer.$name.kafka.offset_commit_interval_seconds**

  *Type*: `timeout_duration_s`

  *Default*: `5s`

  Defines the time interval between two offset commit requests sent for each consumer group.




Defines the mapping between Kafka topics and MQTT topics. Must contain at least one item.

**bridges.kafka_consumer.$name.topic_mapping.$INDEX.kafka_topic**

  *Type*: `string`

  Kafka topic to consume from.


**bridges.kafka_consumer.$name.topic_mapping.$INDEX.mqtt_topic**

  *Type*: `string`

  Local topic to which consumed Kafka messages should be published to.


**bridges.kafka_consumer.$name.topic_mapping.$INDEX.qos**

  *Type*: `qos`

  *Default*: `0`

  MQTT QoS used to publish messages consumed from Kafka.


**bridges.kafka_consumer.$name.topic_mapping.$INDEX.payload_template**

  *Type*: `string`

  *Default*: `${.}`

  The template for transforming the incoming Kafka message.  By default, it will use JSON format to serialize inputs from the Kafka message.  Such fields are:
<code>headers</code>: an object containing string key-value pairs.
<code>key</code>: Kafka message key (uses the chosen key encoding).
<code>offset</code>: offset for the message.
<code>topic</code>: Kafka topic.
<code>ts</code>: message timestamp.
<code>ts_type</code>: message timestamp type, which is one of <code>create</code>, <code>append</code> or <code>undefined</code>.
<code>value</code>: Kafka message value (uses the chosen value encoding).




Kafka Consumer configuration.

**bridges.kafka_consumer.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable (true) or disable (false) this Kafka bridge.


**bridges.kafka_consumer.$name.bootstrap_hosts**

  *Type*: `string`

  A comma separated list of Kafka <code>host[:port]</code> endpoints to bootstrap the client. Default port number is 9092.


**bridges.kafka_consumer.$name.connect_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  Maximum wait time for TCP connection establishment (including authentication time if enabled).


**bridges.kafka_consumer.$name.min_metadata_refresh_interval**

  *Type*: `timeout_duration_ms`

  *Default*: `3s`

  Minimum time interval the client has to wait before refreshing Kafka broker and topic metadata. Setting too small value may add extra load on Kafka.


**bridges.kafka_consumer.$name.metadata_request_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  Maximum wait time when fetching metadata from Kafka.


**bridges.kafka_consumer.$name.authentication**

  *Type*: none | [bridge_kafka:auth_username_password](#bridge_kafka:auth_username_password) | [bridge_kafka:auth_gssapi_kerberos](#bridge_kafka:auth_gssapi_kerberos)

  *Default*: `none`

  Authentication configs.


**bridges.kafka_consumer.$name.socket_opts**

  *Type*: `bridge_kafka:socket_opts`

  Extra socket options.


**bridges.kafka_consumer.$name.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.


**bridges.kafka_consumer.$name.kafka**

  *Type*: `bridge_kafka:consumer_kafka_opts`

  Kafka consumer configs.


**bridges.kafka_consumer.$name.topic_mapping**

  *Type*: `array`

  Defines the mapping between Kafka topics and MQTT topics. Must contain at least one item.


**bridges.kafka_consumer.$name.key_encoding_mode**

  *Type*: `enum`

  *Default*: `none`

  *Optional*: `none | base64`

  Defines how the key from the Kafka message is encoded before being forwarded via MQTT.
<code>none</code> Uses the key from the Kafka message unchanged.  Note: in this case, the key must be a valid UTF-8 string.
<code>base64</code> Uses base-64 encoding on the received key.


**bridges.kafka_consumer.$name.value_encoding_mode**

  *Type*: `enum`

  *Default*: `none`

  *Optional*: `none | base64`

  Defines how the value from the Kafka message is encoded before being forwarded via MQTT.
<code>none</code> Uses the value from the Kafka message unchanged.  Note: in this case, the value must be a valid UTF-8 string.
<code>base64</code> Uses base-64 encoding on the received value.


**bridges.kafka_consumer.$name.resource_opts**

  *Type*: `bridge_kafka:resource_opts`

  *Default*: `{}`




Template to render a Kafka message.

**bridges.kafka.$name.kafka.message.key**

  *Type*: `string`

  *Default*: `${.clientid}`

  Template to render Kafka message key. If the template is rendered into a NULL value (i.e. there is no such data field in Rule Engine context) then Kafka's <code>NULL</code> (but not empty string) is used.


**bridges.kafka.$name.kafka.message.value**

  *Type*: `string`

  *Default*: `${.}`

  Template to render Kafka message value. If the template is rendered into a NULL value (i.e. there is no such data field in Rule Engine context) then Kafka's <code>NULL</code> (but not empty string) is used.


**bridges.kafka.$name.kafka.message.timestamp**

  *Type*: `string`

  *Default*: `${.timestamp}`

  Which timestamp to use. The timestamp is expected to be a millisecond precision Unix epoch which can be in string format, e.g. <code>1661326462115</code> or <code>'1661326462115'</code>. When the desired data field for this template is not found, or if the found data is not a valid integer, the current system timestamp will be used.




Kafka Producer configuration.

**bridges.kafka.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable (true) or disable (false) this Kafka bridge.


**bridges.kafka.$name.bootstrap_hosts**

  *Type*: `string`

  A comma separated list of Kafka <code>host[:port]</code> endpoints to bootstrap the client. Default port number is 9092.


**bridges.kafka.$name.connect_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  Maximum wait time for TCP connection establishment (including authentication time if enabled).


**bridges.kafka.$name.min_metadata_refresh_interval**

  *Type*: `timeout_duration_ms`

  *Default*: `3s`

  Minimum time interval the client has to wait before refreshing Kafka broker and topic metadata. Setting too small value may add extra load on Kafka.


**bridges.kafka.$name.metadata_request_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  Maximum wait time when fetching metadata from Kafka.


**bridges.kafka.$name.authentication**

  *Type*: none | [bridge_kafka:auth_username_password](#bridge_kafka:auth_username_password) | [bridge_kafka:auth_gssapi_kerberos](#bridge_kafka:auth_gssapi_kerberos)

  *Default*: `none`

  Authentication configs.


**bridges.kafka.$name.socket_opts**

  *Type*: `bridge_kafka:socket_opts`

  Extra socket options.


**bridges.kafka.$name.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.


**bridges.kafka.$name.local_topic**

  *Type*: `string`

  MQTT topic or topic filter as data source (bridge input).  If rule action is used as data source, this config should be left empty, otherwise messages will be duplicated in Kafka.


**bridges.kafka.$name.kafka**

  *Type*: `bridge_kafka:producer_kafka_opts`

  Kafka producer configs.


**bridges.kafka.$name.resource_opts**

  *Type*: `bridge_kafka:resource_opts`

  *Default*: `{}`




Configure producer message buffer.

Tell Kafka producer how to buffer messages when EMQX has more messages to send than Kafka can keep up, or when Kafka is down.

**bridge_kafka:producer_buffer.mode**

  *Type*: `enum`

  *Default*: `memory`

  *Optional*: `memory | disk | hybrid`

  Message buffer mode.

<code>memory</code>: Buffer all messages in memory. The messages will be lost in case of EMQX node restart
<code>disk</code>: Buffer all messages on disk. The messages on disk are able to survive EMQX node restart.
<code>hybrid</code>: Buffer message in memory first, when up to certain limit (see <code>segment_bytes</code> config for more information), then start offloading messages to disk, Like <code>memory</code> mode, the messages will be lost in case of EMQX node restart.


**bridge_kafka:producer_buffer.per_partition_limit**

  *Type*: `bytesize`

  *Default*: `2GB`

  Number of bytes allowed to buffer for each Kafka partition. When this limit is exceeded, old messages will be dropped in a trade for credits for new messages to be buffered.


**bridge_kafka:producer_buffer.segment_bytes**

  *Type*: `bytesize`

  *Default*: `100MB`

  Applicable when buffer mode is set to <code>disk</code> or <code>hybrid</code>.
This value is to specify the size of each on-disk buffer file.


**bridge_kafka:producer_buffer.memory_overload_protection**

  *Type*: `boolean`

  *Default*: `false`

  Applicable when buffer mode is set to <code>memory</code>
EMQX will drop old buffered messages under high memory pressure. The high memory threshold is defined in config <code>sysmon.os.sysmem_high_watermark</code>. NOTE: This config only works on Linux.




Please provide more key-value pairs for Kafka headers<br/>
The key-value pairs here will be combined with the
value of <code>kafka_headers</code> field before sending to Kafka.

**bridge_kafka:producer_kafka_ext_headers.kafka_ext_header_key**

  *Type*: `string`

  Key of the Kafka header. Placeholders in format of ${var} are supported.


**bridge_kafka:producer_kafka_ext_headers.kafka_ext_header_value**

  *Type*: `string`

  Value of the Kafka header. Placeholders in format of ${var} are supported.




Kafka producer configs.

**bridges.kafka.$name.kafka.topic**

  *Type*: `string`

  Kafka topic name


**bridges.kafka.$name.kafka.message**

  *Type*: `bridge_kafka:kafka_message`

  Template to render a Kafka message.


**bridges.kafka.$name.kafka.max_batch_bytes**

  *Type*: `bytesize`

  *Default*: `896KB`

  Maximum bytes to collect in a Kafka message batch. Most of the Kafka brokers default to a limit of 1 MB batch size. EMQX's default value is less than 1 MB in order to compensate Kafka message encoding overheads (especially when each individual message is very small). When a single message is over the limit, it is still sent (as a single element batch).


**bridges.kafka.$name.kafka.compression**

  *Type*: `enum`

  *Default*: `no_compression`

  *Optional*: `no_compression | snappy | gzip`

  Compression method.


**bridges.kafka.$name.kafka.partition_strategy**

  *Type*: `enum`

  *Default*: `random`

  *Optional*: `random | key_dispatch`

  Partition strategy is to tell the producer how to dispatch messages to Kafka partitions.

<code>random</code>: Randomly pick a partition for each message
<code>key_dispatch</code>: Hash Kafka message key to a partition number


**bridges.kafka.$name.kafka.required_acks**

  *Type*: `enum`

  *Default*: `all_isr`

  *Optional*: `all_isr | leader_only | none`

  Required acknowledgements for Kafka partition leader to wait for its followers before it sends back the acknowledgement to EMQX Kafka producer

<code>all_isr</code>: Require all in-sync replicas to acknowledge.
<code>leader_only</code>: Require only the partition-leader's acknowledgement.
<code>none</code>: No need for Kafka to acknowledge at all.


**bridges.kafka.$name.kafka.kafka_headers**

  *Type*: `string`

  Please provide a placeholder to be used as Kafka Headers<br/>
e.g. <code>${pub_props}</code><br/>
Notice that the value of the placeholder must either be an object:
<code>{"foo": "bar"}</code>
or an array of key-value pairs:
<code>[{"key": "foo", "value": "bar"}]</code>


**bridges.kafka.$name.kafka.kafka_ext_headers**

  *Type*: `array`

  Please provide more key-value pairs for Kafka headers<br/>
The key-value pairs here will be combined with the
value of <code>kafka_headers</code> field before sending to Kafka.


**bridges.kafka.$name.kafka.kafka_header_value_encode_mode**

  *Type*: `enum`

  *Default*: `none`

  *Optional*: `none | json`

  Kafka headers value encode mode<br/>
 - NONE: only add binary values to Kafka headers;<br/>
 - JSON: only add JSON values to Kafka headers,
and encode it to JSON strings before sending.


**bridges.kafka.$name.kafka.partition_count_refresh_interval**

  *Type*: `timeout_duration_s`

  *Default*: `60s`

  The time interval for Kafka producer to discover increased number of partitions.
After the number of partitions is increased in Kafka, EMQX will start taking the
discovered partitions into account when dispatching messages per <code>partition_strategy</code>.


**bridges.kafka.$name.kafka.max_inflight**

  *Type*: `pos_integer`

  *Default*: `10`

  Maximum number of batches allowed for Kafka producer (per-partition) to send before receiving acknowledgement from Kafka. Greater value typically means better throughput. However, there can be a risk of message reordering when this value is greater than 1.


**bridges.kafka.$name.kafka.buffer**

  *Type*: `bridge_kafka:producer_buffer`

  Configure producer message buffer.

Tell Kafka producer how to buffer messages when EMQX has more messages to send than Kafka can keep up, or when Kafka is down.


**bridges.kafka.$name.kafka.query_mode**

  *Type*: `enum`

  *Default*: `async`

  *Optional*: `async | sync`

  Query mode. Optional 'sync/async', default 'async'.


**bridges.kafka.$name.kafka.sync_query_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  This parameter defines the timeout limit for synchronous queries. It applies only when the bridge query mode is configured to 'sync'.




Resource options.

**bridge_kafka:resource_opts.health_check_interval**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  Health check interval.




Extra socket options.

**bridge_kafka:socket_opts.sndbuf**

  *Type*: `bytesize`

  *Default*: `1MB`

  Fine tune the socket send buffer. The default value is tuned for high throughput.


**bridge_kafka:socket_opts.recbuf**

  *Type*: `bytesize`

  *Default*: `1MB`

  Fine tune the socket receive buffer. The default value is tuned for high throughput.


**bridge_kafka:socket_opts.tcp_keepalive**

  *Type*: `string`

  *Default*: `none`

  Enable TCP keepalive for Kafka bridge connections.
The value is three comma separated numbers in the format of 'Idle,Interval,Probes'
 - Idle: The number of seconds a connection needs to be idle before the server begins to send out keep-alive probes (Linux default 7200).
 - Interval: The number of seconds between TCP keep-alive probes (Linux default 75).
 - Probes: The maximum number of TCP keep-alive probes to send before giving up and killing the connection if no response is obtained from the other end (Linux default 9).
For example "240,30,5" means: TCP keepalive probes are sent after the connection is idle for 240 seconds, and the probes are sent every 30 seconds until a response is received, if it misses 5 consecutive responses, the connection should be closed.
Default: 'none'



### Pulsar


Parameters for basic authentication.

**bridges.pulsar_producer.$name.authentication.username**

  *Type*: `string`

  Basic authentication username.


**bridges.pulsar_producer.$name.authentication.password**

  *Type*: `string`

  Basic authentication password.




Parameters for token authentication.

**bridges.pulsar_producer.$name.authentication.jwt**

  *Type*: `string`

  JWT authentication token.




Configure producer message buffer.

Tell Pulsar producer how to buffer messages when EMQX has more messages to send than Pulsar can keep up, or when Pulsar is down.

**bridges.pulsar_producer.$name.buffer.mode**

  *Type*: `enum`

  *Default*: `memory`

  *Optional*: `memory | disk | hybrid`

  Message buffer mode.
<code>memory</code>: Buffer all messages in memory. The messages will be lost in case of EMQX node restart
<code>disk</code>: Buffer all messages on disk. The messages on disk are able to survive EMQX node restart.
<code>hybrid</code>: Buffer message in memory first, when up to certain limit (see <code>segment_bytes</code> config for more information), then start offloading messages to disk, Like <code>memory</code> mode, the messages will be lost in case of EMQX node restart.


**bridges.pulsar_producer.$name.buffer.per_partition_limit**

  *Type*: `bytesize`

  *Default*: `2GB`

  Number of bytes allowed to buffer for each Pulsar partition. When this limit is exceeded, old messages will be dropped in a trade for credits for new messages to be buffered.


**bridges.pulsar_producer.$name.buffer.segment_bytes**

  *Type*: `bytesize`

  *Default*: `100MB`

  Applicable when buffer mode is set to <code>disk</code> or <code>hybrid</code>.
This value is to specify the size of each on-disk buffer file.


**bridges.pulsar_producer.$name.buffer.memory_overload_protection**

  *Type*: `boolean`

  *Default*: `false`

  Applicable when buffer mode is set to <code>memory</code>
EMQX will drop old buffered messages under high memory pressure. The high memory threshold is defined in config <code>sysmon.os.sysmem_high_watermark</code>. NOTE: This config only works on Linux.




Template to render a Pulsar message.

**bridges.pulsar_producer.$name.message.key**

  *Type*: `string`

  *Default*: `${.clientid}`

  Template to render Pulsar message key.


**bridges.pulsar_producer.$name.message.value**

  *Type*: `string`

  *Default*: `${.}`

  Template to render Pulsar message value.




Creation options.

**bridges.pulsar_producer.$name.resource_opts.health_check_interval**

  *Type*: `timeout_duration_ms`

  *Default*: `1s`

  Health check interval.


**bridges.pulsar_producer.$name.resource_opts.start_after_created**

  *Type*: `boolean`

  *Default*: `true`

  Whether start the resource right after created.


**bridges.pulsar_producer.$name.resource_opts.start_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  Time interval to wait for an auto-started resource to become healthy before responding resource creation requests.




Configuration for a Pulsar bridge.

**bridges.pulsar_producer.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable (true) or disable (false) this Pulsar bridge.


**bridges.pulsar_producer.$name.servers**

  *Type*: `string`

  A comma separated list of Pulsar URLs in the form <code>scheme://host[:port]</code> for the client to connect to. The supported schemes are <code>pulsar://</code> (default) and <code>pulsar+ssl://</code>. The default port is 6650.


**bridges.pulsar_producer.$name.authentication**

  *Type*: none | [bridge_pulsar:auth_basic](#bridge_pulsar:auth_basic) | [bridge_pulsar:auth_token](#bridge_pulsar:auth_token)

  *Default*: `none`

  Authentication configs.


**bridges.pulsar_producer.$name.connect_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  Maximum wait time for TCP connection establishment (including authentication time if enabled).


**bridges.pulsar_producer.$name.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.


**bridges.pulsar_producer.$name.batch_size**

  *Type*: `pos_integer`

  *Default*: `100`

  Maximum number of individual requests to batch in a Pulsar message.


**bridges.pulsar_producer.$name.compression**

  *Type*: `enum`

  *Default*: `no_compression`

  *Optional*: `no_compression | snappy | zlib`

  Compression method.


**bridges.pulsar_producer.$name.send_buffer**

  *Type*: `bytesize`

  *Default*: `1MB`

  Fine tune the socket send buffer. The default value is tuned for high throughput.


**bridges.pulsar_producer.$name.sync_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `3s`

  Maximum wait time for receiving a receipt from Pulsar when publishing synchronously.


**bridges.pulsar_producer.$name.retention_period**

  *Type*: `infinity | duration_ms`

  *Default*: `infinity`

  The amount of time messages will be buffered while there is no connection to the Pulsar broker.  Longer times mean that more memory/disk will be used


**bridges.pulsar_producer.$name.max_batch_bytes**

  *Type*: `bytesize`

  *Default*: `900KB`

  Maximum bytes to collect in a Pulsar message batch. Most of the Pulsar brokers default to a limit of 5 MB batch size. EMQX's default value is less than 5 MB in order to compensate Pulsar message encoding overheads (especially when each individual message is very small). When a single message is over the limit, it is still sent (as a single element batch).


**bridges.pulsar_producer.$name.local_topic**

  *Type*: `string`

  MQTT topic or topic filter as data source (bridge input). If rule action is used as data source, this config should be left empty, otherwise messages will be duplicated in Pulsar.


**bridges.pulsar_producer.$name.pulsar_topic**

  *Type*: `string`

  Pulsar topic name


**bridges.pulsar_producer.$name.strategy**

  *Type*: `enum`

  *Default*: `random`

  *Optional*: `random | roundrobin | key_dispatch`

  Partition strategy is to tell the producer how to dispatch messages to Pulsar partitions.

<code>random</code>: Randomly pick a partition for each message.
<code>roundrobin</code>: Pick each available producer in turn for each message.
<code>key_dispatch</code>: Hash Pulsar message key of the first message in a batch to a partition number.


**bridges.pulsar_producer.$name.buffer**

  *Type*: `bridge_pulsar:producer_buffer`

  Configure producer message buffer.

Tell Pulsar producer how to buffer messages when EMQX has more messages to send than Pulsar can keep up, or when Pulsar is down.


**bridges.pulsar_producer.$name.message**

  *Type*: `bridge_pulsar:producer_pulsar_message`

  Template to render a Pulsar message.


**bridges.pulsar_producer.$name.resource_opts**

  *Type*: `bridge_pulsar:producer_resource_opts`

  Creation options.



### RocketMQ


Configuration for a RocketMQ bridge.

**bridges.rocketmq.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable this bridge


**bridges.rocketmq.$name.template**

  *Type*: `string`

  *Default*: `""`

  Template, the default value is empty. When this value is empty the whole message will be stored in the RocketMQ.<br />
            The template can be any valid string with placeholders, example:<br />
            - ${id}, ${username}, ${clientid}, ${timestamp}<br />
            - {"id" : ${id}, "username" : ${username}}


**bridges.rocketmq.$name.local_topic**

  *Type*: `string`

  The MQTT topic filter to be forwarded to RocketMQ. All MQTT `PUBLISH` messages with the topic
matching the `local_topic` will be forwarded.<br/>
NOTE: if the bridge is used as a rule action, `local_topic` should be left empty otherwise the messages will be duplicated.


**bridges.rocketmq.$name.resource_opts**

  *Type*: `resource_schema:creation_opts`

  *Default*: `{}`

  Resource options.


**bridges.rocketmq.$name.servers**

  *Type*: `string`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The RocketMQ default port 9876 is used if `[:Port]` is not specified.


**bridges.rocketmq.$name.topic**

  *Type*: `string`

  *Default*: `TopicTest`

  RocketMQ Topic


**bridges.rocketmq.$name.access_key**

  *Type*: `string`

  *Default*: `""`

  RocketMQ server `accessKey`.


**bridges.rocketmq.$name.secret_key**

  *Type*: `string`

  *Default*: `""`

  RocketMQ server `secretKey`.


**bridges.rocketmq.$name.security_token**

  *Type*: `string`

  *Default*: `""`

  RocketMQ Server Security Token


**bridges.rocketmq.$name.sync_timeout**

  *Type*: `timeout_duration`

  *Default*: `3s`

  Timeout of RocketMQ driver synchronous call.


**bridges.rocketmq.$name.refresh_interval**

  *Type*: `timeout_duration`

  *Default*: `3s`

  RocketMQ Topic Route Refresh Interval.


**bridges.rocketmq.$name.send_buffer**

  *Type*: `bytesize`

  *Default*: `1024KB`

  The socket send buffer size of the RocketMQ driver client.


**bridges.rocketmq.$name.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**bridges.rocketmq.$name.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.



### RabbitMQ


Configuration for a RabbitMQ bridge.

**bridges.rabbitmq.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable this bridge


**bridges.rabbitmq.$name.local_topic**

  *Type*: `string`

  The MQTT topic filter to be forwarded to RabbitMQ. All MQTT 'PUBLISH' messages with the topic matching the local_topic will be forwarded.
    NOTE: if this bridge is used as the action of a rule (EMQX rule engine), and also local_topic is configured, then both the data got from the rule and the MQTT messages that match local_topic will be forwarded.


**bridges.rabbitmq.$name.resource_opts**

  *Type*: `bridge_rabbitmq:creation_opts`

  *Default*: `{}`

  Resource options.


**bridges.rabbitmq.$name.server**

  *Type*: `string`

  *Default*: `localhost`

  The RabbitMQ server address that you want to connect to (for example, localhost).


**bridges.rabbitmq.$name.port**

  *Type*: `port_number`

  *Default*: `5672`

  The RabbitMQ server address that you want to connect to (for example, localhost).


**bridges.rabbitmq.$name.username**

  *Type*: `string`

  The username used to authenticate with the RabbitMQ server.


**bridges.rabbitmq.$name.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**bridges.rabbitmq.$name.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  The size of the connection pool.


**bridges.rabbitmq.$name.timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  The timeout for waiting on the connection to be established.


**bridges.rabbitmq.$name.wait_for_publish_confirmations**

  *Type*: `boolean`

  *Default*: `true`

  A boolean value that indicates whether to wait for RabbitMQ to confirm message publication when using publisher confirms.


**bridges.rabbitmq.$name.publish_confirmation_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `30s`

  The timeout for waiting on the connection to be established.


**bridges.rabbitmq.$name.virtual_host**

  *Type*: `string`

  *Default*: `/`

  The virtual host to use when connecting to the RabbitMQ server.


**bridges.rabbitmq.$name.heartbeat**

  *Type*: `timeout_duration_ms`

  *Default*: `30s`

  The interval for sending heartbeat messages to the RabbitMQ server.


**bridges.rabbitmq.$name.exchange**

  *Type*: `string`

  The name of the RabbitMQ exchange where the messages will be sent.


**bridges.rabbitmq.$name.routing_key**

  *Type*: `string`

  The routing key used to route messages to the correct queue in the RabbitMQ exchange.


**bridges.rabbitmq.$name.delivery_mode**

  *Type*: `enum`

  *Default*: `non_persistent`

  *Optional*: `non_persistent | persistent`

  The delivery mode for messages published to RabbitMQ. Delivery mode non_persistent (1) is suitable for messages that don't require persistence across RabbitMQ restarts, whereas delivery mode persistent (2) is designed for messages that must survive RabbitMQ restarts.


**bridges.rabbitmq.$name.payload_template**

  *Type*: `string`

  *Default*: `${.}`

  The template for formatting the payload of the message before sending it to RabbitMQ. Template placeholders, such as ${field1.sub_field}, will be substituted with the respective field's value. When left empty, the entire input message will be used as the payload, formatted as a JSON text. This behavior is equivalent to specifying ${.} as the payload template.


**bridges.rabbitmq.$name.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.




Creation options.

**bridges.rabbitmq.$name.resource_opts.worker_pool_size**

  *Type*: `integer`

  *Default*: `16`

  *Optional*: `1-1024`

  The number of buffer workers. Only applicable for egress type bridges.
For bridges only have ingress direction data flow, it can be set to 0 otherwise must be greater than 0.


**bridges.rabbitmq.$name.resource_opts.health_check_interval**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  Health check interval.


**bridges.rabbitmq.$name.resource_opts.start_after_created**

  *Type*: `boolean`

  *Default*: `true`

  Whether start the resource right after created.


**bridges.rabbitmq.$name.resource_opts.start_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  Time interval to wait for an auto-started resource to become healthy before responding resource creation requests.


**bridges.rabbitmq.$name.resource_opts.auto_restart_interval**

  *Type*: `infinity | duration_ms`

  Deprecated since 5.1.0.


**bridges.rabbitmq.$name.resource_opts.query_mode**

  *Type*: `enum`

  *Default*: `async`

  *Optional*: `sync | async`

  Query mode. Optional 'sync/async', default 'async'.


**bridges.rabbitmq.$name.resource_opts.request_ttl**

  *Type*: `timeout_duration_ms | infinity`

  *Default*: `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.


**bridges.rabbitmq.$name.resource_opts.inflight_window**

  *Type*: `pos_integer`

  *Default*: `100`

  Query inflight window. When query_mode is set to async, this config has to be set to 1 if messages from the same MQTT client have to be strictly ordered.


**bridges.rabbitmq.$name.resource_opts.batch_size**

  *Type*: `pos_integer`

  *Default*: `1`

  Maximum batch count. If equal to 1, there's effectively no batching.


**bridges.rabbitmq.$name.resource_opts.batch_time**

  *Type*: `timeout_duration_ms`

  *Default*: `0ms`

  Maximum waiting interval when accumulating a batch at a low message rates for more efficient resource usage.


**bridges.rabbitmq.$name.resource_opts.enable_queue**

  *Type*: `boolean`

  Deprecated since v5.0.14.


**bridges.rabbitmq.$name.resource_opts.max_buffer_bytes**

  *Type*: `bytesize`

  *Default*: `256MB`

  Maximum number of bytes to buffer for each buffer worker.



### Azure Event Hubs


Username/password based authentication.

**bridges.azure_event_hub_producer.$name.authentication.password**

  *Type*: `string`

  The Connection String for connecting to Azure Event Hub.  Should be the "connection string-primary key" of a Namespace shared access policy.




Template to render an Azure Event Hub message.

**bridges.azure_event_hub_producer.$name.kafka.message.key**

  *Type*: `string`

  *Default*: `${.clientid}`

  Template to render Azure Event Hub message key. If the template is rendered into a NULL value (i.e. there is no such data field in Rule Engine context) then Azure Event Hub's <code>NULL</code> (but not empty string) is used.


**bridges.azure_event_hub_producer.$name.kafka.message.value**

  *Type*: `string`

  *Default*: `${.}`

  Template to render Azure Event Hub message value. If the template is rendered into a NULL value (i.e. there is no such data field in Rule Engine context) then Azure Event Hub's <code>NULL</code> (but not empty string) is used.




Azure Event Hub producer configs.

**bridges.azure_event_hub_producer.$name.kafka.topic**

  *Type*: `string`

  Event Hub name


**bridges.azure_event_hub_producer.$name.kafka.message**

  *Type*: `bridge_azure_event_hub:kafka_message`

  Template to render an Azure Event Hub message.


**bridges.azure_event_hub_producer.$name.kafka.max_batch_bytes**

  *Type*: `bytesize`

  *Default*: `896KB`

  Maximum bytes to collect in an Azure Event Hub message batch. Most of the Kafka brokers default to a limit of 1 MB batch size. EMQX's default value is less than 1 MB in order to compensate Kafka message encoding overheads (especially when each individual message is very small). When a single message is over the limit, it is still sent (as a single element batch).


**bridges.azure_event_hub_producer.$name.kafka.partition_strategy**

  *Type*: `enum`

  *Default*: `random`

  *Optional*: `random | key_dispatch`

  Partition strategy is to tell the producer how to dispatch messages to Azure Event Hub partitions.

<code>random</code>: Randomly pick a partition for each message
<code>key_dispatch</code>: Hash Azure Event Hub message key to a partition number


**bridges.azure_event_hub_producer.$name.kafka.required_acks**

  *Type*: `enum`

  *Default*: `all_isr`

  *Optional*: `all_isr | leader_only`

  Required acknowledgements for Azure Event Hub partition leader to wait for its followers before it sends back the acknowledgement to EMQX Azure Event Hub producer

<code>all_isr</code>: Require all in-sync replicas to acknowledge.
<code>leader_only</code>: Require only the partition-leader's acknowledgement.


**bridges.azure_event_hub_producer.$name.kafka.kafka_headers**

  *Type*: `string`

  Please provide a placeholder to be used as Azure Event Hub Headers<br/>
e.g. <code>${pub_props}</code><br/>
Notice that the value of the placeholder must either be an object:
<code>{"foo": "bar"}</code>
or an array of key-value pairs:
<code>[{"key": "foo", "value": "bar"}]</code>


**bridges.azure_event_hub_producer.$name.kafka.kafka_ext_headers**

  *Type*: `array`

  Please provide more key-value pairs for Azure Event Hub headers<br/>
The key-value pairs here will be combined with the
value of <code>kafka_headers</code> field before sending to Azure Event Hub.


**bridges.azure_event_hub_producer.$name.kafka.kafka_header_value_encode_mode**

  *Type*: `enum`

  *Default*: `none`

  *Optional*: `none | json`

  Azure Event Hub headers value encode mode<br/>
 - NONE: only add binary values to Azure Event Hub headers;<br/>
 - JSON: only add JSON values to Azure Event Hub headers,
and encode it to JSON strings before sending.


**bridges.azure_event_hub_producer.$name.kafka.partition_count_refresh_interval**

  *Type*: `timeout_duration_s`

  *Default*: `60s`

  The time interval for Azure Event Hub producer to discover increased number of partitions.
After the number of partitions is increased in Azure Event Hub, EMQX will start taking the
discovered partitions into account when dispatching messages per <code>partition_strategy</code>.


**bridges.azure_event_hub_producer.$name.kafka.max_inflight**

  *Type*: `pos_integer`

  *Default*: `10`

  Maximum number of batches allowed for Azure Event Hub producer (per-partition) to send before receiving acknowledgement from Azure Event Hub. Greater value typically means better throughput. However, there can be a risk of message reordering when this value is greater than 1.


**bridges.azure_event_hub_producer.$name.kafka.buffer**

  *Type*: `bridge_kafka:producer_buffer`

  Configure producer message buffer.

Tell Azure Event Hub producer how to buffer messages when EMQX has more messages to send than Azure Event Hub can keep up, or when Azure Event Hub is down.


**bridges.azure_event_hub_producer.$name.kafka.query_mode**

  *Type*: `enum`

  *Default*: `async`

  *Optional*: `async | sync`

  Query mode. Optional 'sync/async', default 'async'.


**bridges.azure_event_hub_producer.$name.kafka.sync_query_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  This parameter defines the timeout limit for synchronous queries. It applies only when the bridge query mode is configured to 'sync'.




Configuration for an Azure Event Hub bridge.

**bridges.azure_event_hub_producer.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable (true) or disable (false) this bridge.


**bridges.azure_event_hub_producer.$name.bootstrap_hosts**

  *Type*: `string`

  A comma separated list of Azure Event Hub Kafka <code>host[:port]</code> namespace endpoints to bootstrap the client.  Default port number is 9093.


**bridges.azure_event_hub_producer.$name.connect_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  Maximum wait time for TCP connection establishment (including authentication time if enabled).


**bridges.azure_event_hub_producer.$name.min_metadata_refresh_interval**

  *Type*: `timeout_duration_ms`

  *Default*: `3s`

  Minimum time interval the client has to wait before refreshing Azure Event Hub Kafka broker and topic metadata. Setting too small value may add extra load on Azure Event Hub.


**bridges.azure_event_hub_producer.$name.metadata_request_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  Maximum wait time when fetching metadata from Azure Event Hub.


**bridges.azure_event_hub_producer.$name.authentication**

  *Type*: `bridge_azure_event_hub:auth_username_password`

  *Default*: `{}`

  Authentication configs.


**bridges.azure_event_hub_producer.$name.socket_opts**

  *Type*: `bridge_kafka:socket_opts`

  Extra socket options.


**bridges.azure_event_hub_producer.$name.ssl**

  *Type*: `bridge_azure_event_hub:ssl_client_opts`

  *Default*: `{"enable":true}`

  SSL connection settings.


**bridges.azure_event_hub_producer.$name.local_topic**

  *Type*: `string`

  MQTT topic or topic filter as data source (bridge input).  If rule action is used as data source, this config should be left empty, otherwise messages will be duplicated in Azure Event Hub.


**bridges.azure_event_hub_producer.$name.kafka**

  *Type*: `bridge_azure_event_hub:producer_kafka_opts`

  Azure Event Hub producer configs.


**bridges.azure_event_hub_producer.$name.resource_opts**

  *Type*: `bridge_kafka:resource_opts`

  *Default*: `{}`




Socket options for SSL clients.

**bridges.azure_event_hub_producer.$name.ssl.cacertfile**

  *Type*: `string`

  Trusted PEM format CA certificates bundle file.<br/>
The certificates in this file are used to verify the TLS peer's certificates.
Append new certificates to the file if new CAs are to be trusted.
There is no need to restart EMQX to have the updated file loaded, because
the system regularly checks if file has been updated (and reload).<br/>
NOTE: invalidating (deleting) a certificate from the file will not affect
already established connections.


**bridges.azure_event_hub_producer.$name.ssl.cacerts**

  *Type*: `boolean`

  Deprecated since 5.1.4.


**bridges.azure_event_hub_producer.$name.ssl.certfile**

  *Type*: `string`

  PEM format certificates chain file.<br/>
The certificates in this file should be in reversed order of the certificate
issue chain. That is, the host's certificate should be placed in the beginning
of the file, followed by the immediate issuer certificate and so on.
Although the root CA certificate is optional, it should be placed at the end of
the file if it is to be added.


**bridges.azure_event_hub_producer.$name.ssl.keyfile**

  *Type*: `string`

  PEM format private key file.


**bridges.azure_event_hub_producer.$name.ssl.verify**

  *Type*: `enum`

  *Default*: `verify_none`

  *Optional*: `verify_peer | verify_none`

  Enable or disable peer verification.


**bridges.azure_event_hub_producer.$name.ssl.reuse_sessions**

  *Type*: `boolean`

  *Default*: `true`

  Enable TLS session reuse.<br/>
Has no effect when TLS version is configured (or negotiated) to 1.3


**bridges.azure_event_hub_producer.$name.ssl.depth**

  *Type*: `non_neg_integer`

  *Default*: `10`

  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br/>
if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br/>
if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.


**bridges.azure_event_hub_producer.$name.ssl.password**

  *Type*: `string`

  String containing the user's password. Only used if the private key file is password-protected.


**bridges.azure_event_hub_producer.$name.ssl.versions**

  *Type*: `array`

  *Default*: `["tlsv1.3","tlsv1.2"]`

  All TLS/DTLS versions to be supported.<br/>
NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config.<br/>
In case PSK cipher suites are intended, make sure to configure
<code>['tlsv1.2', 'tlsv1.1']</code> here.


**bridges.azure_event_hub_producer.$name.ssl.ciphers**

  *Type*: `array`

  *Default*: `[]`

  This config holds TLS cipher suite names separated by comma,
or as an array of strings. e.g.
<code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> or
<code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>.
<br/>
Ciphers (and their ordering) define the way in which the
client and server encrypts information over the network connection.
Selecting a good cipher suite is critical for the
application's data security, confidentiality and performance.

The names should be in OpenSSL string format (not RFC format).
All default values and examples provided by EMQX config
documentation are all in OpenSSL format.<br/>

NOTE: Certain cipher suites are only compatible with
specific TLS <code>versions</code> ('tlsv1.1', 'tlsv1.2' or 'tlsv1.3')
incompatible cipher suites will be silently dropped.
For instance, if only 'tlsv1.3' is given in the <code>versions</code>,
configuring cipher suites for other versions will have no effect.
<br/>

NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config<br/>
If PSK cipher suites are intended, 'tlsv1.3' should be disabled from <code>versions</code>.<br/>
PSK cipher suites: <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>


**bridges.azure_event_hub_producer.$name.ssl.secure_renegotiate**

  *Type*: `boolean`

  *Default*: `true`

  SSL parameter renegotiation is a feature that allows a client and a server
to renegotiate the parameters of the SSL connection on the fly.
RFC 5746 defines a more secure way of doing this. By enabling secure renegotiation,
you drop support for the insecure renegotiation, prone to MitM attacks.<br/>
Has no effect when TLS version is configured (or negotiated) to 1.3


**bridges.azure_event_hub_producer.$name.ssl.log_level**

  *Type*: `enum`

  *Default*: `notice`

  *Optional*: `emergency | alert | critical | error | warning | notice | info | debug | none | all`

  Log level for SSL communication. Default is 'notice'. Set to 'debug' to inspect TLS handshake messages.


**bridges.azure_event_hub_producer.$name.ssl.hibernate_after**

  *Type*: `duration`

  *Default*: `5s`

  Hibernate the SSL process after idling for amount of time reducing its memory footprint.


**bridges.azure_event_hub_producer.$name.ssl.enable**

  *Type*: `true`

  *Default*: `true`

  Enable TLS.


**bridges.azure_event_hub_producer.$name.ssl.server_name_indication**

  *Type*: `disable | auto | string`

  *Default*: `auto`

  Specify the host name to be used in TLS Server Name Indication extension.<br/>
For instance, when connecting to "server.example.net", the genuine server
which accepts the connection and performs TLS handshake may differ from the
host the TLS client initially connects to, e.g. when connecting to an IP address
or when the host has multiple resolvable DNS records <br/>
If not specified, it will default to the host name string which is used
to establish the connection, unless it is IP addressed used.<br/>
The host name is then also used in the host name verification of the peer
certificate.<br/> The special value 'disable' prevents the Server Name
Indication extension from being sent and disables the hostname
verification check.



### Amazon Kinesis


Configuration for an Amazon Kinesis bridge.

**bridges.kinesis_producer.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable this bridge


**bridges.kinesis_producer.$name.resource_opts**

  *Type*: `bridge_kinesis:creation_opts`

  *Default*: `{}`

  Creation options.


**bridges.kinesis_producer.$name.aws_access_key_id**

  *Type*: `string`

  Access Key ID for connecting to Amazon Kinesis.


**bridges.kinesis_producer.$name.aws_secret_access_key**

  *Type*: `string`

  AWS Secret Access Key for connecting to Amazon Kinesis.


**bridges.kinesis_producer.$name.endpoint**

  *Type*: `string`

  The url of Amazon Kinesis endpoint.


**bridges.kinesis_producer.$name.max_retries**

  *Type*: `non_neg_integer`

  *Default*: `2`

  Max retry times if an error occurs when sending a request.


**bridges.kinesis_producer.$name.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  The pool size.


**bridges.kinesis_producer.$name.payload_template**

  *Type*: `string`

  *Default*: `${.}`

  The template for formatting the outgoing messages.  If undefined, will send all the available context in JSON format.


**bridges.kinesis_producer.$name.local_topic**

  *Type*: `string`

  The MQTT topic filter to be forwarded to Amazon Kinesis. All MQTT `PUBLISH` messages with the topic
matching the `local_topic` will be forwarded.<br/>
NOTE: if this bridge is used as the action of a rule (EMQX rule engine), and also `local_topic` is
configured, then both the data got from the rule and the MQTT messages that match `local_topic`
will be forwarded.


**bridges.kinesis_producer.$name.stream_name**

  *Type*: `string`

  The Amazon Kinesis Stream to publish messages to.


**bridges.kinesis_producer.$name.partition_key**

  *Type*: `string`

  The Amazon Kinesis Partition Key associated to published message. Placeholders in format of ${var} are supported.




Creation options.

**bridges.kinesis_producer.$name.resource_opts.worker_pool_size**

  *Type*: `integer`

  *Default*: `16`

  *Optional*: `1-1024`

  The number of buffer workers. Only applicable for egress type bridges.
For bridges only have ingress direction data flow, it can be set to 0 otherwise must be greater than 0.


**bridges.kinesis_producer.$name.resource_opts.health_check_interval**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  Health check interval.


**bridges.kinesis_producer.$name.resource_opts.start_after_created**

  *Type*: `boolean`

  *Default*: `true`

  Whether start the resource right after created.


**bridges.kinesis_producer.$name.resource_opts.start_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  Time interval to wait for an auto-started resource to become healthy before responding resource creation requests.


**bridges.kinesis_producer.$name.resource_opts.auto_restart_interval**

  *Type*: `infinity | duration_ms`

  Deprecated since 5.1.0.


**bridges.kinesis_producer.$name.resource_opts.query_mode**

  *Type*: `enum`

  *Default*: `async`

  *Optional*: `sync | async`

  Query mode. Optional 'sync/async', default 'async'.


**bridges.kinesis_producer.$name.resource_opts.request_ttl**

  *Type*: `timeout_duration_ms | infinity`

  *Default*: `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.


**bridges.kinesis_producer.$name.resource_opts.inflight_window**

  *Type*: `pos_integer`

  *Default*: `100`

  Query inflight window. When query_mode is set to async, this config has to be set to 1 if messages from the same MQTT client have to be strictly ordered.


**bridges.kinesis_producer.$name.resource_opts.batch_size**

  *Type*: `pos_integer`

  *Default*: `1`

  Maximum batch count. If equal to 1, there's effectively no batching.


**bridges.kinesis_producer.$name.resource_opts.batch_time**

  *Type*: `timeout_duration_ms`

  *Default*: `0ms`

  Maximum waiting interval when accumulating a batch at a low message rates for more efficient resource usage.


**bridges.kinesis_producer.$name.resource_opts.enable_queue**

  *Type*: `boolean`

  Deprecated since v5.0.14.


**bridges.kinesis_producer.$name.resource_opts.max_buffer_bytes**

  *Type*: `bytesize`

  *Default*: `256MB`

  Maximum number of bytes to buffer for each buffer worker.



### Google PubSub


GCP PubSub Consumer configuration.

**bridges.gcp_pubsub_consumer.$name.consumer.pull_max_messages**

  *Type*: `pos_integer`

  *Default*: `100`

  The maximum number of messages to retrieve from GCP PubSub in a single pull request. The actual number may be less than the specified value.


**bridges.gcp_pubsub_consumer.$name.consumer.topic_mapping**

  *Type*: `array`

  Defines the mapping between GCP PubSub topics and MQTT topics. Must contain at least one item.




Defines the mapping between GCP PubSub topics and MQTT topics. Must contain at least one item.

**bridges.gcp_pubsub_consumer.$name.consumer.topic_mapping.$INDEX.pubsub_topic**

  *Type*: `string`

  GCP PubSub topic to consume from.


**bridges.gcp_pubsub_consumer.$name.consumer.topic_mapping.$INDEX.mqtt_topic**

  *Type*: `string`

  Local topic to which consumed GCP PubSub messages should be published to.


**bridges.gcp_pubsub_consumer.$name.consumer.topic_mapping.$INDEX.qos**

  *Type*: `qos`

  *Default*: `0`

  MQTT QoS used to publish messages consumed from GCP PubSub.


**bridges.gcp_pubsub_consumer.$name.consumer.topic_mapping.$INDEX.payload_template**

  *Type*: `string`

  *Default*: `${.}`

  The template for transforming the incoming GCP PubSub message.  By default, it will use JSON format to serialize inputs from the GCP PubSub message.  Available fields are:
<code>message_id</code>: the message ID assigned by GCP PubSub.
<code>publish_time</code>: message timestamp assigned by GCP PubSub.
<code>topic</code>: GCP PubSub topic.
<code>value</code>: the payload of the GCP PubSub message.  Omitted if there's no payload.
<code>attributes</code>: an object containing string key-value pairs.  Omitted if there are no attributes.
<code>ordering_key</code>: GCP PubSub message ordering key.  Omitted if there's none.




Key-value pair.

**bridges.gcp_pubsub.$name.attributes_template.$INDEX.key**

  *Type*: `string`

  Key


**bridges.gcp_pubsub.$name.attributes_template.$INDEX.value**

  *Type*: `string`

  Value




Configuration for a GCP PubSub bridge.

**bridges.gcp_pubsub_consumer.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable this bridge


**bridges.gcp_pubsub_consumer.$name.resource_opts**

  *Type*: `bridge_gcp_pubsub:consumer_resource_opts`

  Creation options.


**bridges.gcp_pubsub_consumer.$name.connect_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  The timeout when connecting to the HTTP server.


**bridges.gcp_pubsub_consumer.$name.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  The pool size.


**bridges.gcp_pubsub_consumer.$name.pipelining**

  *Type*: `pos_integer`

  *Default*: `100`

  A positive integer. Whether to send HTTP requests continuously, when set to 1, it means that after each HTTP request is sent, you need to wait for the server to return and then continue to send the next request.


**bridges.gcp_pubsub_consumer.$name.max_retries**

  *Type*: `non_neg_integer`

  *Default*: `2`

  Max retry times if an error occurs when sending a request.


**bridges.gcp_pubsub_consumer.$name.request_timeout**

  *Type*: `timeout_duration_ms`

  Deprecated since e5.0.1.


**bridges.gcp_pubsub_consumer.$name.service_account_json**

  *Type*: `emqx_bridge_gcp_pubsub:service_account_json`

  JSON containing the GCP Service Account credentials to be used with PubSub.
When a GCP Service Account is created (as described in https://developers.google.com/identity/protocols/oauth2/service-account#creatinganaccount), you have the option of downloading the credentials in JSON form.  That's the file needed.


**bridges.gcp_pubsub_consumer.$name.consumer**

  *Type*: `bridge_gcp_pubsub:consumer`

  Local MQTT publish and GCP PubSub consumer configs.




Configuration for a GCP PubSub bridge.

**bridges.gcp_pubsub.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable this bridge


**bridges.gcp_pubsub.$name.resource_opts**

  *Type*: `resource_schema:creation_opts`

  *Default*: `{}`

  Resource options.


**bridges.gcp_pubsub.$name.connect_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  The timeout when connecting to the HTTP server.


**bridges.gcp_pubsub.$name.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  The pool size.


**bridges.gcp_pubsub.$name.pipelining**

  *Type*: `pos_integer`

  *Default*: `100`

  A positive integer. Whether to send HTTP requests continuously, when set to 1, it means that after each HTTP request is sent, you need to wait for the server to return and then continue to send the next request.


**bridges.gcp_pubsub.$name.max_retries**

  *Type*: `non_neg_integer`

  *Default*: `2`

  Max retry times if an error occurs when sending a request.


**bridges.gcp_pubsub.$name.request_timeout**

  *Type*: `timeout_duration_ms`

  Deprecated since e5.0.1.


**bridges.gcp_pubsub.$name.service_account_json**

  *Type*: `emqx_bridge_gcp_pubsub:service_account_json`

  JSON containing the GCP Service Account credentials to be used with PubSub.
When a GCP Service Account is created (as described in https://developers.google.com/identity/protocols/oauth2/service-account#creatinganaccount), you have the option of downloading the credentials in JSON form.  That's the file needed.


**bridges.gcp_pubsub.$name.attributes_template**

  *Type*: `array`

  *Default*: `[]`

  The template for formatting the outgoing message attributes.  Undefined values will be rendered as empty string values.  Empty keys are removed from the attribute map.


**bridges.gcp_pubsub.$name.ordering_key_template**

  *Type*: `string`

  *Default*: `""`

  The template for formatting the outgoing message ordering key.  Undefined values will be rendered as empty string values.  This value will not be added to the message if it's empty.


**bridges.gcp_pubsub.$name.payload_template**

  *Type*: `string`

  *Default*: `""`

  The template for formatting the outgoing messages.  If undefined, will send all the available context in JSON format.


**bridges.gcp_pubsub.$name.local_topic**

  *Type*: `string`

  The MQTT topic filter to be forwarded to GCP PubSub. All MQTT 'PUBLISH' messages with the topic
matching `local_topic` will be forwarded.<br/>
NOTE: if this bridge is used as the action of a rule (EMQX rule engine), and also local_topic is
configured, then both the data got from the rule and the MQTT messages that match local_topic
will be forwarded.


**bridges.gcp_pubsub.$name.pubsub_topic**

  *Type*: `string`

  The GCP PubSub topic to publish messages to.




Creation options.

**bridges.gcp_pubsub_consumer.$name.resource_opts.health_check_interval**

  *Type*: `timeout_duration_ms`

  *Default*: `30s`

  Health check interval.


**bridges.gcp_pubsub_consumer.$name.resource_opts.request_ttl**

  *Type*: `timeout_duration_ms | infinity`

  *Default*: `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.



### MySQL


Configuration for an HStreamDB bridge.

**bridges.mysql.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable this bridge


**bridges.mysql.$name.sql**

  *Type*: `string`

  *Default*: `insert into t_mqtt_msg(msgid, topic, qos, payload, arrived) values (${id}, ${topic}, ${qos}, ${payload}, FROM_UNIXTIME(${timestamp}/1000))`

  SQL Template


**bridges.mysql.$name.local_topic**

  *Type*: `string`

  The MQTT topic filter to be forwarded to MySQL. All MQTT 'PUBLISH' messages with the topic
matching the local_topic will be forwarded.<br/>
NOTE: if this bridge is used as the action of a rule (EMQX rule engine), and also local_topic is
configured, then both the data got from the rule and the MQTT messages that match local_topic
will be forwarded.


**bridges.mysql.$name.resource_opts**

  *Type*: `resource_schema:creation_opts`

  *Default*: `{}`

  Resource options.


**bridges.mysql.$name.server**

  *Type*: `string`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The MySQL default port 3306 is used if `[:Port]` is not specified.


**bridges.mysql.$name.database**

  *Type*: `string`

  Database name.


**bridges.mysql.$name.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**bridges.mysql.$name.username**

  *Type*: `string`

  *Default*: `root`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**bridges.mysql.$name.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**bridges.mysql.$name.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.


**bridges.mysql.$name.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



### Redis


Cluster mode. Must be set to 'cluster' when Redis server is running in clustered mode.

**bridges.redis_cluster.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable this bridge


**bridges.redis_cluster.$name.local_topic**

  *Type*: `string`

  The MQTT topic filter to be forwarded to Redis. All MQTT 'PUBLISH' messages with the topic
matching the local_topic will be forwarded.<br/>
NOTE: if this bridge is used as the action of a rule (EMQX rule engine), and also local_topic is
configured, then both the data got from the rule and the MQTT messages that match local_topic
will be forwarded.


**bridges.redis_cluster.$name.command_template**

  *Type*: `[binary]`

  Redis command template used to export messages. Each list element stands for a command name or its argument.
For example, to push payloads in a Redis list by key `msgs`, the elements should be the following:
`rpush`, `msgs`, `${payload}`.


**bridges.redis_cluster.$name.resource_opts**

  *Type*: `bridge_redis:creation_opts_redis_cluster`

  *Default*: `{}`

  Resource options.


**bridges.redis_cluster.$name.servers**

  *Type*: `string`

  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
A host entry has the following form: `Host[:Port]`.
The Redis default port 6379 is used if `[:Port]` is not specified.


**bridges.redis_cluster.$name.redis_type**

  *Type*: `cluster`

  *Default*: `cluster`

  Cluster mode. Must be set to 'cluster' when Redis server is running in clustered mode.


**bridges.redis_cluster.$name.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**bridges.redis_cluster.$name.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**bridges.redis_cluster.$name.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**bridges.redis_cluster.$name.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.


**bridges.redis_cluster.$name.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.




Sentinel mode. Must be set to 'sentinel' when Redis server is running in sentinel mode.

**bridges.redis_sentinel.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable this bridge


**bridges.redis_sentinel.$name.local_topic**

  *Type*: `string`

  The MQTT topic filter to be forwarded to Redis. All MQTT 'PUBLISH' messages with the topic
matching the local_topic will be forwarded.<br/>
NOTE: if this bridge is used as the action of a rule (EMQX rule engine), and also local_topic is
configured, then both the data got from the rule and the MQTT messages that match local_topic
will be forwarded.


**bridges.redis_sentinel.$name.command_template**

  *Type*: `[binary]`

  Redis command template used to export messages. Each list element stands for a command name or its argument.
For example, to push payloads in a Redis list by key `msgs`, the elements should be the following:
`rpush`, `msgs`, `${payload}`.


**bridges.redis_sentinel.$name.resource_opts**

  *Type*: `bridge_redis:creation_opts_redis_sentinel`

  *Default*: `{}`

  Resource options.


**bridges.redis_sentinel.$name.servers**

  *Type*: `string`

  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
A host entry has the following form: `Host[:Port]`.
The Redis default port 6379 is used if `[:Port]` is not specified.


**bridges.redis_sentinel.$name.redis_type**

  *Type*: `sentinel`

  *Default*: `sentinel`

  Sentinel mode. Must be set to 'sentinel' when Redis server is running in sentinel mode.


**bridges.redis_sentinel.$name.sentinel**

  *Type*: `string`

  The cluster name in Redis sentinel mode.


**bridges.redis_sentinel.$name.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**bridges.redis_sentinel.$name.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**bridges.redis_sentinel.$name.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**bridges.redis_sentinel.$name.database**

  *Type*: `non_neg_integer`

  *Default*: `0`

  Redis database ID.


**bridges.redis_sentinel.$name.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.


**bridges.redis_sentinel.$name.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.




Single mode. Must be set to 'single' when Redis server is running in single mode.

**bridges.redis_single.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable this bridge


**bridges.redis_single.$name.local_topic**

  *Type*: `string`

  The MQTT topic filter to be forwarded to Redis. All MQTT 'PUBLISH' messages with the topic
matching the local_topic will be forwarded.<br/>
NOTE: if this bridge is used as the action of a rule (EMQX rule engine), and also local_topic is
configured, then both the data got from the rule and the MQTT messages that match local_topic
will be forwarded.


**bridges.redis_single.$name.command_template**

  *Type*: `[binary]`

  Redis command template used to export messages. Each list element stands for a command name or its argument.
For example, to push payloads in a Redis list by key `msgs`, the elements should be the following:
`rpush`, `msgs`, `${payload}`.


**bridges.redis_single.$name.resource_opts**

  *Type*: `bridge_redis:creation_opts_redis_single`

  *Default*: `{}`

  Resource options.


**bridges.redis_single.$name.server**

  *Type*: `string`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The Redis default port 6379 is used if `[:Port]` is not specified.


**bridges.redis_single.$name.redis_type**

  *Type*: `single`

  *Default*: `single`

  Single mode. Must be set to 'single' when Redis server is running in single mode.


**bridges.redis_single.$name.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**bridges.redis_single.$name.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**bridges.redis_single.$name.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**bridges.redis_single.$name.database**

  *Type*: `non_neg_integer`

  *Default*: `0`

  Redis database ID.


**bridges.redis_single.$name.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.


**bridges.redis_single.$name.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.




Creation options.

**bridges.redis_cluster.$name.resource_opts.worker_pool_size**

  *Type*: `integer`

  *Default*: `16`

  *Optional*: `1-1024`

  The number of buffer workers. Only applicable for egress type bridges.
For bridges only have ingress direction data flow, it can be set to 0 otherwise must be greater than 0.


**bridges.redis_cluster.$name.resource_opts.health_check_interval**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  Health check interval.


**bridges.redis_cluster.$name.resource_opts.start_after_created**

  *Type*: `boolean`

  *Default*: `true`

  Whether start the resource right after created.


**bridges.redis_cluster.$name.resource_opts.start_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  Time interval to wait for an auto-started resource to become healthy before responding resource creation requests.


**bridges.redis_cluster.$name.resource_opts.auto_restart_interval**

  *Type*: `infinity | duration_ms`

  Deprecated since 5.1.0.


**bridges.redis_cluster.$name.resource_opts.query_mode**

  *Type*: `enum`

  *Default*: `async`

  *Optional*: `sync | async`

  Query mode. Optional 'sync/async', default 'async'.


**bridges.redis_cluster.$name.resource_opts.request_ttl**

  *Type*: `timeout_duration_ms | infinity`

  *Default*: `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.


**bridges.redis_cluster.$name.resource_opts.inflight_window**

  *Type*: `pos_integer`

  *Default*: `100`

  Query inflight window. When query_mode is set to async, this config has to be set to 1 if messages from the same MQTT client have to be strictly ordered.


**bridges.redis_cluster.$name.resource_opts.enable_queue**

  *Type*: `boolean`

  Deprecated since v5.0.14.


**bridges.redis_cluster.$name.resource_opts.max_buffer_bytes**

  *Type*: `bytesize`

  *Default*: `256MB`

  Maximum number of bytes to buffer for each buffer worker.




Creation options.

**bridges.redis_sentinel.$name.resource_opts.worker_pool_size**

  *Type*: `integer`

  *Default*: `16`

  *Optional*: `1-1024`

  The number of buffer workers. Only applicable for egress type bridges.
For bridges only have ingress direction data flow, it can be set to 0 otherwise must be greater than 0.


**bridges.redis_sentinel.$name.resource_opts.health_check_interval**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  Health check interval.


**bridges.redis_sentinel.$name.resource_opts.start_after_created**

  *Type*: `boolean`

  *Default*: `true`

  Whether start the resource right after created.


**bridges.redis_sentinel.$name.resource_opts.start_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  Time interval to wait for an auto-started resource to become healthy before responding resource creation requests.


**bridges.redis_sentinel.$name.resource_opts.auto_restart_interval**

  *Type*: `infinity | duration_ms`

  Deprecated since 5.1.0.


**bridges.redis_sentinel.$name.resource_opts.query_mode**

  *Type*: `enum`

  *Default*: `async`

  *Optional*: `sync | async`

  Query mode. Optional 'sync/async', default 'async'.


**bridges.redis_sentinel.$name.resource_opts.request_ttl**

  *Type*: `timeout_duration_ms | infinity`

  *Default*: `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.


**bridges.redis_sentinel.$name.resource_opts.inflight_window**

  *Type*: `pos_integer`

  *Default*: `100`

  Query inflight window. When query_mode is set to async, this config has to be set to 1 if messages from the same MQTT client have to be strictly ordered.


**bridges.redis_sentinel.$name.resource_opts.batch_size**

  *Type*: `pos_integer`

  *Default*: `1`

  Maximum batch count. If equal to 1, there's effectively no batching.


**bridges.redis_sentinel.$name.resource_opts.batch_time**

  *Type*: `timeout_duration_ms`

  *Default*: `0ms`

  Maximum waiting interval when accumulating a batch at a low message rates for more efficient resource usage.


**bridges.redis_sentinel.$name.resource_opts.enable_queue**

  *Type*: `boolean`

  Deprecated since v5.0.14.


**bridges.redis_sentinel.$name.resource_opts.max_buffer_bytes**

  *Type*: `bytesize`

  *Default*: `256MB`

  Maximum number of bytes to buffer for each buffer worker.




Creation options.

**bridges.redis_single.$name.resource_opts.worker_pool_size**

  *Type*: `integer`

  *Default*: `16`

  *Optional*: `1-1024`

  The number of buffer workers. Only applicable for egress type bridges.
For bridges only have ingress direction data flow, it can be set to 0 otherwise must be greater than 0.


**bridges.redis_single.$name.resource_opts.health_check_interval**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  Health check interval.


**bridges.redis_single.$name.resource_opts.start_after_created**

  *Type*: `boolean`

  *Default*: `true`

  Whether start the resource right after created.


**bridges.redis_single.$name.resource_opts.start_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  Time interval to wait for an auto-started resource to become healthy before responding resource creation requests.


**bridges.redis_single.$name.resource_opts.auto_restart_interval**

  *Type*: `infinity | duration_ms`

  Deprecated since 5.1.0.


**bridges.redis_single.$name.resource_opts.query_mode**

  *Type*: `enum`

  *Default*: `async`

  *Optional*: `sync | async`

  Query mode. Optional 'sync/async', default 'async'.


**bridges.redis_single.$name.resource_opts.request_ttl**

  *Type*: `timeout_duration_ms | infinity`

  *Default*: `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.


**bridges.redis_single.$name.resource_opts.inflight_window**

  *Type*: `pos_integer`

  *Default*: `100`

  Query inflight window. When query_mode is set to async, this config has to be set to 1 if messages from the same MQTT client have to be strictly ordered.


**bridges.redis_single.$name.resource_opts.batch_size**

  *Type*: `pos_integer`

  *Default*: `1`

  Maximum batch count. If equal to 1, there's effectively no batching.


**bridges.redis_single.$name.resource_opts.batch_time**

  *Type*: `timeout_duration_ms`

  *Default*: `0ms`

  Maximum waiting interval when accumulating a batch at a low message rates for more efficient resource usage.


**bridges.redis_single.$name.resource_opts.enable_queue**

  *Type*: `boolean`

  Deprecated since v5.0.14.


**bridges.redis_single.$name.resource_opts.max_buffer_bytes**

  *Type*: `bytesize`

  *Default*: `256MB`

  Maximum number of bytes to buffer for each buffer worker.



### MongoDB


MongoDB (Standalone) configuration

**bridges.mongodb_single.$name.mongo_type**

  *Type*: `single`

  *Default*: `single`

  Standalone instance. Must be set to 'single' when MongoDB server is running in standalone mode.


**bridges.mongodb_single.$name.server**

  *Type*: `string`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The MongoDB default port 27017 is used if `[:Port]` is not specified.


**bridges.mongodb_single.$name.w_mode**

  *Type*: `enum`

  *Default*: `unsafe`

  *Optional*: `unsafe | safe`

  Write mode.


**bridges.mongodb_single.$name.srv_record**

  *Type*: `boolean`

  *Default*: `false`

  Use DNS SRV record.


**bridges.mongodb_single.$name.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**bridges.mongodb_single.$name.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**bridges.mongodb_single.$name.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**bridges.mongodb_single.$name.use_legacy_protocol**

  *Type*: `enum`

  *Default*: `auto`

  *Optional*: `auto | true | false`

  Whether to use MongoDB's legacy protocol for communicating with the database.  The default is to attempt to automatically determine if the newer protocol is supported.


**bridges.mongodb_single.$name.auth_source**

  *Type*: `string`

  Database name associated with the user's credentials.


**bridges.mongodb_single.$name.database**

  *Type*: `string`

  Database name.


**bridges.mongodb_single.$name.topology**

  *Type*: `topology`


**bridges.mongodb_single.$name.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.


**bridges.mongodb_single.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable this MongoDB Bridge


**bridges.mongodb_single.$name.collection**

  *Type*: `string`

  *Default*: `mqtt`

  The collection where data will be stored into


**bridges.mongodb_single.$name.payload_template**

  *Type*: `string`

  The template for formatting the outgoing messages.  If undefined, rule engine will use JSON format to serialize all visible inputs, such as clientid, topic, payload etc.


**bridges.mongodb_single.$name.resource_opts**

  *Type*: `bridge_mongodb:creation_opts`

  Creation options.




MongoDB (Replica Set) configuration

**bridges.mongodb_rs.$name.mongo_type**

  *Type*: `rs`

  *Default*: `rs`

  Replica set. Must be set to 'rs' when MongoDB server is running in 'replica set' mode.


**bridges.mongodb_rs.$name.servers**

  *Type*: `string`

  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
A host entry has the following form: `Host[:Port]`.
The MongoDB default port 27017 is used if `[:Port]` is not specified.


**bridges.mongodb_rs.$name.w_mode**

  *Type*: `enum`

  *Default*: `unsafe`

  *Optional*: `unsafe | safe`

  Write mode.


**bridges.mongodb_rs.$name.r_mode**

  *Type*: `enum`

  *Default*: `master`

  *Optional*: `master | slave_ok`

  Read mode.


**bridges.mongodb_rs.$name.replica_set_name**

  *Type*: `string`

  Name of the replica set.


**bridges.mongodb_rs.$name.srv_record**

  *Type*: `boolean`

  *Default*: `false`

  Use DNS SRV record.


**bridges.mongodb_rs.$name.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**bridges.mongodb_rs.$name.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**bridges.mongodb_rs.$name.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**bridges.mongodb_rs.$name.use_legacy_protocol**

  *Type*: `enum`

  *Default*: `auto`

  *Optional*: `auto | true | false`

  Whether to use MongoDB's legacy protocol for communicating with the database.  The default is to attempt to automatically determine if the newer protocol is supported.


**bridges.mongodb_rs.$name.auth_source**

  *Type*: `string`

  Database name associated with the user's credentials.


**bridges.mongodb_rs.$name.database**

  *Type*: `string`

  Database name.


**bridges.mongodb_rs.$name.topology**

  *Type*: `topology`


**bridges.mongodb_rs.$name.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.


**bridges.mongodb_rs.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable this MongoDB Bridge


**bridges.mongodb_rs.$name.collection**

  *Type*: `string`

  *Default*: `mqtt`

  The collection where data will be stored into


**bridges.mongodb_rs.$name.payload_template**

  *Type*: `string`

  The template for formatting the outgoing messages.  If undefined, rule engine will use JSON format to serialize all visible inputs, such as clientid, topic, payload etc.


**bridges.mongodb_rs.$name.resource_opts**

  *Type*: `bridge_mongodb:creation_opts`

  Creation options.




MongoDB (Sharded) configuration

**bridges.mongodb_sharded.$name.mongo_type**

  *Type*: `sharded`

  *Default*: `sharded`

  Sharded cluster. Must be set to 'sharded' when MongoDB server is running in 'sharded' mode.


**bridges.mongodb_sharded.$name.servers**

  *Type*: `string`

  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
A host entry has the following form: `Host[:Port]`.
The MongoDB default port 27017 is used if `[:Port]` is not specified.


**bridges.mongodb_sharded.$name.w_mode**

  *Type*: `enum`

  *Default*: `unsafe`

  *Optional*: `unsafe | safe`

  Write mode.


**bridges.mongodb_sharded.$name.srv_record**

  *Type*: `boolean`

  *Default*: `false`

  Use DNS SRV record.


**bridges.mongodb_sharded.$name.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**bridges.mongodb_sharded.$name.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**bridges.mongodb_sharded.$name.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**bridges.mongodb_sharded.$name.use_legacy_protocol**

  *Type*: `enum`

  *Default*: `auto`

  *Optional*: `auto | true | false`

  Whether to use MongoDB's legacy protocol for communicating with the database.  The default is to attempt to automatically determine if the newer protocol is supported.


**bridges.mongodb_sharded.$name.auth_source**

  *Type*: `string`

  Database name associated with the user's credentials.


**bridges.mongodb_sharded.$name.database**

  *Type*: `string`

  Database name.


**bridges.mongodb_sharded.$name.topology**

  *Type*: `topology`


**bridges.mongodb_sharded.$name.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.


**bridges.mongodb_sharded.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable this MongoDB Bridge


**bridges.mongodb_sharded.$name.collection**

  *Type*: `string`

  *Default*: `mqtt`

  The collection where data will be stored into


**bridges.mongodb_sharded.$name.payload_template**

  *Type*: `string`

  The template for formatting the outgoing messages.  If undefined, rule engine will use JSON format to serialize all visible inputs, such as clientid, topic, payload etc.


**bridges.mongodb_sharded.$name.resource_opts**

  *Type*: `bridge_mongodb:creation_opts`

  Creation options.




Creation options.

**bridge_mongodb:creation_opts.worker_pool_size**

  *Type*: `integer`

  *Default*: `16`

  *Optional*: `1-1024`

  The number of buffer workers. Only applicable for egress type bridges.
For bridges only have ingress direction data flow, it can be set to 0 otherwise must be greater than 0.


**bridge_mongodb:creation_opts.health_check_interval**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  Health check interval.


**bridge_mongodb:creation_opts.start_after_created**

  *Type*: `boolean`

  *Default*: `true`

  Whether start the resource right after created.


**bridge_mongodb:creation_opts.start_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  Time interval to wait for an auto-started resource to become healthy before responding resource creation requests.


**bridge_mongodb:creation_opts.auto_restart_interval**

  *Type*: `infinity | duration_ms`

  Deprecated since 5.1.0.


**bridge_mongodb:creation_opts.query_mode**

  *Type*: `enum`

  *Default*: `async`

  *Optional*: `sync | async`

  Query mode. Optional 'sync/async', default 'async'.


**bridge_mongodb:creation_opts.request_ttl**

  *Type*: `timeout_duration_ms | infinity`

  *Default*: `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.


**bridge_mongodb:creation_opts.inflight_window**

  *Type*: `pos_integer`

  *Default*: `100`

  Query inflight window. When query_mode is set to async, this config has to be set to 1 if messages from the same MQTT client have to be strictly ordered.


**bridge_mongodb:creation_opts.batch_time**

  *Type*: `timeout_duration_ms`

  *Default*: `0ms`

  Maximum waiting interval when accumulating a batch at a low message rates for more efficient resource usage.


**bridge_mongodb:creation_opts.enable_queue**

  *Type*: `boolean`

  Deprecated since v5.0.14.


**bridge_mongodb:creation_opts.max_buffer_bytes**

  *Type*: `bytesize`

  *Default*: `256MB`

  Maximum number of bytes to buffer for each buffer worker.



### InfluxDB


InfluxDB's protocol. Support InfluxDB v1.8 and before.

**bridges.influxdb_api_v1.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable this bridge


**bridges.influxdb_api_v1.$name.local_topic**

  *Type*: `string`

  The MQTT topic filter to be forwarded to the InfluxDB. All MQTT 'PUBLISH' messages with the topic
matching the local_topic will be forwarded.<br/>
NOTE: if this bridge is used as the action of a rule (EMQX rule engine), and also local_topic is
configured, then both the data got from the rule and the MQTT messages that match local_topic
will be forwarded.


**bridges.influxdb_api_v1.$name.write_syntax**

  *Type*: `emqx_bridge_influxdb:write_syntax`

  Conf of InfluxDB line protocol to write data points. It is a text-based format that provides the measurement, tag set, field set, and timestamp of a data point, and placeholder supported.
See also [InfluxDB 2.3 Line Protocol](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/) and
[InfluxDB 1.8 Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_tutorial/) <br/>
TLDR:<br/>
```
<measurement>[,<tag_key>=<tag_value>[,<tag_key>=<tag_value>]] <field_key>=<field_value>[,<field_key>=<field_value>] [<timestamp>]
```
Please note that a placeholder for an integer value must be annotated with a suffix `i`. For example `${payload.int_value}i`.


**bridges.influxdb_api_v1.$name.resource_opts**

  *Type*: `resource_schema:creation_opts`

  *Default*: `{}`

  Resource options.


**bridges.influxdb_api_v1.$name.server**

  *Type*: `string`

  *Default*: `127.0.0.1:8086`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The InfluxDB default port 8086 is used if `[:Port]` is not specified.


**bridges.influxdb_api_v1.$name.precision**

  *Type*: `enum`

  *Default*: `ms`

  *Optional*: `ns | us | ms | s`

  InfluxDB time precision.


**bridges.influxdb_api_v1.$name.database**

  *Type*: `string`

  InfluxDB database.


**bridges.influxdb_api_v1.$name.username**

  *Type*: `string`

  InfluxDB username.


**bridges.influxdb_api_v1.$name.password**

  *Type*: `string`

  InfluxDB password.


**bridges.influxdb_api_v1.$name.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.




InfluxDB's protocol. Support InfluxDB v2.0 and after.

**bridges.influxdb_api_v2.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable this bridge


**bridges.influxdb_api_v2.$name.local_topic**

  *Type*: `string`

  The MQTT topic filter to be forwarded to the InfluxDB. All MQTT 'PUBLISH' messages with the topic
matching the local_topic will be forwarded.<br/>
NOTE: if this bridge is used as the action of a rule (EMQX rule engine), and also local_topic is
configured, then both the data got from the rule and the MQTT messages that match local_topic
will be forwarded.


**bridges.influxdb_api_v2.$name.write_syntax**

  *Type*: `emqx_bridge_influxdb:write_syntax`

  Conf of InfluxDB line protocol to write data points. It is a text-based format that provides the measurement, tag set, field set, and timestamp of a data point, and placeholder supported.
See also [InfluxDB 2.3 Line Protocol](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/) and
[InfluxDB 1.8 Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_tutorial/) <br/>
TLDR:<br/>
```
<measurement>[,<tag_key>=<tag_value>[,<tag_key>=<tag_value>]] <field_key>=<field_value>[,<field_key>=<field_value>] [<timestamp>]
```
Please note that a placeholder for an integer value must be annotated with a suffix `i`. For example `${payload.int_value}i`.


**bridges.influxdb_api_v2.$name.resource_opts**

  *Type*: `resource_schema:creation_opts`

  *Default*: `{}`

  Resource options.


**bridges.influxdb_api_v2.$name.server**

  *Type*: `string`

  *Default*: `127.0.0.1:8086`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The InfluxDB default port 8086 is used if `[:Port]` is not specified.


**bridges.influxdb_api_v2.$name.precision**

  *Type*: `enum`

  *Default*: `ms`

  *Optional*: `ns | us | ms | s`

  InfluxDB time precision.


**bridges.influxdb_api_v2.$name.bucket**

  *Type*: `string`

  InfluxDB bucket name.


**bridges.influxdb_api_v2.$name.org**

  *Type*: `string`

  Organization name of InfluxDB.


**bridges.influxdb_api_v2.$name.token**

  *Type*: `string`

  InfluxDB token.


**bridges.influxdb_api_v2.$name.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



### PostgreSQL


Configuration for a PostgreSQL bridge.

**bridge_pgsql:config.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable this bridge


**bridge_pgsql:config.sql**

  *Type*: `string`

  *Default*: `insert into t_mqtt_msg(msgid, topic, qos, payload, arrived) values (${id}, ${topic}, ${qos}, ${payload}, TO_TIMESTAMP((${timestamp} :: bigint)/1000))`

  SQL Template


**bridge_pgsql:config.local_topic**

  *Type*: `string`

  The MQTT topic filter to be forwarded to PostgreSQL. All MQTT 'PUBLISH' messages with the topic
matching the local_topic will be forwarded.<br/>
NOTE: if this bridge is used as the action of a rule (EMQX rule engine), and also local_topic is
configured, then both the data got from the rule and the MQTT messages that match local_topic
will be forwarded.


**bridge_pgsql:config.resource_opts**

  *Type*: `resource_schema:creation_opts`

  *Default*: `{}`

  Resource options.


**bridge_pgsql:config.server**

  *Type*: `string`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The PostgreSQL default port 5432 is used if `[:Port]` is not specified.


**bridge_pgsql:config.database**

  *Type*: `string`

  Database name.


**bridge_pgsql:config.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**bridge_pgsql:config.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**bridge_pgsql:config.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**bridge_pgsql:config.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.


**bridge_pgsql:config.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



### TDengine


Configuration for a TDengine bridge.

**bridges.tdengine.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable this bridge


**bridges.tdengine.$name.sql**

  *Type*: `string`

  *Default*: `insert into t_mqtt_msg(ts, msgid, mqtt_topic, qos, payload, arrived) values (${ts}, '${id}', '${topic}', ${qos}, '${payload}', ${timestamp})`

  SQL Template


**bridges.tdengine.$name.local_topic**

  *Type*: `string`

  The MQTT topic filter to be forwarded to TDengine. All MQTT 'PUBLISH' messages with the topic
matching the local_topic will be forwarded.<br/>
NOTE: if this bridge is used as the action of a rule (EMQX rule engine), and also local_topic is
configured, then both the data got from the rule and the MQTT messages that match local_topic
will be forwarded.


**bridges.tdengine.$name.resource_opts**

  *Type*: `resource_schema:creation_opts`

  *Default*: `{}`

  Resource options.


**bridges.tdengine.$name.server**

  *Type*: `string`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The TDengine default port 6041 is used if `[:Port]` is not specified.


**bridges.tdengine.$name.database**

  *Type*: `string`

  Database name.


**bridges.tdengine.$name.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**bridges.tdengine.$name.username**

  *Type*: `string`

  *Default*: `root`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**bridges.tdengine.$name.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**bridges.tdengine.$name.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.



### TimescaleDB

### Apache IoTDB


Parameters for basic authentication.

**bridges.iotdb.$name.authentication.username**

  *Type*: `string`

  The username as configured at the IoTDB REST interface


**bridges.iotdb.$name.authentication.password**

  *Type*: `string`

  The password as configured at the IoTDB REST interface




Configuration for Apache IoTDB bridge.

**bridges.iotdb.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable this bridge


**bridges.iotdb.$name.authentication**

  *Type*: [bridge_iotdb:auth_basic](#bridge_iotdb:auth_basic)

  *Default*: `auth_basic`

  Authentication configuration


**bridges.iotdb.$name.is_aligned**

  *Type*: `boolean`

  *Default*: `false`

  Whether to align the timeseries


**bridges.iotdb.$name.device_id**

  *Type*: `string`

  The IoTDB device ID this data should be inserted for.
If left empty, the MQTT message payload must contain a `device_id` field,
or EMQX's rule-engine SQL must produce a `device_id` field.


**bridges.iotdb.$name.iotdb_version**

  *Type*: `enum`

  *Default*: `v1.1.x`

  *Optional*: `v1.1.x | v1.0.x | v0.13.x`

  The version of the IoTDB system to connect to.


**bridges.iotdb.$name.resource_opts**

  *Type*: `bridge_iotdb:creation_opts`

  *Default*: `{}`

  Resource options.


**bridges.iotdb.$name.connect_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  The timeout when connecting to the HTTP server.


**bridges.iotdb.$name.retry_interval**

  *Type*: `timeout_duration`

  Deprecated since 5.0.4.


**bridges.iotdb.$name.pool_type**

  *Type*: `emqx_bridge_http_connector:pool_type`

  *Default*: `random`

  The type of the pool. Can be one of `random`, `hash`.


**bridges.iotdb.$name.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  The pool size.


**bridges.iotdb.$name.enable_pipelining**

  *Type*: `pos_integer`

  *Default*: `100`

  A positive integer. Whether to send HTTP requests continuously, when set to 1, it means that after each HTTP request is sent, you need to wait for the server to return and then continue to send the next request.


**bridges.iotdb.$name.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.


**bridges.iotdb.$name.base_url**

  *Type*: `url`

  The base URL of the external IoTDB service's REST interface.


**bridges.iotdb.$name.max_retries**

  *Type*: `non_neg_integer`

  *Default*: `2`

  HTTP request max retry times if failed.




Creation options.

**bridges.iotdb.$name.resource_opts.worker_pool_size**

  *Type*: `integer`

  *Default*: `16`

  *Optional*: `1-1024`

  The number of buffer workers. Only applicable for egress type bridges.
For bridges only have ingress direction data flow, it can be set to 0 otherwise must be greater than 0.


**bridges.iotdb.$name.resource_opts.health_check_interval**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  Health check interval.


**bridges.iotdb.$name.resource_opts.start_after_created**

  *Type*: `boolean`

  *Default*: `true`

  Whether start the resource right after created.


**bridges.iotdb.$name.resource_opts.start_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  Time interval to wait for an auto-started resource to become healthy before responding resource creation requests.


**bridges.iotdb.$name.resource_opts.auto_restart_interval**

  *Type*: `infinity | duration_ms`

  Deprecated since 5.1.0.


**bridges.iotdb.$name.resource_opts.query_mode**

  *Type*: `enum`

  *Default*: `async`

  *Optional*: `sync | async`

  Query mode. Optional 'sync/async', default 'async'.


**bridges.iotdb.$name.resource_opts.request_ttl**

  *Type*: `timeout_duration_ms | infinity`

  *Default*: `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.


**bridges.iotdb.$name.resource_opts.inflight_window**

  *Type*: `pos_integer`

  *Default*: `100`

  Query inflight window. When query_mode is set to async, this config has to be set to 1 if messages from the same MQTT client have to be strictly ordered.


**bridges.iotdb.$name.resource_opts.enable_queue**

  *Type*: `boolean`

  Deprecated since v5.0.14.


**bridges.iotdb.$name.resource_opts.max_buffer_bytes**

  *Type*: `bytesize`

  *Default*: `256MB`

  Maximum number of bytes to buffer for each buffer worker.



### MatrixDB

### OpenTSDB


Configuration for an OpenTSDB bridge.

**bridges.opents.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable this bridge


**bridges.opents.$name.resource_opts**

  *Type*: `resource_schema:creation_opts`

  *Default*: `{}`

  Resource options.


**bridges.opents.$name.server**

  *Type*: `string`

  The URL of OpenTSDB endpoint.


**bridges.opents.$name.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**bridges.opents.$name.summary**

  *Type*: `boolean`

  *Default*: `true`

  Whether to return summary information.


**bridges.opents.$name.details**

  *Type*: `boolean`

  *Default*: `false`

  Whether to return detailed information.


**bridges.opents.$name.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.



### GreptimeDB


GreptimeDB's protocol. Support GreptimeDB v1.8 and before.

**bridges.greptimedb.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable this bridge


**bridges.greptimedb.$name.local_topic**

  *Type*: `string`

  The MQTT topic filter to be forwarded to the GreptimeDB. All MQTT 'PUBLISH' messages with the topic
matching the local_topic will be forwarded.<br/>
NOTE: if this bridge is used as the action of a rule (EMQX rule engine), and also local_topic is
configured, then both the data got from the rule and the MQTT messages that match local_topic
will be forwarded.


**bridges.greptimedb.$name.write_syntax**

  *Type*: `emqx_bridge_influxdb:write_syntax`

  Conf of GreptimeDB gRPC protocol to write data points. Write syntax is a text-based format that provides the measurement, tag set, field set, and timestamp of a data point, and placeholder supported, which is the same as InfluxDB line protocol.
See also [InfluxDB 2.3 Line Protocol](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/) and
[GreptimeDB 1.8 Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_tutorial/) <br/>
TLDR:<br/>
```
<measurement>[,<tag_key>=<tag_value>[,<tag_key>=<tag_value>]] <field_key>=<field_value>[,<field_key>=<field_value>] [<timestamp>]
```
Please note that a placeholder for an integer value must be annotated with a suffix `i`. For example `${payload.int_value}i`.


**bridges.greptimedb.$name.resource_opts**

  *Type*: `resource_schema:creation_opts`

  *Default*: `{}`

  Resource options.


**bridges.greptimedb.$name.server**

  *Type*: `string`

  *Default*: `127.0.0.1:4001`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The GreptimeDB default port 8086 is used if `[:Port]` is not specified.


**bridges.greptimedb.$name.precision**

  *Type*: `enum`

  *Default*: `ms`

  *Optional*: `ns | us | ms | s`

  GreptimeDB time precision.


**bridges.greptimedb.$name.dbname**

  *Type*: `string`

  GreptimeDB database.


**bridges.greptimedb.$name.username**

  *Type*: `string`

  GreptimeDB username.


**bridges.greptimedb.$name.password**

  *Type*: `string`

  GreptimeDB password.


**bridges.greptimedb.$name.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



### ClickHouse


Configuration for a Clickhouse bridge.

**bridges.clickhouse.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable this bridge


**bridges.clickhouse.$name.sql**

  *Type*: `string`

  *Default*: `INSERT INTO mqtt_test(payload, arrived) VALUES ('${payload}', ${timestamp})`

  The template string can contain ${field} placeholders for message metadata and payload field. Make sure that the inserted values are formatted and escaped correctly. [Prepared Statement](https://docs.emqx.com/en/enterprise/v5.0/data-integration/data-bridges.html#Prepared-Statement) is not supported.


**bridges.clickhouse.$name.batch_value_separator**

  *Type*: `string`

  *Default*: `, `

  The default value ',' works for the VALUES format. You can also use other separator if other format is specified. See [INSERT INTO Statement](https://clickhouse.com/docs/en/sql-reference/statements/insert-into).


**bridges.clickhouse.$name.local_topic**

  *Type*: `string`

  The MQTT topic filter to be forwarded to Clickhouse. All MQTT 'PUBLISH' messages with the topic
matching the local_topic will be forwarded.<br/>
NOTE: if this bridge is used as the action of a rule (EMQX rule engine), and also local_topic is
configured, then both the data got from the rule and the MQTT messages that match local_topic
will be forwarded.


**bridges.clickhouse.$name.resource_opts**

  *Type*: `bridge_clickhouse:creation_opts`

  *Default*: `{}`

  Resource options.


**bridges.clickhouse.$name.url**

  *Type*: `emqx_bridge_clickhouse_connector:url`

  The HTTP URL to the Clickhouse server that you want to connect to (for example http://myhostname:8123)


**bridges.clickhouse.$name.connect_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  The timeout when connecting to the Clickhouse server.


**bridges.clickhouse.$name.database**

  *Type*: `string`

  Database name.


**bridges.clickhouse.$name.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**bridges.clickhouse.$name.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**bridges.clickhouse.$name.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**bridges.clickhouse.$name.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.




Creation options.

**bridges.clickhouse.$name.resource_opts.worker_pool_size**

  *Type*: `integer`

  *Default*: `16`

  *Optional*: `1-1024`

  The number of buffer workers. Only applicable for egress type bridges.
For bridges only have ingress direction data flow, it can be set to 0 otherwise must be greater than 0.


**bridges.clickhouse.$name.resource_opts.health_check_interval**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  Health check interval.


**bridges.clickhouse.$name.resource_opts.start_after_created**

  *Type*: `boolean`

  *Default*: `true`

  Whether start the resource right after created.


**bridges.clickhouse.$name.resource_opts.start_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  Time interval to wait for an auto-started resource to become healthy before responding resource creation requests.


**bridges.clickhouse.$name.resource_opts.auto_restart_interval**

  *Type*: `infinity | duration_ms`

  Deprecated since 5.1.0.


**bridges.clickhouse.$name.resource_opts.query_mode**

  *Type*: `enum`

  *Default*: `async`

  *Optional*: `sync | async`

  Query mode. Optional 'sync/async', default 'async'.


**bridges.clickhouse.$name.resource_opts.request_ttl**

  *Type*: `timeout_duration_ms | infinity`

  *Default*: `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.


**bridges.clickhouse.$name.resource_opts.inflight_window**

  *Type*: `pos_integer`

  *Default*: `100`

  Query inflight window. When query_mode is set to async, this config has to be set to 1 if messages from the same MQTT client have to be strictly ordered.


**bridges.clickhouse.$name.resource_opts.batch_size**

  *Type*: `pos_integer`

  *Default*: `1`

  Maximum batch count. If equal to 1, there's effectively no batching.


**bridges.clickhouse.$name.resource_opts.batch_time**

  *Type*: `timeout_duration_ms`

  *Default*: `0ms`

  Maximum waiting interval when accumulating a batch at a low message rates for more efficient resource usage.


**bridges.clickhouse.$name.resource_opts.enable_queue**

  *Type*: `boolean`

  Deprecated since v5.0.14.


**bridges.clickhouse.$name.resource_opts.max_buffer_bytes**

  *Type*: `bytesize`

  *Default*: `256MB`

  Maximum number of bytes to buffer for each buffer worker.



### DynamoDB


Configuration for a DynamoDB bridge.

**bridges.dynamo.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable this bridge


**bridges.dynamo.$name.template**

  *Type*: `string`

  *Default*: `""`

  Template, the default value is empty. When this value is empty the whole message will be stored in the database.<br />
The template can be any valid JSON with placeholders and make sure all keys for table are here, example:<br />
  <code>{"id" : "${id}", "clientid" : "${clientid}", "data" : "${payload.data}"}</code>


**bridges.dynamo.$name.local_topic**

  *Type*: `string`

  The MQTT topic filter to be forwarded to DynamoDB. All MQTT `PUBLISH` messages with the topic
matching the `local_topic` will be forwarded.<br/>
NOTE: if this bridge is used as the action of a rule (EMQX rule engine), and also `local_topic` is
configured, then both the data got from the rule and the MQTT messages that match `local_topic`
will be forwarded.


**bridges.dynamo.$name.resource_opts**

  *Type*: `bridge_dynamo:creation_opts`

  *Default*: `{}`

  Resource options.


**bridges.dynamo.$name.url**

  *Type*: `string`

  The url of DynamoDB endpoint.


**bridges.dynamo.$name.table**

  *Type*: `string`

  DynamoDB Table.


**bridges.dynamo.$name.aws_access_key_id**

  *Type*: `string`

  Access Key ID for connecting to DynamoDB.


**bridges.dynamo.$name.aws_secret_access_key**

  *Type*: `string`

  AWS Secret Access Key for connecting to DynamoDB.


**bridges.dynamo.$name.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**bridges.dynamo.$name.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.




Creation options.

**bridges.dynamo.$name.resource_opts.worker_pool_size**

  *Type*: `integer`

  *Default*: `16`

  *Optional*: `1-1024`

  The number of buffer workers. Only applicable for egress type bridges.
For bridges only have ingress direction data flow, it can be set to 0 otherwise must be greater than 0.


**bridges.dynamo.$name.resource_opts.health_check_interval**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  Health check interval.


**bridges.dynamo.$name.resource_opts.start_after_created**

  *Type*: `boolean`

  *Default*: `true`

  Whether start the resource right after created.


**bridges.dynamo.$name.resource_opts.start_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  Time interval to wait for an auto-started resource to become healthy before responding resource creation requests.


**bridges.dynamo.$name.resource_opts.auto_restart_interval**

  *Type*: `infinity | duration_ms`

  Deprecated since 5.1.0.


**bridges.dynamo.$name.resource_opts.query_mode**

  *Type*: `enum`

  *Default*: `async`

  *Optional*: `sync | async`

  Query mode. Optional 'sync/async', default 'async'.


**bridges.dynamo.$name.resource_opts.request_ttl**

  *Type*: `timeout_duration_ms | infinity`

  *Default*: `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.


**bridges.dynamo.$name.resource_opts.inflight_window**

  *Type*: `pos_integer`

  *Default*: `100`

  Query inflight window. When query_mode is set to async, this config has to be set to 1 if messages from the same MQTT client have to be strictly ordered.


**bridges.dynamo.$name.resource_opts.batch_size**

  *Type*: `pos_integer`

  *Default*: `1`

  Maximum batch count. If equal to 1, there's effectively no batching.


**bridges.dynamo.$name.resource_opts.batch_time**

  *Type*: `timeout_duration_ms`

  *Default*: `0ms`

  Maximum waiting interval when accumulating a batch at a low message rates for more efficient resource usage.


**bridges.dynamo.$name.resource_opts.enable_queue**

  *Type*: `boolean`

  Deprecated since v5.0.14.


**bridges.dynamo.$name.resource_opts.max_buffer_bytes**

  *Type*: `bytesize`

  *Default*: `256MB`

  Maximum number of bytes to buffer for each buffer worker.



### Cassandra


Configuration for a Cassandra bridge.

**bridges.cassandra.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable this bridge


**bridges.cassandra.$name.cql**

  *Type*: `string`

  *Default*: `insert into mqtt_msg(topic, msgid, sender, qos, payload, arrived, retain) values (${topic}, ${id}, ${clientid}, ${qos}, ${payload}, ${timestamp}, ${flags.retain})`

  CQL Template


**bridges.cassandra.$name.local_topic**

  *Type*: `string`

  The MQTT topic filter to be forwarded to Cassandra. All MQTT 'PUBLISH' messages with the topic
matching the local_topic will be forwarded.<br/>
NOTE: if this bridge is used as the action of a rule (EMQX rule engine), and also local_topic is
configured, then both the data got from the rule and the MQTT messages that match local_topic
will be forwarded.


**bridges.cassandra.$name.resource_opts**

  *Type*: `resource_schema:creation_opts`

  *Default*: `{}`

  Resource options.


**bridges.cassandra.$name.servers**

  *Type*: `string`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port][,Host2:Port]`.<br/>
The Cassandra default port 9042 is used if `[:Port]` is not specified.


**bridges.cassandra.$name.keyspace**

  *Type*: `string`

  Keyspace name to connect to.


**bridges.cassandra.$name.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**bridges.cassandra.$name.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**bridges.cassandra.$name.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**bridges.cassandra.$name.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.


**bridges.cassandra.$name.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



### Microsoft SQL Server


Configuration for a Microsoft SQL Server bridge.

**bridges.sqlserver.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable this bridge


**bridges.sqlserver.$name.sql**

  *Type*: `string`

  *Default*: `insert into t_mqtt_msg(msgid, topic, qos, payload) values ( ${id}, ${topic}, ${qos}, ${payload} )`

  SQL Template


**bridges.sqlserver.$name.driver**

  *Type*: `string`

  *Default*: `ms-sql`

  SQL Server Driver Name


**bridges.sqlserver.$name.local_topic**

  *Type*: `string`

  The MQTT topic filter to be forwarded to Microsoft SQL Server. All MQTT 'PUBLISH' messages with the topic
matching the local_topic will be forwarded.<br/>
NOTE: if this bridge is used as the action of a rule (EMQX rule engine), and also local_topic is
configured, then both the data got from the rule and the MQTT messages that match local_topic
will be forwarded.


**bridges.sqlserver.$name.resource_opts**

  *Type*: `bridge_sqlserver:creation_opts`

  *Default*: `{}`

  Resource options.


**bridges.sqlserver.$name.server**

  *Type*: `string`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The SQL Server default port 1433 is used if `[:Port]` is not specified.


**bridges.sqlserver.$name.database**

  *Type*: `string`

  Database name.


**bridges.sqlserver.$name.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**bridges.sqlserver.$name.username**

  *Type*: `string`

  *Default*: `sa`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**bridges.sqlserver.$name.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**bridges.sqlserver.$name.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.




Creation options.

**bridges.sqlserver.$name.resource_opts.worker_pool_size**

  *Type*: `integer`

  *Default*: `16`

  *Optional*: `1-1024`

  The number of buffer workers. Only applicable for egress type bridges.
For bridges only have ingress direction data flow, it can be set to 0 otherwise must be greater than 0.


**bridges.sqlserver.$name.resource_opts.health_check_interval**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  Health check interval.


**bridges.sqlserver.$name.resource_opts.start_after_created**

  *Type*: `boolean`

  *Default*: `true`

  Whether start the resource right after created.


**bridges.sqlserver.$name.resource_opts.start_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  Time interval to wait for an auto-started resource to become healthy before responding resource creation requests.


**bridges.sqlserver.$name.resource_opts.auto_restart_interval**

  *Type*: `infinity | duration_ms`

  Deprecated since 5.1.0.


**bridges.sqlserver.$name.resource_opts.query_mode**

  *Type*: `enum`

  *Default*: `async`

  *Optional*: `sync | async`

  Query mode. Optional 'sync/async', default 'async'.


**bridges.sqlserver.$name.resource_opts.request_ttl**

  *Type*: `timeout_duration_ms | infinity`

  *Default*: `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.


**bridges.sqlserver.$name.resource_opts.inflight_window**

  *Type*: `pos_integer`

  *Default*: `100`

  Query inflight window. When query_mode is set to async, this config has to be set to 1 if messages from the same MQTT client have to be strictly ordered.


**bridges.sqlserver.$name.resource_opts.batch_size**

  *Type*: `pos_integer`

  *Default*: `1`

  Maximum batch count. If equal to 1, there's effectively no batching.


**bridges.sqlserver.$name.resource_opts.batch_time**

  *Type*: `timeout_duration_ms`

  *Default*: `0ms`

  Maximum waiting interval when accumulating a batch at a low message rates for more efficient resource usage.


**bridges.sqlserver.$name.resource_opts.enable_queue**

  *Type*: `boolean`

  Deprecated since v5.0.14.


**bridges.sqlserver.$name.resource_opts.max_buffer_bytes**

  *Type*: `bytesize`

  *Default*: `256MB`

  Maximum number of bytes to buffer for each buffer worker.



### Oracle Database


Configuration for an Oracle Database bridge.

**bridges.oracle.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable this bridge


**bridges.oracle.$name.sql**

  *Type*: `string`

  *Default*: `insert into t_mqtt_msgs(msgid, topic, qos, payload) values (${id}, ${topic}, ${qos}, ${payload})`

  SQL Template. The template string can contain placeholders for message metadata and payload field. The placeholders are inserted without any checking and special formatting, so it is important to ensure that the inserted values are formatted and escaped correctly.


**bridges.oracle.$name.local_topic**

  *Type*: `string`

  The MQTT topic filter to be forwarded to Oracle Database. All MQTT 'PUBLISH' messages with the topic matching the local_topic will be forwarded.<br/>NOTE: if this bridge is used as the action of a rule (EMQX rule engine), and also local_topic is configured, then both the data got from the rule and the MQTT messages that match local_topic will be forwarded.


**bridges.oracle.$name.resource_opts**

  *Type*: `resource_schema:creation_opts`

  *Default*: `{}`

  Resource options.


**bridges.oracle.$name.server**

  *Type*: `string`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>A host entry has the following form: `Host[:Port]`.<br/>The Oracle Database default port 1521 is used if `[:Port]` is not specified.


**bridges.oracle.$name.sid**

  *Type*: `string`

  Sid for Oracle Database.


**bridges.oracle.$name.service_name**

  *Type*: `string`

  Service Name for Oracle Database.


**bridges.oracle.$name.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**bridges.oracle.$name.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**bridges.oracle.$name.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**bridges.oracle.$name.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.



### HStreamDB


Configuration for an HStreamDB bridge.

**bridges.hstreamdb.$name.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable or disable this bridge


**bridges.hstreamdb.$name.direction**

  *Type*: `egress`

  *Default*: `egress`

  The direction of this bridge, MUST be 'egress'


**bridges.hstreamdb.$name.local_topic**

  *Type*: `string`

  The MQTT topic filter to be forwarded to the HStreamDB. All MQTT 'PUBLISH' messages with the topic
matching the local_topic will be forwarded.<br/>
NOTE: if this bridge is used as the action of a rule (EMQX rule engine), and also local_topic is
configured, then both the data got from the rule and the MQTT messages that match local_topic
will be forwarded.


**bridges.hstreamdb.$name.record_template**

  *Type*: `string`

  *Default*: `${payload}`

  The HStream Record template to be forwarded to the HStreamDB. Placeholders supported.<br />
NOTE: When you use `raw record` template (which means the data is not a valid JSON), you should use `read` or `subscription` in HStream to get the data.


**bridges.hstreamdb.$name.resource_opts**

  *Type*: `resource_schema:creation_opts`

  *Default*: `{}`

  Resource options.


**bridges.hstreamdb.$name.url**

  *Type*: `string`

  *Default*: `http://127.0.0.1:6570`

  HStreamDB Server URL. Using gRPC http server address.


**bridges.hstreamdb.$name.stream**

  *Type*: `string`

  HStreamDB Stream Name.


**bridges.hstreamdb.$name.partition_key**

  *Type*: `string`

  HStreamDB Partition Key. Placeholders supported.


**bridges.hstreamdb.$name.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**bridges.hstreamdb.$name.grpc_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `30s`

  HStreamDB gRPC Timeout.


**bridges.hstreamdb.$name.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



{% endemqxee %}

### Appendix: Common configurations


Creation options.

**resource_schema:creation_opts.worker_pool_size**

  *Type*: `integer`

  *Default*: `16`

  *Optional*: `1-1024`

  The number of buffer workers. Only applicable for egress type bridges.
For bridges only have ingress direction data flow, it can be set to 0 otherwise must be greater than 0.


**resource_schema:creation_opts.health_check_interval**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  Health check interval.


**resource_schema:creation_opts.start_after_created**

  *Type*: `boolean`

  *Default*: `true`

  Whether start the resource right after created.


**resource_schema:creation_opts.start_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `5s`

  Time interval to wait for an auto-started resource to become healthy before responding resource creation requests.


**resource_schema:creation_opts.auto_restart_interval**

  *Type*: `infinity | duration_ms`

  Deprecated since 5.1.0.


**resource_schema:creation_opts.query_mode**

  *Type*: `enum`

  *Default*: `async`

  *Optional*: `sync | async`

  Query mode. Optional 'sync/async', default 'async'.


**resource_schema:creation_opts.request_ttl**

  *Type*: `timeout_duration_ms | infinity`

  *Default*: `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.


**resource_schema:creation_opts.inflight_window**

  *Type*: `pos_integer`

  *Default*: `100`

  Query inflight window. When query_mode is set to async, this config has to be set to 1 if messages from the same MQTT client have to be strictly ordered.


**resource_schema:creation_opts.batch_size**

  *Type*: `pos_integer`

  *Default*: `1`

  Maximum batch count. If equal to 1, there's effectively no batching.


**resource_schema:creation_opts.batch_time**

  *Type*: `timeout_duration_ms`

  *Default*: `0ms`

  Maximum waiting interval when accumulating a batch at a low message rates for more efficient resource usage.


**resource_schema:creation_opts.enable_queue**

  *Type*: `boolean`

  Deprecated since v5.0.14.


**resource_schema:creation_opts.max_buffer_bytes**

  *Type*: `bytesize`

  *Default*: `256MB`

  Maximum number of bytes to buffer for each buffer worker.



## Plugin


Manage EMQX plugins.<br/>
Plugins can be pre-built as a part of EMQX package,
or installed as a standalone package in a location specified by
<code>install_dir</code> config key<br/>
The standalone-installed plugins are referred to as 'external' plugins.

**plugins.states**

  *Type*: `array`

  *Default*: `[]`

  An array of plugins in the desired states.<br/>
The plugins are started in the defined order


**plugins.install_dir**

  *Type*: `string`

  *Default*: `plugins`

  The installation directory for the external plugins.
The plugin beam files and configuration files should reside in
the subdirectory named as <code>emqx_foo_bar-0.1.0</code>.
<br/>
NOTE: For security reasons, this directory should **NOT** be writable
by anyone except <code>emqx</code> (or any user which runs EMQX).


**plugins.check_interval**

  *Type*: `duration`

  Deprecated since 5.0.24.




A per-plugin config to describe the desired state of the plugin.

**plugins.states.$INDEX.name_vsn**

  *Type*: `string`

  The {name}-{version} of the plugin.<br/>
It should match the plugin application name-version as the for the plugin release package name<br/>
For example: my_plugin-0.1.0.


**plugins.states.$INDEX.enable**

  *Type*: `boolean`

  Set to 'true' to enable this plugin



## ExHook


External hook (exhook) configuration.

**exhook.servers**

  *Type*: `array`

  *Default*: `[]`

  List of exhook servers




gRPC server configuration.

**exhook.servers.$INDEX.name**

  *Type*: `string`

  Name of the exhook server


**exhook.servers.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable this Exhook server


**exhook.servers.$INDEX.url**

  *Type*: `string`

  URL of the gRPC server


**exhook.servers.$INDEX.request_timeout**

  *Type*: `timeout_duration`

  *Default*: `5s`

  The timeout of request gRPC server


**exhook.servers.$INDEX.failed_action**

  *Type*: `enum`

  *Default*: `deny`

  *Optional*: `deny | ignore`

  The value that is returned when the request to the gRPC server fails for any reason


**exhook.servers.$INDEX.ssl**

  *Type*: `exhook:ssl_conf`


**exhook.servers.$INDEX.socket_options**

  *Type*: `exhook:socket_options`

  *Default*: `{"nodelay":true,"keepalive":true}`


**exhook.servers.$INDEX.auto_reconnect**

  *Type*: `false | timeout_duration`

  *Default*: `60s`

  Whether to automatically reconnect (initialize) the gRPC server.
When gRPC is not available, Exhook tries to request the gRPC service at that interval and reinitialize the list of mounted hooks.


**exhook.servers.$INDEX.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  The process pool size for gRPC client




Connection socket options

**exhook.servers.$INDEX.socket_options.keepalive**

  *Type*: `boolean`

  *Default*: `true`

  Enables/disables periodic transmission on a connected socket when no other data is exchanged.
If the other end does not respond, the connection is considered broken and an error message is sent to the controlling process.


**exhook.servers.$INDEX.socket_options.nodelay**

  *Type*: `boolean`

  *Default*: `true`

  If true, option TCP_NODELAY is turned on for the socket,
which means that also small amounts of data are sent immediately


**exhook.servers.$INDEX.socket_options.recbuf**

  *Type*: `bytesize`

  The minimum size of receive buffer to use for the socket


**exhook.servers.$INDEX.socket_options.sndbuf**

  *Type*: `bytesize`

  The minimum size of send buffer to use for the socket




SSL client configuration.

**exhook.servers.$INDEX.ssl.cacertfile**

  *Type*: `string`

  Trusted PEM format CA certificates bundle file.<br/>
The certificates in this file are used to verify the TLS peer's certificates.
Append new certificates to the file if new CAs are to be trusted.
There is no need to restart EMQX to have the updated file loaded, because
the system regularly checks if file has been updated (and reload).<br/>
NOTE: invalidating (deleting) a certificate from the file will not affect
already established connections.


**exhook.servers.$INDEX.ssl.cacerts**

  *Type*: `boolean`

  Deprecated since 5.1.4.


**exhook.servers.$INDEX.ssl.certfile**

  *Type*: `string`

  PEM format certificates chain file.<br/>
The certificates in this file should be in reversed order of the certificate
issue chain. That is, the host's certificate should be placed in the beginning
of the file, followed by the immediate issuer certificate and so on.
Although the root CA certificate is optional, it should be placed at the end of
the file if it is to be added.


**exhook.servers.$INDEX.ssl.keyfile**

  *Type*: `string`

  PEM format private key file.


**exhook.servers.$INDEX.ssl.verify**

  *Type*: `enum`

  *Default*: `verify_none`

  *Optional*: `verify_peer | verify_none`

  Enable or disable peer verification.


**exhook.servers.$INDEX.ssl.reuse_sessions**

  *Type*: `boolean`

  *Default*: `true`

  Enable TLS session reuse.<br/>
Has no effect when TLS version is configured (or negotiated) to 1.3


**exhook.servers.$INDEX.ssl.depth**

  *Type*: `non_neg_integer`

  *Default*: `10`

  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br/>
if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br/>
if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.


**exhook.servers.$INDEX.ssl.password**

  *Type*: `string`

  String containing the user's password. Only used if the private key file is password-protected.


**exhook.servers.$INDEX.ssl.versions**

  *Type*: `array`

  *Default*: `["tlsv1.3","tlsv1.2"]`

  All TLS/DTLS versions to be supported.<br/>
NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config.<br/>
In case PSK cipher suites are intended, make sure to configure
<code>['tlsv1.2', 'tlsv1.1']</code> here.


**exhook.servers.$INDEX.ssl.ciphers**

  *Type*: `array`

  *Default*: `[]`

  This config holds TLS cipher suite names separated by comma,
or as an array of strings. e.g.
<code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> or
<code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>.
<br/>
Ciphers (and their ordering) define the way in which the
client and server encrypts information over the network connection.
Selecting a good cipher suite is critical for the
application's data security, confidentiality and performance.

The names should be in OpenSSL string format (not RFC format).
All default values and examples provided by EMQX config
documentation are all in OpenSSL format.<br/>

NOTE: Certain cipher suites are only compatible with
specific TLS <code>versions</code> ('tlsv1.1', 'tlsv1.2' or 'tlsv1.3')
incompatible cipher suites will be silently dropped.
For instance, if only 'tlsv1.3' is given in the <code>versions</code>,
configuring cipher suites for other versions will have no effect.
<br/>

NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config<br/>
If PSK cipher suites are intended, 'tlsv1.3' should be disabled from <code>versions</code>.<br/>
PSK cipher suites: <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>


**exhook.servers.$INDEX.ssl.secure_renegotiate**

  *Type*: `boolean`

  *Default*: `true`

  SSL parameter renegotiation is a feature that allows a client and a server
to renegotiate the parameters of the SSL connection on the fly.
RFC 5746 defines a more secure way of doing this. By enabling secure renegotiation,
you drop support for the insecure renegotiation, prone to MitM attacks.<br/>
Has no effect when TLS version is configured (or negotiated) to 1.3


**exhook.servers.$INDEX.ssl.log_level**

  *Type*: `enum`

  *Default*: `notice`

  *Optional*: `emergency | alert | critical | error | warning | notice | info | debug | none | all`

  Log level for SSL communication. Default is 'notice'. Set to 'debug' to inspect TLS handshake messages.


**exhook.servers.$INDEX.ssl.hibernate_after**

  *Type*: `duration`

  *Default*: `5s`

  Hibernate the SSL process after idling for amount of time reducing its memory footprint.


**exhook.servers.$INDEX.ssl.enable**

  *Type*: `boolean`

  *Default*: `false`

  Enable TLS.


**exhook.servers.$INDEX.ssl.server_name_indication**

  *Type*: `disable | string`

  Specify the host name to be used in TLS Server Name Indication extension.<br/>
For instance, when connecting to "server.example.net", the genuine server
which accepts the connection and performs TLS handshake may differ from the
host the TLS client initially connects to, e.g. when connecting to an IP address
or when the host has multiple resolvable DNS records <br/>
If not specified, it will default to the host name string which is used
to establish the connection, unless it is IP addressed used.<br/>
The host name is then also used in the host name verification of the peer
certificate.<br/> The special value 'disable' prevents the Server Name
Indication extension from being sent and disables the hostname
verification check.



## Gateway

### CoAP


The CoAP protocol gateway provides EMQX with the access capability of the CoAP protocol.
It allows publishing, subscribing, and receiving messages to EMQX in accordance
with a certain defined CoAP message format.

**gateway.coap.heartbeat**

  *Type*: `emqx_coap_schema:duration`

  *Default*: `30s`

  The gateway server required minimum heartbeat interval.
When connection mode is enabled, this parameter is used to set the minimum heartbeat interval for the connection to be alive


**gateway.coap.connection_required**

  *Type*: `boolean`

  *Default*: `false`

  Enable or disable connection mode.
Connection mode is a feature of non-standard protocols. When connection mode is enabled, it is necessary to maintain the creation, authentication and alive of connection resources


**gateway.coap.notify_type**

  *Type*: `enum`

  *Default*: `qos`

  *Optional*: `non | con | qos`

  The Notification Message will be delivered to the CoAP client if a new message received on an observed topic.
The type of delivered coap message can be set to:<br/>
  - non: Non-confirmable;<br/>
  - con: Confirmable;<br/>
  - qos: Mapping from QoS type of received message, QoS0 -> non, QoS1,2 -> con


**gateway.coap.subscribe_qos**

  *Type*: `enum`

  *Default*: `coap`

  *Optional*: `qos0 | qos1 | qos2 | coap`

  The Default QoS Level indicator for subscribe request.
This option specifies the QoS level for the CoAP Client when establishing a subscription membership, if the subscribe request is not carried `qos` option. The indicator can be set to:<br/>
  - qos0, qos1, qos2: Fixed default QoS level<br/>
  - coap: Dynamic QoS level by the message type of subscribe request<br/>
    * qos0: If the subscribe request is non-confirmable<br/>
    * qos1: If the subscribe request is confirmable


**gateway.coap.publish_qos**

  *Type*: `enum`

  *Default*: `coap`

  *Optional*: `qos0 | qos1 | qos2 | coap`

  The Default QoS Level indicator for publish request.
This option specifies the QoS level for the CoAP Client when publishing a message to EMQX PUB/SUB system, if the publish request is not carried `qos` option. The indicator can be set to:<br/>
  - qos0, qos1, qos2: Fixed default QoS level<br/>
  - coap: Dynamic QoS level by the message type of publish request<br/>
    * qos0: If the publish request is non-confirmable<br/>
    * qos1: If the publish request is confirmable


**gateway.coap.mountpoint**

  *Type*: `string`

  *Default*: `""`

  When publishing or subscribing, prefix all topics with a mountpoint string.
The prefixed string will be removed from the topic name when the message is delivered to the subscriber.
The mountpoint is a way that users can use to implement isolation of message routing between different listeners.
For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint` set to `some_tenant`,
then the client actually subscribes to the topic `some_tenant/t`.
Similarly, if another client B (connected to the same listener as the client A) sends a message to topic `t`,
the message is routed to all the clients subscribed `some_tenant/t`,
so client A will receive the message, with topic name `t`. Set to `""` to disable the feature.
Variables in mountpoint string:<br/>
  - <code>${clientid}</code>: clientid<br/>
  - <code>${username}</code>: username


**gateway.coap.listeners**

  *Type*: `gateway:udp_listeners`


**gateway.coap.enable**

  *Type*: `boolean`

  *Default*: `true`

  Whether to enable this gateway


**gateway.coap.enable_stats**

  *Type*: `boolean`

  *Default*: `true`

  Whether to enable client process statistic


**gateway.coap.idle_timeout**

  *Type*: `emqx_gateway_schema:duration`

  *Default*: `30s`

  The idle time of the client connection process. It has two purposes:
  1. A newly created client process that does not receive any client requests after that time will be closed directly.
  2. A running client process that does not receive any client requests after this time will go into hibernation to save resources.


**gateway.coap.clientinfo_override**

  *Type*: `gateway:clientinfo_override`

  ClientInfo override.



### LwM2M


The LwM2M protocol gateway.

**gateway.lwm2m.xml_dir**

  *Type*: `string`

  The Directory for LwM2M Resource definition.


**gateway.lwm2m.lifetime_min**

  *Type*: `emqx_lwm2m_schema:duration`

  *Default*: `15s`

  Minimum value of lifetime allowed to be set by the LwM2M client.


**gateway.lwm2m.lifetime_max**

  *Type*: `emqx_lwm2m_schema:duration`

  *Default*: `86400s`

  Maximum value of lifetime allowed to be set by the LwM2M client.


**gateway.lwm2m.qmode_time_window**

  *Type*: `emqx_lwm2m_schema:duration_s`

  *Default*: `22s`

  The value of the time window during which the network link is considered valid by the LwM2M Gateway in QMode mode.
For example, after receiving an update message from a client, any messages within this time window are sent directly to the LwM2M client, and all messages beyond this time window are temporarily stored in memory.


**gateway.lwm2m.auto_observe**

  *Type*: `boolean`

  *Default*: `false`

  Automatically observe the object list of REGISTER packet.


**gateway.lwm2m.update_msg_publish_condition**

  *Type*: `enum`

  *Default*: `contains_object_list`

  *Optional*: `always | contains_object_list`

  Policy for publishing UPDATE event message.<br/>
  - always: send update events as long as the UPDATE request is received.<br/>
  - contains_object_list: send update events only if the UPDATE request carries any Object List


**gateway.lwm2m.translators**

  *Type*: `lwm2m_translators`

  Topic configuration for LwM2M's gateway publishing and subscription.


**gateway.lwm2m.mountpoint**

  *Type*: `string`

  *Default*: `lwm2m/${endpoint_name}/`

  When publishing or subscribing, prefix all topics with a mountpoint string.
The prefixed string will be removed from the topic name when the message is delivered to the subscriber.
The mountpoint is a way that users can use to implement isolation of message routing between different listeners.
For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint` set to `some_tenant`,
then the client actually subscribes to the topic `some_tenant/t`.
Similarly, if another client B (connected to the same listener as the client A) sends a message to topic `t`,
the message is routed to all the clients subscribed `some_tenant/t`,
so client A will receive the message, with topic name `t`. Set to `""` to disable the feature.
Variables in mountpoint string:<br/>
  - <code>${clientid}</code>: clientid<br/>
  - <code>${username}</code>: username


**gateway.lwm2m.listeners**

  *Type*: `gateway:udp_listeners`


**gateway.lwm2m.enable**

  *Type*: `boolean`

  *Default*: `true`

  Whether to enable this gateway


**gateway.lwm2m.enable_stats**

  *Type*: `boolean`

  *Default*: `true`

  Whether to enable client process statistic


**gateway.lwm2m.idle_timeout**

  *Type*: `emqx_gateway_schema:duration`

  *Default*: `30s`

  The idle time of the client connection process. It has two purposes:
  1. A newly created client process that does not receive any client requests after that time will be closed directly.
  2. A running client process that does not receive any client requests after this time will go into hibernation to save resources.


**gateway.lwm2m.clientinfo_override**

  *Type*: `gateway:clientinfo_override`

  ClientInfo override.




MQTT topics that correspond to LwM2M events.

**gateway.lwm2m.translators.command**

  *Type*: `translator`

  The topic for receiving downstream commands.
For each new LwM2M client that succeeds in going online, the gateway creates a subscription relationship to receive downstream commands and send it to the LwM2M client


**gateway.lwm2m.translators.response**

  *Type*: `translator`

  The topic for gateway to publish the acknowledge events from LwM2M client


**gateway.lwm2m.translators.notify**

  *Type*: `translator`

  The topic for gateway to publish the notify events from LwM2M client.
After succeed observe a resource of LwM2M client, Gateway will send the notify events via this topic, if the client reports any resource changes


**gateway.lwm2m.translators.register**

  *Type*: `translator`

  The topic for gateway to publish the register events from LwM2M client.


**gateway.lwm2m.translators.update**

  *Type*: `translator`

  The topic for gateway to publish the update events from LwM2M client




MQTT topic that corresponds to a particular type of event.

**translator.topic**

  *Type*: `string`

  Topic Name


**translator.qos**

  *Type*: `qos`

  *Default*: `0`

  QoS Level




Topology of MongoDB.

**topology.max_overflow**

  *Type*: `non_neg_integer`

  *Default*: `0`

  The maximum number of additional workers that can be created when all workers in the pool are busy. This helps to manage temporary spikes in workload by allowing more concurrent connections to the MongoDB server.


**topology.overflow_ttl**

  *Type*: `timeout_duration_ms`

  Period of time before workers that exceed the configured pool size ("overflow") to be terminated.


**topology.overflow_check_period**

  *Type*: `timeout_duration_ms`

  Period for checking if there are more workers than configured ("overflow").


**topology.local_threshold_ms**

  *Type*: `timeout_duration_ms`

  The size of the latency window for selecting among multiple suitable MongoDB instances.


**topology.connect_timeout_ms**

  *Type*: `timeout_duration_ms`

  The duration to attempt a connection before timing out.


**topology.socket_timeout_ms**

  *Type*: `timeout_duration_ms`

  The duration to attempt to send or to receive on a socket before the attempt times out.


**topology.server_selection_timeout_ms**

  *Type*: `timeout_duration_ms`

  Specifies how long to block for server selection before throwing an exception.


**topology.wait_queue_timeout_ms**

  *Type*: `timeout_duration_ms`

  The maximum duration that a worker can wait for a connection to become available.


**topology.heartbeat_frequency_ms**

  *Type*: `timeout_duration_ms`

  *Default*: `200s`

  Controls when the driver checks the state of the MongoDB deployment. Specify the interval between checks, counted from the end of the previous check until the beginning of the next one. If the number of connections is increased (which will happen, for example, if you increase the pool size), you may need to increase this period as well to avoid creating too many log entries in the MongoDB log file.


**topology.min_heartbeat_frequency_ms**

  *Type*: `timeout_duration_ms`

  Controls the minimum amount of time to wait between heartbeats.



### MQTT-SN


The MQTT-SN (MQTT for Sensor Networks) protocol gateway.

**gateway.mqttsn.gateway_id**

  *Type*: `integer`

  *Default*: `1`

  MQTT-SN Gateway ID.
When the <code>broadcast</code> option is enabled, the gateway will broadcast ADVERTISE message with this value


**gateway.mqttsn.broadcast**

  *Type*: `boolean`

  *Default*: `false`

  Whether to periodically broadcast ADVERTISE messages


**gateway.mqttsn.enable_qos3**

  *Type*: `boolean`

  *Default*: `true`

  Allows connectionless clients to publish messages with a Qos of -1.
This feature is defined for very simple client implementations which do not support any other features except this one. There is no connection setup nor tear down, no registration nor subscription. The client just sends its 'PUBLISH' messages to a GW


**gateway.mqttsn.subs_resume**

  *Type*: `boolean`

  *Default*: `false`

  Whether to initiate all subscribed topic name registration messages to the client after the Session has been taken over by a new channel


**gateway.mqttsn.predefined**

  *Type*: `array`

  *Default*: `[]`

  The pre-defined topic IDs and topic names.
A 'pre-defined' topic ID is a topic ID whose mapping to a topic name is known in advance by both the client's application and the gateway


**gateway.mqttsn.mountpoint**

  *Type*: `string`

  *Default*: `""`

  When publishing or subscribing, prefix all topics with a mountpoint string.
The prefixed string will be removed from the topic name when the message is delivered to the subscriber.
The mountpoint is a way that users can use to implement isolation of message routing between different listeners.
For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint` set to `some_tenant`,
then the client actually subscribes to the topic `some_tenant/t`.
Similarly, if another client B (connected to the same listener as the client A) sends a message to topic `t`,
the message is routed to all the clients subscribed `some_tenant/t`,
so client A will receive the message, with topic name `t`. Set to `""` to disable the feature.
Variables in mountpoint string:<br/>
  - <code>${clientid}</code>: clientid<br/>
  - <code>${username}</code>: username


**gateway.mqttsn.listeners**

  *Type*: `gateway:udp_listeners`


**gateway.mqttsn.enable**

  *Type*: `boolean`

  *Default*: `true`

  Whether to enable this gateway


**gateway.mqttsn.enable_stats**

  *Type*: `boolean`

  *Default*: `true`

  Whether to enable client process statistic


**gateway.mqttsn.idle_timeout**

  *Type*: `emqx_gateway_schema:duration`

  *Default*: `30s`

  The idle time of the client connection process. It has two purposes:
  1. A newly created client process that does not receive any client requests after that time will be closed directly.
  2. A running client process that does not receive any client requests after this time will go into hibernation to save resources.


**gateway.mqttsn.clientinfo_override**

  *Type*: `gateway:clientinfo_override`

  ClientInfo override.




The pre-defined topic name corresponding to the pre-defined topic
ID of N.

Note: the pre-defined topic ID of 0 is reserved.

**gateway.mqttsn.predefined.$INDEX.id**

  *Type*: `integer`

  *Optional*: `1-1024`

  Topic ID. Range: 1-65535


**gateway.mqttsn.predefined.$INDEX.topic**

  *Type*: `string`

  Topic Name



### STOMP


The STOMP protocol gateway provides EMQX with the ability to access STOMP
(Simple (or Streaming) Text Orientated Messaging Protocol) protocol.

**gateway.stomp.frame**

  *Type*: `stomp_frame`


**gateway.stomp.mountpoint**

  *Type*: `string`

  *Default*: `""`

  When publishing or subscribing, prefix all topics with a mountpoint string.
The prefixed string will be removed from the topic name when the message is delivered to the subscriber.
The mountpoint is a way that users can use to implement isolation of message routing between different listeners.
For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint` set to `some_tenant`,
then the client actually subscribes to the topic `some_tenant/t`.
Similarly, if another client B (connected to the same listener as the client A) sends a message to topic `t`,
the message is routed to all the clients subscribed `some_tenant/t`,
so client A will receive the message, with topic name `t`. Set to `""` to disable the feature.
Variables in mountpoint string:<br/>
  - <code>${clientid}</code>: clientid<br/>
  - <code>${username}</code>: username


**gateway.stomp.listeners**

  *Type*: `gateway:tcp_listeners`


**gateway.stomp.enable**

  *Type*: `boolean`

  *Default*: `true`

  Whether to enable this gateway


**gateway.stomp.enable_stats**

  *Type*: `boolean`

  *Default*: `true`

  Whether to enable client process statistic


**gateway.stomp.idle_timeout**

  *Type*: `emqx_gateway_schema:duration`

  *Default*: `30s`

  The idle time of the client connection process. It has two purposes:
  1. A newly created client process that does not receive any client requests after that time will be closed directly.
  2. A running client process that does not receive any client requests after this time will go into hibernation to save resources.


**gateway.stomp.clientinfo_override**

  *Type*: `gateway:clientinfo_override`

  ClientInfo override.




Size limits for the STOMP frames.

**gateway.stomp.frame.max_headers**

  *Type*: `non_neg_integer`

  *Default*: `10`

  The maximum number of Header


**gateway.stomp.frame.max_headers_length**

  *Type*: `non_neg_integer`

  *Default*: `1024`

  The maximum string length of the Header Value


**gateway.stomp.frame.max_body_length**

  *Type*: `integer`

  *Default*: `65536`

  Maximum number of bytes of Body allowed per Stomp packet



### ExProto


Settings for EMQX extension protocol (exproto).

**gateway.exproto.server**

  *Type*: `exproto_grpc_server`

  Configurations for starting the <code>ConnectionAdapter</code> service


**gateway.exproto.handler**

  *Type*: `exproto_grpc_handler`

  Configurations for request to <code>ConnectionHandler</code> service


**gateway.exproto.mountpoint**

  *Type*: `string`

  *Default*: `""`

  When publishing or subscribing, prefix all topics with a mountpoint string.
The prefixed string will be removed from the topic name when the message is delivered to the subscriber.
The mountpoint is a way that users can use to implement isolation of message routing between different listeners.
For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint` set to `some_tenant`,
then the client actually subscribes to the topic `some_tenant/t`.
Similarly, if another client B (connected to the same listener as the client A) sends a message to topic `t`,
the message is routed to all the clients subscribed `some_tenant/t`,
so client A will receive the message, with topic name `t`. Set to `""` to disable the feature.
Variables in mountpoint string:<br/>
  - <code>${clientid}</code>: clientid<br/>
  - <code>${username}</code>: username


**gateway.exproto.listeners**

  *Type*: `gateway:tcp_udp_listeners`


**gateway.exproto.enable**

  *Type*: `boolean`

  *Default*: `true`

  Whether to enable this gateway


**gateway.exproto.enable_stats**

  *Type*: `boolean`

  *Default*: `true`

  Whether to enable client process statistic


**gateway.exproto.idle_timeout**

  *Type*: `emqx_gateway_schema:duration`

  *Default*: `30s`

  The idle time of the client connection process. It has two purposes:
  1. A newly created client process that does not receive any client requests after that time will be closed directly.
  2. A running client process that does not receive any client requests after this time will go into hibernation to save resources.


**gateway.exproto.clientinfo_override**

  *Type*: `gateway:clientinfo_override`

  ClientInfo override.




Settings for the exproto gRPC connection handler.

**gateway.exproto.handler.address**

  *Type*: `string`

  gRPC server address.


**gateway.exproto.handler.service_name**

  *Type*: `ConnectionHandler | ConnectionUnaryHandler`

  *Default*: `ConnectionUnaryHandler`

  The service name to handle the connection events.
In the initial version, we expected to use streams to improve the efficiency
of requests in `ConnectionHandler`. But unfortunately, events between different
streams are out of order. It causes the `OnSocketCreated` event to may arrive
later than `OnReceivedBytes`.
So we added the `ConnectionUnaryHandler` service since v5.0.25 and forced
the use of Unary in it to avoid ordering problems.


**gateway.exproto.handler.ssl_options**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  SSL configuration for the gRPC client.




Settings for the exproto gRPC server.

**gateway.exproto.server.bind**

  *Type*: `emqx_exproto_schema:ip_port`

  Listening address and port for the gRPC server.


**gateway.exproto.server.ssl_options**

  *Type*: `ssl_server_opts`

  SSL configuration for the gRPC server.




SSL configuration for the server.

**gateway.exproto.server.ssl_options.cacertfile**

  *Type*: `string`

  *Default*: `${EMQX_ETC_DIR}/certs/cacert.pem`

  Trusted PEM format CA certificates bundle file.<br/>
The certificates in this file are used to verify the TLS peer's certificates.
Append new certificates to the file if new CAs are to be trusted.
There is no need to restart EMQX to have the updated file loaded, because
the system regularly checks if file has been updated (and reload).<br/>
NOTE: invalidating (deleting) a certificate from the file will not affect
already established connections.


**gateway.exproto.server.ssl_options.cacerts**

  *Type*: `boolean`

  Deprecated since 5.1.4.


**gateway.exproto.server.ssl_options.certfile**

  *Type*: `string`

  *Default*: `${EMQX_ETC_DIR}/certs/cert.pem`

  PEM format certificates chain file.<br/>
The certificates in this file should be in reversed order of the certificate
issue chain. That is, the host's certificate should be placed in the beginning
of the file, followed by the immediate issuer certificate and so on.
Although the root CA certificate is optional, it should be placed at the end of
the file if it is to be added.


**gateway.exproto.server.ssl_options.keyfile**

  *Type*: `string`

  *Default*: `${EMQX_ETC_DIR}/certs/key.pem`

  PEM format private key file.


**gateway.exproto.server.ssl_options.verify**

  *Type*: `enum`

  *Default*: `verify_none`

  *Optional*: `verify_peer | verify_none`

  Enable or disable peer verification.


**gateway.exproto.server.ssl_options.reuse_sessions**

  *Type*: `boolean`

  *Default*: `true`

  Enable TLS session reuse.<br/>
Has no effect when TLS version is configured (or negotiated) to 1.3


**gateway.exproto.server.ssl_options.depth**

  *Type*: `non_neg_integer`

  *Default*: `10`

  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br/>
if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br/>
if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.


**gateway.exproto.server.ssl_options.password**

  *Type*: `string`

  String containing the user's password. Only used if the private key file is password-protected.


**gateway.exproto.server.ssl_options.versions**

  *Type*: `array`

  *Default*: `["tlsv1.3","tlsv1.2"]`

  All TLS/DTLS versions to be supported.<br/>
NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config.<br/>
In case PSK cipher suites are intended, make sure to configure
<code>['tlsv1.2', 'tlsv1.1']</code> here.


**gateway.exproto.server.ssl_options.ciphers**

  *Type*: `array`

  *Default*: `[]`

  This config holds TLS cipher suite names separated by comma,
or as an array of strings. e.g.
<code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> or
<code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>.
<br/>
Ciphers (and their ordering) define the way in which the
client and server encrypts information over the network connection.
Selecting a good cipher suite is critical for the
application's data security, confidentiality and performance.

The names should be in OpenSSL string format (not RFC format).
All default values and examples provided by EMQX config
documentation are all in OpenSSL format.<br/>

NOTE: Certain cipher suites are only compatible with
specific TLS <code>versions</code> ('tlsv1.1', 'tlsv1.2' or 'tlsv1.3')
incompatible cipher suites will be silently dropped.
For instance, if only 'tlsv1.3' is given in the <code>versions</code>,
configuring cipher suites for other versions will have no effect.
<br/>

NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config<br/>
If PSK cipher suites are intended, 'tlsv1.3' should be disabled from <code>versions</code>.<br/>
PSK cipher suites: <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>


**gateway.exproto.server.ssl_options.secure_renegotiate**

  *Type*: `boolean`

  *Default*: `true`

  SSL parameter renegotiation is a feature that allows a client and a server
to renegotiate the parameters of the SSL connection on the fly.
RFC 5746 defines a more secure way of doing this. By enabling secure renegotiation,
you drop support for the insecure renegotiation, prone to MitM attacks.<br/>
Has no effect when TLS version is configured (or negotiated) to 1.3


**gateway.exproto.server.ssl_options.log_level**

  *Type*: `enum`

  *Default*: `notice`

  *Optional*: `emergency | alert | critical | error | warning | notice | info | debug | none | all`

  Log level for SSL communication. Default is 'notice'. Set to 'debug' to inspect TLS handshake messages.


**gateway.exproto.server.ssl_options.hibernate_after**

  *Type*: `duration`

  *Default*: `5s`

  Hibernate the SSL process after idling for amount of time reducing its memory footprint.


**gateway.exproto.server.ssl_options.dhfile**

  *Type*: `string`

  Path to a file containing PEM-encoded Diffie-Hellman parameters
to be used by the server if a cipher suite using Diffie-Hellman
key exchange is negotiated. If not specified, default parameters
are used.<br/>
NOTE: The <code>dhfile</code> option is not supported by TLS 1.3.


**gateway.exproto.server.ssl_options.fail_if_no_peer_cert**

  *Type*: `boolean`

  *Default*: `false`

  Used together with {verify, verify_peer} by an TLS/DTLS server.
If set to true, the server fails if the client does not have a
certificate to send, that is, sends an empty certificate.
If set to false, it fails only if the client sends an invalid
certificate (an empty certificate is considered valid).


**gateway.exproto.server.ssl_options.honor_cipher_order**

  *Type*: `boolean`

  *Default*: `true`

  An important security setting, it forces the cipher to be set based
 on the server-specified order instead of the client-specified order,
 hence enforcing the (usually more properly configured) security
 ordering of the server administrator.


**gateway.exproto.server.ssl_options.client_renegotiation**

  *Type*: `boolean`

  *Default*: `true`

  In protocols that support client-initiated renegotiation,
the cost of resources of such an operation is higher for the server than the client.
This can act as a vector for denial of service attacks.
The SSL application already takes measures to counter-act such attempts,
but client-initiated renegotiation can be strictly disabled by setting this option to false.
The default value is true. Note that disabling renegotiation can result in
long-lived connections becoming unusable due to limits on
the number of messages the underlying cipher suite can encipher.<br/>
Has no effect when TLS version is configured (or negotiated) to 1.3


**gateway.exproto.server.ssl_options.handshake_timeout**

  *Type*: `duration`

  *Default*: `15s`

  Maximum time duration allowed for the handshake to complete



### Gateway Client Mapping


ClientInfo override.

**gateway:clientinfo_override.username**

  *Type*: `string`

  Template for overriding username.


**gateway:clientinfo_override.password**

  *Type*: `string`

  Template for overriding password.


**gateway:clientinfo_override.clientid**

  *Type*: `string`

  Template for overriding clientid.

" 

### Gateway Listeners - TCP


Settings for TCP listener.

**gateway:tcp_listener.acceptors**

  *Type*: `integer`

  *Default*: `16`

  Size of the acceptor pool.


**gateway:tcp_listener.tcp_options**

  *Type*: [broker:tcp_opts](#tcp_opts)

  Setting the TCP socket options.


**gateway:tcp_listener.proxy_protocol**

  *Type*: `boolean`

  *Default*: `false`

  Enable the Proxy Protocol V1/2 if the EMQX cluster is deployed behind HAProxy or Nginx.
See: https://www.haproxy.com/blog/haproxy/proxy-protocol/


**gateway:tcp_listener.proxy_protocol_timeout**

  *Type*: `emqx_gateway_schema:duration`

  *Default*: `15s`

  Timeout for proxy protocol.
EMQX will close the TCP connection if proxy protocol packet is not received within the timeout.


**gateway:tcp_listener.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable the listener.


**gateway:tcp_listener.bind**

  *Type*: `emqx_gateway_schema:ip_port`

  The IP address and port that the listener will bind.


**gateway:tcp_listener.max_connections**

  *Type*: `pos_integer | infinity`

  *Default*: `1024`

  Maximum number of concurrent connections.


**gateway:tcp_listener.max_conn_rate**

  *Type*: `integer`

  *Default*: `1000`

  Maximum connections per second.


**gateway:tcp_listener.enable_authn**

  *Type*: `boolean`

  *Default*: `true`

  Set <code>true</code> (default) to enable client authentication on this listener. 
When set to <code>false</code> clients will be allowed to connect without authentication.


**gateway:tcp_listener.mountpoint**

  *Type*: `string`

  When publishing or subscribing, prefix all topics with a mountpoint string.
The prefixed string will be removed from the topic name when the message is delivered to the subscriber.
The mountpoint is a way that users can use to implement isolation of message routing between different listeners.
For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint` set to `some_tenant`,
then the client actually subscribes to the topic `some_tenant/t`.
Similarly, if another client B (connected to the same listener as the client A) sends a message to topic `t`,
the message is routed to all the clients subscribed `some_tenant/t`,
so client A will receive the message, with topic name `t`. Set to `""` to disable the feature.
Variables in mountpoint string:<br/>
  - <code>${clientid}</code>: clientid<br/>
  - <code>${username}</code>: username


**gateway:tcp_listener.access_rules**

  *Type*: `array`

  *Default*: `[]`

  The access control rules for this listener.
See: https://github.com/emqtt/esockd#allowdeny




Settings for the TCP listeners.

**gateway.stomp.listeners.tcp**

  *Type*: `name`

  A map from listener names to listener settings.


**gateway.stomp.listeners.ssl**

  *Type*: `name`

  A map from listener names to listener settings.




Settings for TCP and UDP listeners.

**gateway.exproto.listeners.tcp**

  *Type*: `name`

  A map from listener names to listener settings.


**gateway.exproto.listeners.ssl**

  *Type*: `name`

  A map from listener names to listener settings.


**gateway.exproto.listeners.udp**

  *Type*: `name`

  A map from listener names to listener settings.


**gateway.exproto.listeners.dtls**

  *Type*: `name`

  A map from listener names to listener settings.



### Gateway Listeners - SSL


Settings for SSL listener.

**gateway:ssl_listener.acceptors**

  *Type*: `integer`

  *Default*: `16`

  Size of the acceptor pool.


**gateway:ssl_listener.tcp_options**

  *Type*: [broker:tcp_opts](#tcp_opts)

  Setting the TCP socket options.


**gateway:ssl_listener.proxy_protocol**

  *Type*: `boolean`

  *Default*: `false`

  Enable the Proxy Protocol V1/2 if the EMQX cluster is deployed behind HAProxy or Nginx.
See: https://www.haproxy.com/blog/haproxy/proxy-protocol/


**gateway:ssl_listener.proxy_protocol_timeout**

  *Type*: `emqx_gateway_schema:duration`

  *Default*: `15s`

  Timeout for proxy protocol.
EMQX will close the TCP connection if proxy protocol packet is not received within the timeout.


**gateway:ssl_listener.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable the listener.


**gateway:ssl_listener.bind**

  *Type*: `emqx_gateway_schema:ip_port`

  The IP address and port that the listener will bind.


**gateway:ssl_listener.max_connections**

  *Type*: `pos_integer | infinity`

  *Default*: `1024`

  Maximum number of concurrent connections.


**gateway:ssl_listener.max_conn_rate**

  *Type*: `integer`

  *Default*: `1000`

  Maximum connections per second.


**gateway:ssl_listener.enable_authn**

  *Type*: `boolean`

  *Default*: `true`

  Set <code>true</code> (default) to enable client authentication on this listener. 
When set to <code>false</code> clients will be allowed to connect without authentication.


**gateway:ssl_listener.mountpoint**

  *Type*: `string`

  When publishing or subscribing, prefix all topics with a mountpoint string.
The prefixed string will be removed from the topic name when the message is delivered to the subscriber.
The mountpoint is a way that users can use to implement isolation of message routing between different listeners.
For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint` set to `some_tenant`,
then the client actually subscribes to the topic `some_tenant/t`.
Similarly, if another client B (connected to the same listener as the client A) sends a message to topic `t`,
the message is routed to all the clients subscribed `some_tenant/t`,
so client A will receive the message, with topic name `t`. Set to `""` to disable the feature.
Variables in mountpoint string:<br/>
  - <code>${clientid}</code>: clientid<br/>
  - <code>${username}</code>: username


**gateway:ssl_listener.access_rules**

  *Type*: `array`

  *Default*: `[]`

  The access control rules for this listener.
See: https://github.com/emqtt/esockd#allowdeny


**gateway:ssl_listener.ssl_options**

  *Type*: [listener_ssl_opts](#ssl-tls-configuration-for-the-listener)

  SSL Socket options.



### Gateway Listeners - UDP


Settings for UDP listener.

**gateway:udp_listener.udp_options**

  *Type*: `gateway:udp_opts`


**gateway:udp_listener.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable the listener.


**gateway:udp_listener.bind**

  *Type*: `emqx_gateway_schema:ip_port`

  The IP address and port that the listener will bind.


**gateway:udp_listener.max_connections**

  *Type*: `pos_integer | infinity`

  *Default*: `1024`

  Maximum number of concurrent connections.


**gateway:udp_listener.max_conn_rate**

  *Type*: `integer`

  *Default*: `1000`

  Maximum connections per second.


**gateway:udp_listener.enable_authn**

  *Type*: `boolean`

  *Default*: `true`

  Set <code>true</code> (default) to enable client authentication on this listener. 
When set to <code>false</code> clients will be allowed to connect without authentication.


**gateway:udp_listener.mountpoint**

  *Type*: `string`

  When publishing or subscribing, prefix all topics with a mountpoint string.
The prefixed string will be removed from the topic name when the message is delivered to the subscriber.
The mountpoint is a way that users can use to implement isolation of message routing between different listeners.
For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint` set to `some_tenant`,
then the client actually subscribes to the topic `some_tenant/t`.
Similarly, if another client B (connected to the same listener as the client A) sends a message to topic `t`,
the message is routed to all the clients subscribed `some_tenant/t`,
so client A will receive the message, with topic name `t`. Set to `""` to disable the feature.
Variables in mountpoint string:<br/>
  - <code>${clientid}</code>: clientid<br/>
  - <code>${username}</code>: username


**gateway:udp_listener.access_rules**

  *Type*: `array`

  *Default*: `[]`

  The access control rules for this listener.
See: https://github.com/emqtt/esockd#allowdeny




Settings for the UDP listeners.

**gateway:udp_listeners.udp**

  *Type*: `name`

  A map from listener names to listener settings.


**gateway:udp_listeners.dtls**

  *Type*: `name`

  A map from listener names to listener settings.




Settings for UDP sockets.

**gateway:udp_opts.active_n**

  *Type*: `integer`

  *Default*: `100`

  Specify the {active, N} option for the socket.
See: https://erlang.org/doc/man/inet.html#setopts-2


**gateway:udp_opts.recbuf**

  *Type*: `emqx_gateway_schema:bytesize`

  Size of the kernel-space receive buffer for the socket.


**gateway:udp_opts.sndbuf**

  *Type*: `emqx_gateway_schema:bytesize`

  Size of the kernel-space send buffer for the socket.


**gateway:udp_opts.buffer**

  *Type*: `emqx_gateway_schema:bytesize`

  Size of the user-space buffer for the socket.


**gateway:udp_opts.reuseaddr**

  *Type*: `boolean`

  *Default*: `true`

  Allow local reuse of port numbers.



### Gateway Listeners - DTLS


Settings for DTLS listener.

**gateway:dtls_listener.acceptors**

  *Type*: `integer`

  *Default*: `16`

  Size of the acceptor pool.


**gateway:dtls_listener.udp_options**

  *Type*: `gateway:udp_opts`


**gateway:dtls_listener.enable**

  *Type*: `boolean`

  *Default*: `true`

  Enable the listener.


**gateway:dtls_listener.bind**

  *Type*: `emqx_gateway_schema:ip_port`

  The IP address and port that the listener will bind.


**gateway:dtls_listener.max_connections**

  *Type*: `pos_integer | infinity`

  *Default*: `1024`

  Maximum number of concurrent connections.


**gateway:dtls_listener.max_conn_rate**

  *Type*: `integer`

  *Default*: `1000`

  Maximum connections per second.


**gateway:dtls_listener.enable_authn**

  *Type*: `boolean`

  *Default*: `true`

  Set <code>true</code> (default) to enable client authentication on this listener. 
When set to <code>false</code> clients will be allowed to connect without authentication.


**gateway:dtls_listener.mountpoint**

  *Type*: `string`

  When publishing or subscribing, prefix all topics with a mountpoint string.
The prefixed string will be removed from the topic name when the message is delivered to the subscriber.
The mountpoint is a way that users can use to implement isolation of message routing between different listeners.
For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint` set to `some_tenant`,
then the client actually subscribes to the topic `some_tenant/t`.
Similarly, if another client B (connected to the same listener as the client A) sends a message to topic `t`,
the message is routed to all the clients subscribed `some_tenant/t`,
so client A will receive the message, with topic name `t`. Set to `""` to disable the feature.
Variables in mountpoint string:<br/>
  - <code>${clientid}</code>: clientid<br/>
  - <code>${username}</code>: username


**gateway:dtls_listener.access_rules**

  *Type*: `array`

  *Default*: `[]`

  The access control rules for this listener.
See: https://github.com/emqtt/esockd#allowdeny


**gateway:dtls_listener.dtls_options**

  *Type*: `gateway:dtls_opts`

  DTLS socket options




Settings for DTLS protocol.

**gateway:dtls_opts.cacertfile**

  *Type*: `string`

  *Default*: `${EMQX_ETC_DIR}/certs/cacert.pem`

  Trusted PEM format CA certificates bundle file.<br/>
The certificates in this file are used to verify the TLS peer's certificates.
Append new certificates to the file if new CAs are to be trusted.
There is no need to restart EMQX to have the updated file loaded, because
the system regularly checks if file has been updated (and reload).<br/>
NOTE: invalidating (deleting) a certificate from the file will not affect
already established connections.


**gateway:dtls_opts.cacerts**

  *Type*: `boolean`

  Deprecated since 5.1.4.


**gateway:dtls_opts.certfile**

  *Type*: `string`

  *Default*: `${EMQX_ETC_DIR}/certs/cert.pem`

  PEM format certificates chain file.<br/>
The certificates in this file should be in reversed order of the certificate
issue chain. That is, the host's certificate should be placed in the beginning
of the file, followed by the immediate issuer certificate and so on.
Although the root CA certificate is optional, it should be placed at the end of
the file if it is to be added.


**gateway:dtls_opts.keyfile**

  *Type*: `string`

  *Default*: `${EMQX_ETC_DIR}/certs/key.pem`

  PEM format private key file.


**gateway:dtls_opts.verify**

  *Type*: `enum`

  *Default*: `verify_none`

  *Optional*: `verify_peer | verify_none`

  Enable or disable peer verification.


**gateway:dtls_opts.reuse_sessions**

  *Type*: `boolean`

  *Default*: `true`

  Enable TLS session reuse.<br/>
Has no effect when TLS version is configured (or negotiated) to 1.3


**gateway:dtls_opts.depth**

  *Type*: `non_neg_integer`

  *Default*: `10`

  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br/>
if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br/>
if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.


**gateway:dtls_opts.password**

  *Type*: `string`

  String containing the user's password. Only used if the private key file is password-protected.


**gateway:dtls_opts.versions**

  *Type*: `array`

  *Default*: `["dtlsv1.2"]`

  All TLS/DTLS versions to be supported.<br/>
NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config.<br/>
In case PSK cipher suites are intended, make sure to configure
<code>['tlsv1.2', 'tlsv1.1']</code> here.


**gateway:dtls_opts.ciphers**

  *Type*: `array`

  *Default*: `[]`

  This config holds TLS cipher suite names separated by comma,
or as an array of strings. e.g.
<code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> or
<code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>.
<br/>
Ciphers (and their ordering) define the way in which the
client and server encrypts information over the network connection.
Selecting a good cipher suite is critical for the
application's data security, confidentiality and performance.

The names should be in OpenSSL string format (not RFC format).
All default values and examples provided by EMQX config
documentation are all in OpenSSL format.<br/>

NOTE: Certain cipher suites are only compatible with
specific TLS <code>versions</code> ('tlsv1.1', 'tlsv1.2' or 'tlsv1.3')
incompatible cipher suites will be silently dropped.
For instance, if only 'tlsv1.3' is given in the <code>versions</code>,
configuring cipher suites for other versions will have no effect.
<br/>

NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config<br/>
If PSK cipher suites are intended, 'tlsv1.3' should be disabled from <code>versions</code>.<br/>
PSK cipher suites: <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>


**gateway:dtls_opts.secure_renegotiate**

  *Type*: `boolean`

  *Default*: `true`

  SSL parameter renegotiation is a feature that allows a client and a server
to renegotiate the parameters of the SSL connection on the fly.
RFC 5746 defines a more secure way of doing this. By enabling secure renegotiation,
you drop support for the insecure renegotiation, prone to MitM attacks.<br/>
Has no effect when TLS version is configured (or negotiated) to 1.3


**gateway:dtls_opts.log_level**

  *Type*: `enum`

  *Default*: `notice`

  *Optional*: `emergency | alert | critical | error | warning | notice | info | debug | none | all`

  Log level for SSL communication. Default is 'notice'. Set to 'debug' to inspect TLS handshake messages.


**gateway:dtls_opts.hibernate_after**

  *Type*: `duration`

  *Default*: `5s`

  Hibernate the SSL process after idling for amount of time reducing its memory footprint.


**gateway:dtls_opts.dhfile**

  *Type*: `string`

  Path to a file containing PEM-encoded Diffie-Hellman parameters
to be used by the server if a cipher suite using Diffie-Hellman
key exchange is negotiated. If not specified, default parameters
are used.<br/>
NOTE: The <code>dhfile</code> option is not supported by TLS 1.3.


**gateway:dtls_opts.fail_if_no_peer_cert**

  *Type*: `boolean`

  *Default*: `false`

  Used together with {verify, verify_peer} by an TLS/DTLS server.
If set to true, the server fails if the client does not have a
certificate to send, that is, sends an empty certificate.
If set to false, it fails only if the client sends an invalid
certificate (an empty certificate is considered valid).


**gateway:dtls_opts.honor_cipher_order**

  *Type*: `boolean`

  *Default*: `true`

  An important security setting, it forces the cipher to be set based
 on the server-specified order instead of the client-specified order,
 hence enforcing the (usually more properly configured) security
 ordering of the server administrator.


**gateway:dtls_opts.client_renegotiation**

  *Type*: `boolean`

  *Default*: `true`

  In protocols that support client-initiated renegotiation,
the cost of resources of such an operation is higher for the server than the client.
This can act as a vector for denial of service attacks.
The SSL application already takes measures to counter-act such attempts,
but client-initiated renegotiation can be strictly disabled by setting this option to false.
The default value is true. Note that disabling renegotiation can result in
long-lived connections becoming unusable due to limits on
the number of messages the underlying cipher suite can encipher.<br/>
Has no effect when TLS version is configured (or negotiated) to 1.3


**gateway:dtls_opts.handshake_timeout**

  *Type*: `duration`

  *Default*: `15s`

  Maximum time duration allowed for the handshake to complete


**gateway:dtls_opts.gc_after_handshake**

  *Type*: `boolean`

  *Default*: `false`

  Memory usage tuning. If enabled, will immediately perform a garbage collection after the TLS/SSL handshake.


**gateway:dtls_opts.ocsp**

  *Type*: `broker:ocsp`


**gateway:dtls_opts.enable_crl_check**

  *Type*: `boolean`

  *Default*: `false`

  Whether to enable CRL verification for this listener.



## Others

### SSL/TLS configuration for clients


Socket options for SSL clients.

**ssl_client_opts.cacertfile**

  *Type*: `string`

  Trusted PEM format CA certificates bundle file.<br/>
The certificates in this file are used to verify the TLS peer's certificates.
Append new certificates to the file if new CAs are to be trusted.
There is no need to restart EMQX to have the updated file loaded, because
the system regularly checks if file has been updated (and reload).<br/>
NOTE: invalidating (deleting) a certificate from the file will not affect
already established connections.


**ssl_client_opts.cacerts**

  *Type*: `boolean`

  Deprecated since 5.1.4.


**ssl_client_opts.certfile**

  *Type*: `string`

  PEM format certificates chain file.<br/>
The certificates in this file should be in reversed order of the certificate
issue chain. That is, the host's certificate should be placed in the beginning
of the file, followed by the immediate issuer certificate and so on.
Although the root CA certificate is optional, it should be placed at the end of
the file if it is to be added.


**ssl_client_opts.keyfile**

  *Type*: `string`

  PEM format private key file.


**ssl_client_opts.verify**

  *Type*: `enum`

  *Default*: `verify_none`

  *Optional*: `verify_peer | verify_none`

  Enable or disable peer verification.


**ssl_client_opts.reuse_sessions**

  *Type*: `boolean`

  *Default*: `true`

  Enable TLS session reuse.<br/>
Has no effect when TLS version is configured (or negotiated) to 1.3


**ssl_client_opts.depth**

  *Type*: `non_neg_integer`

  *Default*: `10`

  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br/>
if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br/>
if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.


**ssl_client_opts.password**

  *Type*: `string`

  String containing the user's password. Only used if the private key file is password-protected.


**ssl_client_opts.versions**

  *Type*: `array`

  *Default*: `["tlsv1.3","tlsv1.2"]`

  All TLS/DTLS versions to be supported.<br/>
NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config.<br/>
In case PSK cipher suites are intended, make sure to configure
<code>['tlsv1.2', 'tlsv1.1']</code> here.


**ssl_client_opts.ciphers**

  *Type*: `array`

  *Default*: `[]`

  This config holds TLS cipher suite names separated by comma,
or as an array of strings. e.g.
<code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> or
<code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>.
<br/>
Ciphers (and their ordering) define the way in which the
client and server encrypts information over the network connection.
Selecting a good cipher suite is critical for the
application's data security, confidentiality and performance.

The names should be in OpenSSL string format (not RFC format).
All default values and examples provided by EMQX config
documentation are all in OpenSSL format.<br/>

NOTE: Certain cipher suites are only compatible with
specific TLS <code>versions</code> ('tlsv1.1', 'tlsv1.2' or 'tlsv1.3')
incompatible cipher suites will be silently dropped.
For instance, if only 'tlsv1.3' is given in the <code>versions</code>,
configuring cipher suites for other versions will have no effect.
<br/>

NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config<br/>
If PSK cipher suites are intended, 'tlsv1.3' should be disabled from <code>versions</code>.<br/>
PSK cipher suites: <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>


**ssl_client_opts.secure_renegotiate**

  *Type*: `boolean`

  *Default*: `true`

  SSL parameter renegotiation is a feature that allows a client and a server
to renegotiate the parameters of the SSL connection on the fly.
RFC 5746 defines a more secure way of doing this. By enabling secure renegotiation,
you drop support for the insecure renegotiation, prone to MitM attacks.<br/>
Has no effect when TLS version is configured (or negotiated) to 1.3


**ssl_client_opts.log_level**

  *Type*: `enum`

  *Default*: `notice`

  *Optional*: `emergency | alert | critical | error | warning | notice | info | debug | none | all`

  Log level for SSL communication. Default is 'notice'. Set to 'debug' to inspect TLS handshake messages.


**ssl_client_opts.hibernate_after**

  *Type*: `duration`

  *Default*: `5s`

  Hibernate the SSL process after idling for amount of time reducing its memory footprint.


**ssl_client_opts.enable**

  *Type*: `boolean`

  *Default*: `false`

  Enable TLS.


**ssl_client_opts.server_name_indication**

  *Type*: `disable | string`

  Specify the host name to be used in TLS Server Name Indication extension.<br/>
For instance, when connecting to "server.example.net", the genuine server
which accepts the connection and performs TLS handshake may differ from the
host the TLS client initially connects to, e.g. when connecting to an IP address
or when the host has multiple resolvable DNS records <br/>
If not specified, it will default to the host name string which is used
to establish the connection, unless it is IP addressed used.<br/>
The host name is then also used in the host name verification of the peer
certificate.<br/> The special value 'disable' prevents the Server Name
Indication extension from being sent and disables the hostname
verification check.



### SSL/TLS configuration for the listener


Socket options for SSL connections.

**listener_ssl_opts.cacertfile**

  *Type*: `string`

  *Default*: `${EMQX_ETC_DIR}/certs/cacert.pem`

  Trusted PEM format CA certificates bundle file.<br/>
The certificates in this file are used to verify the TLS peer's certificates.
Append new certificates to the file if new CAs are to be trusted.
There is no need to restart EMQX to have the updated file loaded, because
the system regularly checks if file has been updated (and reload).<br/>
NOTE: invalidating (deleting) a certificate from the file will not affect
already established connections.


**listener_ssl_opts.cacerts**

  *Type*: `boolean`

  Deprecated since 5.1.4.


**listener_ssl_opts.certfile**

  *Type*: `string`

  *Default*: `${EMQX_ETC_DIR}/certs/cert.pem`

  PEM format certificates chain file.<br/>
The certificates in this file should be in reversed order of the certificate
issue chain. That is, the host's certificate should be placed in the beginning
of the file, followed by the immediate issuer certificate and so on.
Although the root CA certificate is optional, it should be placed at the end of
the file if it is to be added.


**listener_ssl_opts.keyfile**

  *Type*: `string`

  *Default*: `${EMQX_ETC_DIR}/certs/key.pem`

  PEM format private key file.


**listener_ssl_opts.verify**

  *Type*: `enum`

  *Default*: `verify_none`

  *Optional*: `verify_peer | verify_none`

  Enable or disable peer verification.


**listener_ssl_opts.reuse_sessions**

  *Type*: `boolean`

  *Default*: `true`

  Enable TLS session reuse.<br/>
Has no effect when TLS version is configured (or negotiated) to 1.3


**listener_ssl_opts.depth**

  *Type*: `non_neg_integer`

  *Default*: `10`

  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br/>
if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br/>
if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.


**listener_ssl_opts.password**

  *Type*: `string`

  String containing the user's password. Only used if the private key file is password-protected.


**listener_ssl_opts.versions**

  *Type*: `array`

  *Default*: `["tlsv1.3","tlsv1.2"]`

  All TLS/DTLS versions to be supported.<br/>
NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config.<br/>
In case PSK cipher suites are intended, make sure to configure
<code>['tlsv1.2', 'tlsv1.1']</code> here.


**listener_ssl_opts.ciphers**

  *Type*: `array`

  *Default*: `[]`

  This config holds TLS cipher suite names separated by comma,
or as an array of strings. e.g.
<code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> or
<code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>.
<br/>
Ciphers (and their ordering) define the way in which the
client and server encrypts information over the network connection.
Selecting a good cipher suite is critical for the
application's data security, confidentiality and performance.

The names should be in OpenSSL string format (not RFC format).
All default values and examples provided by EMQX config
documentation are all in OpenSSL format.<br/>

NOTE: Certain cipher suites are only compatible with
specific TLS <code>versions</code> ('tlsv1.1', 'tlsv1.2' or 'tlsv1.3')
incompatible cipher suites will be silently dropped.
For instance, if only 'tlsv1.3' is given in the <code>versions</code>,
configuring cipher suites for other versions will have no effect.
<br/>

NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config<br/>
If PSK cipher suites are intended, 'tlsv1.3' should be disabled from <code>versions</code>.<br/>
PSK cipher suites: <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>


**listener_ssl_opts.secure_renegotiate**

  *Type*: `boolean`

  *Default*: `true`

  SSL parameter renegotiation is a feature that allows a client and a server
to renegotiate the parameters of the SSL connection on the fly.
RFC 5746 defines a more secure way of doing this. By enabling secure renegotiation,
you drop support for the insecure renegotiation, prone to MitM attacks.<br/>
Has no effect when TLS version is configured (or negotiated) to 1.3


**listener_ssl_opts.log_level**

  *Type*: `enum`

  *Default*: `notice`

  *Optional*: `emergency | alert | critical | error | warning | notice | info | debug | none | all`

  Log level for SSL communication. Default is 'notice'. Set to 'debug' to inspect TLS handshake messages.


**listener_ssl_opts.hibernate_after**

  *Type*: `duration`

  *Default*: `5s`

  Hibernate the SSL process after idling for amount of time reducing its memory footprint.


**listener_ssl_opts.dhfile**

  *Type*: `string`

  Path to a file containing PEM-encoded Diffie-Hellman parameters
to be used by the server if a cipher suite using Diffie-Hellman
key exchange is negotiated. If not specified, default parameters
are used.<br/>
NOTE: The <code>dhfile</code> option is not supported by TLS 1.3.


**listener_ssl_opts.fail_if_no_peer_cert**

  *Type*: `boolean`

  *Default*: `false`

  Used together with {verify, verify_peer} by an TLS/DTLS server.
If set to true, the server fails if the client does not have a
certificate to send, that is, sends an empty certificate.
If set to false, it fails only if the client sends an invalid
certificate (an empty certificate is considered valid).


**listener_ssl_opts.honor_cipher_order**

  *Type*: `boolean`

  *Default*: `true`

  An important security setting, it forces the cipher to be set based
 on the server-specified order instead of the client-specified order,
 hence enforcing the (usually more properly configured) security
 ordering of the server administrator.


**listener_ssl_opts.client_renegotiation**

  *Type*: `boolean`

  *Default*: `true`

  In protocols that support client-initiated renegotiation,
the cost of resources of such an operation is higher for the server than the client.
This can act as a vector for denial of service attacks.
The SSL application already takes measures to counter-act such attempts,
but client-initiated renegotiation can be strictly disabled by setting this option to false.
The default value is true. Note that disabling renegotiation can result in
long-lived connections becoming unusable due to limits on
the number of messages the underlying cipher suite can encipher.<br/>
Has no effect when TLS version is configured (or negotiated) to 1.3


**listener_ssl_opts.handshake_timeout**

  *Type*: `duration`

  *Default*: `15s`

  Maximum time duration allowed for the handshake to complete


**listener_ssl_opts.gc_after_handshake**

  *Type*: `boolean`

  *Default*: `false`

  Memory usage tuning. If enabled, will immediately perform a garbage collection after the TLS/SSL handshake.


**listener_ssl_opts.ocsp**

  *Type*: `broker:ocsp`


**listener_ssl_opts.enable_crl_check**

  *Type*: `boolean`

  *Default*: `false`

  Whether to enable CRL verification for this listener.



### tcp_opts


TCP listener options.

**tcp_opts.active_n**

  *Type*: `integer`

  *Default*: `100`

  Specify the {active, N} option for this Socket.<br/>
See: https://erlang.org/doc/man/inet.html#setopts-2


**tcp_opts.backlog**

  *Type*: `pos_integer`

  *Default*: `1024`

  TCP backlog defines the maximum length that the queue of
pending connections can grow to.


**tcp_opts.send_timeout**

  *Type*: `duration`

  *Default*: `15s`

  The TCP send timeout for the connections.


**tcp_opts.send_timeout_close**

  *Type*: `boolean`

  *Default*: `true`

  Close the connection if send timeout.


**tcp_opts.recbuf**

  *Type*: `bytesize`

  The TCP receive buffer (OS kernel) for the connections.


**tcp_opts.sndbuf**

  *Type*: `bytesize`

  The TCP send buffer (OS kernel) for the connections.


**tcp_opts.buffer**

  *Type*: `bytesize`

  *Default*: `4KB`

  The size of the user-space buffer used by the driver.


**tcp_opts.high_watermark**

  *Type*: `bytesize`

  *Default*: `1MB`

  The socket is set to a busy state when the amount of data queued internally
by the VM socket implementation reaches this limit.


**tcp_opts.nodelay**

  *Type*: `boolean`

  *Default*: `true`

  The TCP_NODELAY flag for the connections.


**tcp_opts.reuseaddr**

  *Type*: `boolean`

  *Default*: `true`

  The SO_REUSEADDR flag for the connections.


**tcp_opts.keepalive**

  *Type*: `string`

  *Default*: `none`

  Enable TCP keepalive for MQTT connections over TCP or SSL.
The value is three comma separated numbers in the format of 'Idle,Interval,Probes'
 - Idle: The number of seconds a connection needs to be idle before the server begins to send out keep-alive probes (Linux default 7200).
 - Interval: The number of seconds between TCP keep-alive probes (Linux default 75).
 - Probes: The maximum number of TCP keep-alive probes to send before giving up and killing the connection if no response is obtained from the other end (Linux default 9).
For example "240,30,5" means: EMQX should start sending TCP keepalive probes after the connection is in idle for 240 seconds, and the probes are sent every 30 seconds until a response is received from the MQTT client, if it misses 5 consecutive responses, EMQX should close the connection.
Default: 'none'



### ws_opts


WebSocket listener options.

**ws_opts.mqtt_path**

  *Type*: `string`

  *Default*: `/mqtt`

  WebSocket's MQTT protocol path. So the address of EMQX Broker's WebSocket is:
<code>ws://{ip}:{port}/mqtt</code>


**ws_opts.mqtt_piggyback**

  *Type*: `enum`

  *Default*: `multiple`

  *Optional*: `single | multiple`

  Whether a WebSocket message is allowed to contain multiple MQTT packets.


**ws_opts.compress**

  *Type*: `boolean`

  *Default*: `false`

  If <code>true</code>, compress WebSocket messages using <code>zlib</code>.<br/>
The configuration items under <code>deflate_opts</code> belong to the compression-related parameter configuration.


**ws_opts.idle_timeout**

  *Type*: `duration`

  *Default*: `7200s`

  Close transport-layer connections from the clients that have not sent MQTT CONNECT message within this interval.


**ws_opts.max_frame_size**

  *Type*: `infinity | integer`

  *Default*: `infinity`

  The maximum length of a single MQTT packet.


**ws_opts.fail_if_no_subprotocol**

  *Type*: `boolean`

  *Default*: `true`

  If <code>true</code>, the server will return an error when
 the client does not carry the <code>Sec-WebSocket-Protocol</code> field.
 <br/>Note: WeChat applet needs to disable this verification.


**ws_opts.supported_subprotocols**

  *Type*: `comma_separated_list`

  *Default*: `mqtt, mqtt-v3, mqtt-v3.1.1, mqtt-v5`

  Comma-separated list of supported subprotocols.


**ws_opts.check_origin_enable**

  *Type*: `boolean`

  *Default*: `false`

  If <code>true</code>, <code>origin</code> HTTP header will be
 validated against the list of allowed origins configured in <code>check_origins</code>
 parameter.


**ws_opts.allow_origin_absence**

  *Type*: `boolean`

  *Default*: `true`

  If <code>false</code> and <code>check_origin_enable</code> is
 <code>true</code>, the server will reject requests that don't have <code>origin</code>
 HTTP header.


**ws_opts.check_origins**

  *Type*: `comma_separated_binary`

  *Default*: `http://localhost:18083, http://127.0.0.1:18083`

  List of allowed origins.<br/>See <code>check_origin_enable</code>.


**ws_opts.proxy_address_header**

  *Type*: `string`

  *Default*: `x-forwarded-for`

  HTTP header used to pass information about the client IP address.
Relevant when the EMQX cluster is deployed behind a load-balancer.


**ws_opts.proxy_port_header**

  *Type*: `string`

  *Default*: `x-forwarded-port`

  HTTP header used to pass information about the client port. Relevant when the EMQX cluster is deployed behind a load-balancer.


**ws_opts.deflate_opts**

  *Type*: [broker:deflate_opts](#deflate_opts)



### listener_wss_opts


Socket options for WebSocket/SSL connections.

**listeners.wss.$name.ssl_options.cacertfile**

  *Type*: `string`

  *Default*: `${EMQX_ETC_DIR}/certs/cacert.pem`

  Trusted PEM format CA certificates bundle file.<br/>
The certificates in this file are used to verify the TLS peer's certificates.
Append new certificates to the file if new CAs are to be trusted.
There is no need to restart EMQX to have the updated file loaded, because
the system regularly checks if file has been updated (and reload).<br/>
NOTE: invalidating (deleting) a certificate from the file will not affect
already established connections.


**listeners.wss.$name.ssl_options.cacerts**

  *Type*: `boolean`

  Deprecated since 5.1.4.


**listeners.wss.$name.ssl_options.certfile**

  *Type*: `string`

  *Default*: `${EMQX_ETC_DIR}/certs/cert.pem`

  PEM format certificates chain file.<br/>
The certificates in this file should be in reversed order of the certificate
issue chain. That is, the host's certificate should be placed in the beginning
of the file, followed by the immediate issuer certificate and so on.
Although the root CA certificate is optional, it should be placed at the end of
the file if it is to be added.


**listeners.wss.$name.ssl_options.keyfile**

  *Type*: `string`

  *Default*: `${EMQX_ETC_DIR}/certs/key.pem`

  PEM format private key file.


**listeners.wss.$name.ssl_options.verify**

  *Type*: `enum`

  *Default*: `verify_none`

  *Optional*: `verify_peer | verify_none`

  Enable or disable peer verification.


**listeners.wss.$name.ssl_options.reuse_sessions**

  *Type*: `boolean`

  *Default*: `true`

  Enable TLS session reuse.<br/>
Has no effect when TLS version is configured (or negotiated) to 1.3


**listeners.wss.$name.ssl_options.depth**

  *Type*: `non_neg_integer`

  *Default*: `10`

  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br/>
if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br/>
if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.


**listeners.wss.$name.ssl_options.password**

  *Type*: `string`

  String containing the user's password. Only used if the private key file is password-protected.


**listeners.wss.$name.ssl_options.versions**

  *Type*: `array`

  *Default*: `["tlsv1.3","tlsv1.2"]`

  All TLS/DTLS versions to be supported.<br/>
NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config.<br/>
In case PSK cipher suites are intended, make sure to configure
<code>['tlsv1.2', 'tlsv1.1']</code> here.


**listeners.wss.$name.ssl_options.ciphers**

  *Type*: `array`

  *Default*: `[]`

  This config holds TLS cipher suite names separated by comma,
or as an array of strings. e.g.
<code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> or
<code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>.
<br/>
Ciphers (and their ordering) define the way in which the
client and server encrypts information over the network connection.
Selecting a good cipher suite is critical for the
application's data security, confidentiality and performance.

The names should be in OpenSSL string format (not RFC format).
All default values and examples provided by EMQX config
documentation are all in OpenSSL format.<br/>

NOTE: Certain cipher suites are only compatible with
specific TLS <code>versions</code> ('tlsv1.1', 'tlsv1.2' or 'tlsv1.3')
incompatible cipher suites will be silently dropped.
For instance, if only 'tlsv1.3' is given in the <code>versions</code>,
configuring cipher suites for other versions will have no effect.
<br/>

NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config<br/>
If PSK cipher suites are intended, 'tlsv1.3' should be disabled from <code>versions</code>.<br/>
PSK cipher suites: <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>


**listeners.wss.$name.ssl_options.secure_renegotiate**

  *Type*: `boolean`

  *Default*: `true`

  SSL parameter renegotiation is a feature that allows a client and a server
to renegotiate the parameters of the SSL connection on the fly.
RFC 5746 defines a more secure way of doing this. By enabling secure renegotiation,
you drop support for the insecure renegotiation, prone to MitM attacks.<br/>
Has no effect when TLS version is configured (or negotiated) to 1.3


**listeners.wss.$name.ssl_options.log_level**

  *Type*: `enum`

  *Default*: `notice`

  *Optional*: `emergency | alert | critical | error | warning | notice | info | debug | none | all`

  Log level for SSL communication. Default is 'notice'. Set to 'debug' to inspect TLS handshake messages.


**listeners.wss.$name.ssl_options.hibernate_after**

  *Type*: `duration`

  *Default*: `5s`

  Hibernate the SSL process after idling for amount of time reducing its memory footprint.


**listeners.wss.$name.ssl_options.dhfile**

  *Type*: `string`

  Path to a file containing PEM-encoded Diffie-Hellman parameters
to be used by the server if a cipher suite using Diffie-Hellman
key exchange is negotiated. If not specified, default parameters
are used.<br/>
NOTE: The <code>dhfile</code> option is not supported by TLS 1.3.


**listeners.wss.$name.ssl_options.fail_if_no_peer_cert**

  *Type*: `boolean`

  *Default*: `false`

  Used together with {verify, verify_peer} by an TLS/DTLS server.
If set to true, the server fails if the client does not have a
certificate to send, that is, sends an empty certificate.
If set to false, it fails only if the client sends an invalid
certificate (an empty certificate is considered valid).


**listeners.wss.$name.ssl_options.honor_cipher_order**

  *Type*: `boolean`

  *Default*: `true`

  An important security setting, it forces the cipher to be set based
 on the server-specified order instead of the client-specified order,
 hence enforcing the (usually more properly configured) security
 ordering of the server administrator.


**listeners.wss.$name.ssl_options.client_renegotiation**

  *Type*: `boolean`

  *Default*: `true`

  In protocols that support client-initiated renegotiation,
the cost of resources of such an operation is higher for the server than the client.
This can act as a vector for denial of service attacks.
The SSL application already takes measures to counter-act such attempts,
but client-initiated renegotiation can be strictly disabled by setting this option to false.
The default value is true. Note that disabling renegotiation can result in
long-lived connections becoming unusable due to limits on
the number of messages the underlying cipher suite can encipher.<br/>
Has no effect when TLS version is configured (or negotiated) to 1.3


**listeners.wss.$name.ssl_options.handshake_timeout**

  *Type*: `duration`

  *Default*: `15s`

  Maximum time duration allowed for the handshake to complete



### deflate_opts


Compression options.

**deflate_opts.level**

  *Type*: `enum`

  *Optional*: `none | default | best_compression | best_speed`

  Compression level.


**deflate_opts.mem_level**

  *Type*: `integer`

  *Default*: `8`

  *Optional*: `1-9`

  Specifies the size of the compression state.<br/>
Lower values decrease memory usage per connection.


**deflate_opts.strategy**

  *Type*: `enum`

  *Default*: `default`

  *Optional*: `default | filtered | huffman_only | rle`

  Specifies the compression strategy.


**deflate_opts.server_context_takeover**

  *Type*: `enum`

  *Default*: `takeover`

  *Optional*: `takeover | no_takeover`

  Takeover means the compression state is retained between server messages.


**deflate_opts.client_context_takeover**

  *Type*: `enum`

  *Default*: `takeover`

  *Optional*: `takeover | no_takeover`

  Takeover means the compression state is retained between client messages.


**deflate_opts.server_max_window_bits**

  *Type*: `integer`

  *Default*: `15`

  *Optional*: `8-15`

  Specifies the size of the compression context for the server.


**deflate_opts.client_max_window_bits**

  *Type*: `integer`

  *Default*: `15`

  *Optional*: `8-15`

  Specifies the size of the compression context for the client.


