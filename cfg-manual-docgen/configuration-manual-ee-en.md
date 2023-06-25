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


**node.process_limit**

  *Type*: `integer`

  *Default*: `2097152`

  *Optional*: `1024-134217727`

  Maximum number of simultaneously existing processes for this Erlang system.
The actual maximum chosen may be much larger than the Number passed.
For more information, see: https://www.erlang.org/doc/man/erl.html


**node.max_ports**

  *Type*: `integer`

  *Default*: `1048576`

  *Optional*: `1024-134217727`

  Maximum number of simultaneously existing ports for this Erlang system.
The actual maximum chosen may be much larger than the Number passed.
For more information, see: https://www.erlang.org/doc/man/erl.html


**node.dist_buffer_size**

  *Type*: `integer`

  *Default*: `8192`

  *Optional*: `1-2097151`

  Erlang's distribution buffer busy limit in kilobytes.


**node.data_dir**

  *Type*: `string`

  Path to the persistent data directory.<br />
Possible auto-created subdirectories are:<br />
- `mnesia/<node_name>`: EMQX's built-in database directory.<br />
For example, `mnesia/emqx@127.0.0.1`.<br />
There should be only one such subdirectory.<br />
Meaning, in case the node is to be renamed (to e.g. `emqx@10.0.1.1`),<br />
the old dir should be deleted first.<br />
- `configs`: Generated configs at boot time, and cluster/local override configs.<br />
- `patches`: Hot-patch beam files are to be placed here.<br />
- `trace`: Trace log files.<br />

**NOTE**: One data dir cannot be shared by two or more EMQX nodes.


**node.global_gc_interval**

  *Type*: `disabled | duration`

  *Default*: `15m`

  Periodic garbage collection interval. Set to <code>disabled</code> to have it disabled.


**node.role**

  *Type*: `enum`

  *Default*: `core`

  *Optional*: `core | replicant`

  Select a node role.<br />
<code>core</code> nodes provide durability of the data, and take care of writes.
It is recommended to place core nodes in different racks or different availability zones.<br />
<code>replicant</code> nodes are ephemeral worker nodes. Removing them from the cluster
doesn't affect database redundancy<br />
It is recommended to have more replicant nodes than core nodes.<br />
Note: this parameter only takes effect when the <code>backend</code> is set
to <code>rlog</code>.



## RPC


EMQX uses a library called <code>gen_rpc</code> for inter-broker communication.<br />
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

  <code>manual</code>: discover ports by <code>tcp_server_port</code>.<br />
<code>stateless</code>: discover ports in a stateless manner, using the following algorithm.
If node name is <code>emqxN@127.0.0.1</code>, where the N is an integer,
then the listening port will be 5370 + N.


**rpc.tcp_server_port**

  *Type*: `integer`

  *Default*: `5369`

  Listening port used by RPC local service.<br />
Note that this config only takes effect when rpc.port_discovery is set to manual.


**rpc.ssl_server_port**

  *Type*: `integer`

  *Default*: `5369`

  Listening port used by RPC local service.<br />
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

  Path to the private key file for the <code>rpc.certfile</code>.<br />
Note: contents of this file are secret, so it's necessary to set permissions to 600.


**rpc.cacertfile**

  *Type*: `file`

  Path to certification authority TLS certificate file used to validate <code>rpc.certfile</code>.<br />
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


EMQX nodes can form a cluster to scale up the total capacity.<br />
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
- manual: Use <code>emqx ctl cluster</code> command to manage cluster.<br />
- static: Configure static nodes list by setting <code>seeds</code> in config file.<br />
- dns: Use DNS A record to discover peer nodes.<br />
- etcd: Use etcd to discover peer nodes.<br />
- k8s: Use Kubernetes API to discover peer pods.
- mcast: Deprecated since 5.1, will be removed in 5.2.
  This supports discovery via UDP multicast.


**cluster.core_nodes**

  *Type*: `comma_separated_atoms | array`

  *Default*: `[]`

  List of core nodes that the replicant will connect to.<br />
Note: this parameter only takes effect when the <code>backend</code> is set
to <code>rlog</code> and the <code>role</code> is set to <code>replicant</code>.<br />
This value needs to be defined for manual or static cluster discovery mechanisms.<br />
If an automatic cluster discovery mechanism is being used (such as <code>etcd</code>),
there is no need to set this value.


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

  The Erlang distribution protocol for the cluster.<br />
- inet_tcp: IPv4 TCP <br />
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

This is the default configuration of clustering, nodes join a cluster by executing ./bin/emqx_ctl join \<Node\> CLI command:

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

  *Default*: `http://10.110.111.204:8080`

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

  Node name suffix.<br />
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

  This parameter controls log file rotation. The value `infinity` means the log file will grow indefinitely, otherwise the log file will be rotated once it reaches `max_size` in bytes.


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

## MQTT/TCP Listener - 1883

EMQX supports the creation of multiple listeners, and the default MQTT/TCP listener port is `1883`.

**listeners.tcp.$name.enabled**

  *Type*: `boolean`

  *Default*: `true`

  Enable listener.


**listeners.tcp.$name.bind**

  *Type*: `ip_port | integer`

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
message, with topic name `t`.<br />
Set to `""` to disable the feature.<br />

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

  Maximum connection rate.<br />
This is used to limit the connection rate for this listener,
once the limit is reached, new connections will be deferred or refused


**listeners.tcp.$name.messages_rate**

  *Type*: `rate`

  Messages publish rate.<br />
This is used to limit the inbound message numbers for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.


**listeners.tcp.$name.bytes_rate**

  *Type*: `rate`

  Data publish rate.<br />
This is used to limit the inbound bytes rate for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.


**listeners.tcp.$name.access_rules**

  *Type*: `array`

  *Default*: `["allow all"]`

  The access control rules for this listener.<br />See: https://github.com/emqtt/esockd#allowdeny


**listeners.tcp.$name.proxy_protocol**

  *Type*: `boolean`

  *Default*: `false`

  Enable the Proxy Protocol V1/2 if the EMQX cluster is deployed behind HAProxy or Nginx.<br />
See: https://www.haproxy.com/blog/haproxy/proxy-protocol/


**listeners.tcp.$name.proxy_protocol_timeout**

  *Type*: `duration`

  *Default*: `3s`

  Timeout for proxy protocol. EMQX will close the TCP connection if proxy protocol packet is not received within the timeout.


**listeners.tcp.$name.tcp_options**

  *Type*: [broker:tcp_opts](#tcp_opts)



## MQTT/SSL Listener - 8883


Settings for the MQTT over SSL listener.

**listeners.ssl.$name.enabled**

  *Type*: `boolean`

  *Default*: `true`

  Enable listener.


**listeners.ssl.$name.bind**

  *Type*: `ip_port | integer`

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
message, with topic name `t`.<br />
Set to `""` to disable the feature.<br />

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

  Maximum connection rate.<br />
This is used to limit the connection rate for this listener,
once the limit is reached, new connections will be deferred or refused


**listeners.ssl.$name.messages_rate**

  *Type*: `rate`

  Messages publish rate.<br />
This is used to limit the inbound message numbers for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.


**listeners.ssl.$name.bytes_rate**

  *Type*: `rate`

  Data publish rate.<br />
This is used to limit the inbound bytes rate for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.


**listeners.ssl.$name.access_rules**

  *Type*: `array`

  *Default*: `["allow all"]`

  The access control rules for this listener.<br />See: https://github.com/emqtt/esockd#allowdeny


**listeners.ssl.$name.proxy_protocol**

  *Type*: `boolean`

  *Default*: `false`

  Enable the Proxy Protocol V1/2 if the EMQX cluster is deployed behind HAProxy or Nginx.<br />
See: https://www.haproxy.com/blog/haproxy/proxy-protocol/


**listeners.ssl.$name.proxy_protocol_timeout**

  *Type*: `duration`

  *Default*: `3s`

  Timeout for proxy protocol. EMQX will close the TCP connection if proxy protocol packet is not received within the timeout.


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
<br />
Ciphers (and their ordering) define the way in which the
client and server encrypts information over the network connection.
Selecting a good cipher suite is critical for the
application's data security, confidentiality and performance.

The names should be in OpenSSL string format (not RFC format).
All default values and examples provided by EMQX config
documentation are all in OpenSSL format.<br />

NOTE: Certain cipher suites are only compatible with
specific TLS <code>versions</code> ('tlsv1.1', 'tlsv1.2' or 'tlsv1.3')
incompatible cipher suites will be silently dropped.
For instance, if only 'tlsv1.3' is given in the <code>versions</code>,
configuring cipher suites for other versions will have no effect.
<br />

NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config<br />
If PSK cipher suites are intended, 'tlsv1.3' should be disabled from <code>versions</code>.<br />
PSK cipher suites: <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code><br />

NOTE: QUIC listener supports only 'tlsv1.3' ciphers


**listeners.quic.$name.ssl_options**

  *Type*: `broker:listener_quic_ssl_opts`

  TLS options for QUIC transport


**listeners.quic.$name.enabled**

  *Type*: `boolean`

  *Default*: `true`

  Enable listener.


**listeners.quic.$name.bind**

  *Type*: `ip_port | integer`

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
message, with topic name `t`.<br />
Set to `""` to disable the feature.<br />

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

  Maximum connection rate.<br />
This is used to limit the connection rate for this listener,
once the limit is reached, new connections will be deferred or refused


**listeners.quic.$name.messages_rate**

  *Type*: `rate`

  Messages publish rate.<br />
This is used to limit the inbound message numbers for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.


**listeners.quic.$name.bytes_rate**

  *Type*: `rate`

  Data publish rate.<br />
This is used to limit the inbound bytes rate for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.



## MQTT/WebSocket Listener - 8083


Settings for the MQTT over WebSocket listener.

**listeners.ws.$name.enabled**

  *Type*: `boolean`

  *Default*: `true`

  Enable listener.


**listeners.ws.$name.bind**

  *Type*: `ip_port | integer`

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
message, with topic name `t`.<br />
Set to `""` to disable the feature.<br />

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

  Maximum connection rate.<br />
This is used to limit the connection rate for this listener,
once the limit is reached, new connections will be deferred or refused


**listeners.ws.$name.messages_rate**

  *Type*: `rate`

  Messages publish rate.<br />
This is used to limit the inbound message numbers for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.


**listeners.ws.$name.bytes_rate**

  *Type*: `rate`

  Data publish rate.<br />
This is used to limit the inbound bytes rate for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.


**listeners.ws.$name.access_rules**

  *Type*: `array`

  *Default*: `["allow all"]`

  The access control rules for this listener.<br />See: https://github.com/emqtt/esockd#allowdeny


**listeners.ws.$name.proxy_protocol**

  *Type*: `boolean`

  *Default*: `false`

  Enable the Proxy Protocol V1/2 if the EMQX cluster is deployed behind HAProxy or Nginx.<br />
See: https://www.haproxy.com/blog/haproxy/proxy-protocol/


**listeners.ws.$name.proxy_protocol_timeout**

  *Type*: `duration`

  *Default*: `3s`

  Timeout for proxy protocol. EMQX will close the TCP connection if proxy protocol packet is not received within the timeout.


**listeners.ws.$name.tcp_options**

  *Type*: [broker:tcp_opts](#tcp_opts)


**listeners.ws.$name.websocket**

  *Type*: [broker:ws_opts](#ws_opts)



## MQTT/WebSocket with SSL Listener - 8084


Settings for the MQTT over WebSocket/SSL listener.

**listeners.wss.$name.enabled**

  *Type*: `boolean`

  *Default*: `true`

  Enable listener.


**listeners.wss.$name.bind**

  *Type*: `ip_port | integer`

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
message, with topic name `t`.<br />
Set to `""` to disable the feature.<br />

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

  Maximum connection rate.<br />
This is used to limit the connection rate for this listener,
once the limit is reached, new connections will be deferred or refused


**listeners.wss.$name.messages_rate**

  *Type*: `rate`

  Messages publish rate.<br />
This is used to limit the inbound message numbers for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.


**listeners.wss.$name.bytes_rate**

  *Type*: `rate`

  Data publish rate.<br />
This is used to limit the inbound bytes rate for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.


**listeners.wss.$name.access_rules**

  *Type*: `array`

  *Default*: `["allow all"]`

  The access control rules for this listener.<br />See: https://github.com/emqtt/esockd#allowdeny


**listeners.wss.$name.proxy_protocol**

  *Type*: `boolean`

  *Default*: `false`

  Enable the Proxy Protocol V1/2 if the EMQX cluster is deployed behind HAProxy or Nginx.<br />
See: https://www.haproxy.com/blog/haproxy/proxy-protocol/


**listeners.wss.$name.proxy_protocol_timeout**

  *Type*: `duration`

  *Default*: `3s`

  Timeout for proxy protocol. EMQX will close the TCP connection if proxy protocol packet is not received within the timeout.


**listeners.wss.$name.tcp_options**

  *Type*: [broker:tcp_opts](#tcp_opts)


**listeners.wss.$name.ssl_options**

  *Type*: [broker:listener_wss_opts](#listener_wss_opts)


**listeners.wss.$name.websocket**

  *Type*: [broker:ws_opts](#ws_opts)



## MQTT Basic Parameters

Global MQTT configuration parameters.


Global MQTT configuration.<br />The configs here work as default values which can be overridden
in <code>zone</code> configs

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

  Maximum MQTT packet size allowed.


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

  Message retention time. 0 means message will never be expired.


**retainer.msg_clear_interval**

  *Type*: `timeout_duration_ms`

  *Default*: `0s`

  Periodic interval for cleaning up expired messages.
Never clear if the value is 0.


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


**retainer.deliver_rate**

  *Type*: `rate`

  The maximum rate of delivering retain messages


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

  Retainer index specifications: list of arrays of positive ascending integers. Each array specifies an index. Numbers in an index specification are 1-based word positions in topics. Words from specified positions will be used for indexing.<br />For example, it is good to have <code>[2, 4]</code> index to optimize <code>+/X/+/Y/...</code> topic wildcard subscriptions.



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

  Time interval of publishing `$SYS` messages.


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

## Integration With Prometheus


Settings for reporting metrics to Prometheus

**prometheus.push_gateway_server**

  *Type*: `string`

  *Default*: `http://127.0.0.1:9091`

  URL of Prometheus server


**prometheus.interval**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  Data reporting interval


**prometheus.headers**

  *Type*: `[{string, string()}]`

  *Default*: `[]`

  An HTTP Headers when pushing to Push Gateway.<br />
For example, <code> { Authorization = "some-authz-tokens"}</code>


**prometheus.job_name**

  *Type*: `string`

  *Default*: `${name}/instance/${name}~${host}`

  Job Name that is pushed to the Push Gateway. Available variables:<br />
- ${name}: Name of EMQX node.<br />
- ${host}: Host name of EMQX node.<br />
For example, when the EMQX node name is <code>emqx@127.0.0.1</code> then the <code>name</code> variable takes value <code>emqx</code> and the <code>host</code> variable takes value <code>127.0.0.1</code>.<br />
Default value is: <code>${name}/instance/${name}~${host}</code>


**prometheus.enable**

  *Type*: `boolean`

  *Default*: `false`

  Turn Prometheus data pushing on or off



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

  The actions triggered when the alarm is activated.<br />Currently, the following actions are supported: <code>log</code> and <code>publish</code>.
<code>log</code> is to write the alarm to log (console or file).
<code>publish</code> is to publish the alarm as an MQTT message to the system topics:
<code>$SYS/brokers/emqx@xx.xx.xx.x/alarms/activate</code> and
<code>$SYS/brokers/emqx@xx.xx.xx.x/alarms/deactivate</code>


**alarm.size_limit**

  *Type*: `integer`

  *Default*: `1000`

  *Optional*: `1-3000`

  The maximum total number of deactivated alarms to keep as history.<br />When this limit is exceeded, the oldest deactivated alarms are deleted to cap the total number.


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

  The time interval for the periodic CPU check.


**sysmon.os.cpu_high_watermark**

  *Type*: `percent`

  *Default*: `80%`

  The threshold, as percentage of system CPU load,
 for how much system cpu can be used before the corresponding alarm is raised.


**sysmon.os.cpu_low_watermark**

  *Type*: `percent`

  *Default*: `60%`

  The threshold, as percentage of system CPU load,
 for how much system cpu can be used before the corresponding alarm is cleared.


**sysmon.os.mem_check_interval**

  *Type*: `disabled | duration`

  *Default*: `60s`

  The time interval for the periodic memory check.


**sysmon.os.sysmem_high_watermark**

  *Type*: `percent`

  *Default*: `70%`

  The threshold, as percentage of system memory,
 for how much system memory can be allocated before the corresponding alarm is raised.


**sysmon.os.procmem_high_watermark**

  *Type*: `percent`

  *Default*: `5%`

  The threshold, as percentage of system memory,
 for how much system memory can be allocated by one Erlang process before
 the corresponding alarm is raised.



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


**dashboard.bootstrap_users_file**

  *Type*: `string`

  Deprecated since 5.1.0.




Configuration for the dashboard listener (plaintext).

**dashboard.listeners.http.bind**

  *Type*: `non_neg_integer | ip_port`

  *Default*: `0`

  Port without IP(18083) or port with specified IP(127.0.0.1:18083).


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

  *Type*: `non_neg_integer | ip_port`

  *Default*: `0`

  Port without IP(18083) or port with specified IP(127.0.0.1:18083).


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


**dashboard.listeners.https.cacertfile**

  *Type*: `string`

  *Default*: `${EMQX_ETC_DIR}/certs/cacert.pem`

  Trusted PEM format CA certificates bundle file.<br />
The certificates in this file are used to verify the TLS peer's certificates.
Append new certificates to the file if new CAs are to be trusted.
There is no need to restart EMQX to have the updated file loaded, because
the system regularly checks if file has been updated (and reload).<br />
NOTE: invalidating (deleting) a certificate from the file will not affect
already established connections.


**dashboard.listeners.https.certfile**

  *Type*: `string`

  *Default*: `${EMQX_ETC_DIR}/certs/cert.pem`

  PEM format certificates chain file.<br />
The certificates in this file should be in reversed order of the certificate
issue chain. That is, the host's certificate should be placed in the beginning
of the file, followed by the immediate issuer certificate and so on.
Although the root CA certificate is optional, it should be placed at the end of
the file if it is to be added.


**dashboard.listeners.https.keyfile**

  *Type*: `string`

  *Default*: `${EMQX_ETC_DIR}/certs/key.pem`

  PEM format private key file.


**dashboard.listeners.https.verify**

  *Type*: `enum`

  *Default*: `verify_none`

  *Optional*: `verify_peer | verify_none`

  Enable or disable peer verification.


**dashboard.listeners.https.reuse_sessions**

  *Type*: `boolean`

  *Default*: `true`

  Enable TLS session reuse.<br />
Has no effect when TLS version is configured (or negotiated) to 1.3


**dashboard.listeners.https.depth**

  *Type*: `non_neg_integer`

  *Default*: `10`

  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br />
if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br />
if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.


**dashboard.listeners.https.password**

  *Type*: `string`

  String containing the user's password. Only used if the private key file is password-protected.


**dashboard.listeners.https.versions**

  *Type*: `array`

  *Default*: `["tlsv1.3","tlsv1.2"]`

  All TLS/DTLS versions to be supported.<br />
NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config.<br />
In case PSK cipher suites are intended, make sure to configure
<code>['tlsv1.2', 'tlsv1.1']</code> here.


**dashboard.listeners.https.ciphers**

  *Type*: `array`

  *Default*: `[]`

  This config holds TLS cipher suite names separated by comma,
or as an array of strings. e.g.
<code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> or
<code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>.
<br />
Ciphers (and their ordering) define the way in which the
client and server encrypts information over the network connection.
Selecting a good cipher suite is critical for the
application's data security, confidentiality and performance.

The names should be in OpenSSL string format (not RFC format).
All default values and examples provided by EMQX config
documentation are all in OpenSSL format.<br />

NOTE: Certain cipher suites are only compatible with
specific TLS <code>versions</code> ('tlsv1.1', 'tlsv1.2' or 'tlsv1.3')
incompatible cipher suites will be silently dropped.
For instance, if only 'tlsv1.3' is given in the <code>versions</code>,
configuring cipher suites for other versions will have no effect.
<br />

NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config<br />
If PSK cipher suites are intended, 'tlsv1.3' should be disabled from <code>versions</code>.<br />
PSK cipher suites: <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>


**dashboard.listeners.https.secure_renegotiate**

  *Type*: `boolean`

  *Default*: `true`

  SSL parameter renegotiation is a feature that allows a client and a server
to renegotiate the parameters of the SSL connection on the fly.
RFC 5746 defines a more secure way of doing this. By enabling secure renegotiation,
you drop support for the insecure renegotiation, prone to MitM attacks.<br />
Has no effect when TLS version is configured (or negotiated) to 1.3


**dashboard.listeners.https.log_level**

  *Type*: `enum`

  *Default*: `notice`

  *Optional*: `emergency | alert | critical | error | warning | notice | info | debug | none | all`

  Log level for SSL communication. Default is 'notice'. Set to 'debug' to inspect TLS handshake messages.


**dashboard.listeners.https.hibernate_after**

  *Type*: `duration`

  *Default*: `5s`

  Hibernate the SSL process after idling for amount of time reducing its memory footprint.


**dashboard.listeners.https.dhfile**

  *Type*: `string`

  Path to a file containing PEM-encoded Diffie-Hellman parameters
to be used by the server if a cipher suite using Diffie-Hellman
key exchange is negotiated. If not specified, default parameters
are used.<br />
NOTE: The <code>dhfile</code> option is not supported by TLS 1.3.


**dashboard.listeners.https.honor_cipher_order**

  *Type*: `boolean`

  *Default*: `true`

  An important security setting, it forces the cipher to be set based
 on the server-specified order instead of the client-specified order,
 hence enforcing the (usually more properly configured) security
 ordering of the server administrator.


**dashboard.listeners.https.client_renegotiation**

  *Type*: `boolean`

  *Default*: `true`

  In protocols that support client-initiated renegotiation,
the cost of resources of such an operation is higher for the server than the client.
This can act as a vector for denial of service attacks.
The SSL application already takes measures to counter-act such attempts,
but client-initiated renegotiation can be strictly disabled by setting this option to false.
The default value is true. Note that disabling renegotiation can result in
long-lived connections becoming unusable due to limits on
the number of messages the underlying cipher suite can encipher.<br />
Has no effect when TLS version is configured (or negotiated) to 1.3


**dashboard.listeners.https.handshake_timeout**

  *Type*: `duration`

  *Default*: `15s`

  Maximum time duration allowed for the handshake to complete




Configuration for the dashboard listener.

**dashboard.listeners.http**

  *Type*: `dashboard:http`

  TCP listeners


**dashboard.listeners.https**

  *Type*: `dashboard:https`

  SSL listeners



## API Keys


API Key, can be used to request API other than the management API key and the Dashboard user management API

**api_key.bootstrap_file**

  *Type*: `string`

  *Default*: `""`

  Bootstrap file is used to add an api_key when emqx is launched,
      the format is:
       ```
       7e729ae70d23144b:2QILI9AcQ9BYlVqLDHQNWN2saIjBV4egr1CZneTNKr9CpK
       ec3907f865805db0:Ee3taYltUKtoBVD9C3XjQl9C6NXheip8Z9B69BpUv5JxVHL
       ```



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

  MQTT Keepalive. Time interval is a string that contains a number followed by time unit:<br />- `ms` for milliseconds,
- `s` for seconds,
- `m` for minutes,
- `h` for hours;
<br />or combination of whereof: `1h5m0s`


**bridges.mqtt.$name.retry_interval**

  *Type*: `string`

  *Default*: `15s`

  Message retry interval. Delay for the MQTT bridge to retry sending the QoS1/QoS2 messages in case of ACK not received. Time interval is a string that contains a number followed by time unit:<br />- `ms` for milliseconds,
- `s` for seconds,
- `m` for minutes,
- `h` for hours;
<br />or combination of whereof: `1h5m0s`


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
        send them to the local broker.<br />
        Template with variables is allowed in 'remote.qos', 'local.topic', 'local.qos', 'local.retain', 'local.payload'.<br />
        NOTE: if this bridge is used as the input of a rule, and also 'local.topic' is
        configured, then messages got from the remote broker will be sent to both the 'local.topic' and
        the rule.


**bridges.mqtt.$name.egress**

  *Type*: `connector-mqtt:egress`

  The egress config defines how this bridge forwards messages from the local broker to the remote broker.<br />
Template with variables is allowed in 'remote.topic', 'local.qos', 'local.retain', 'local.payload'.<br />
NOTE: if this bridge is used as the action of a rule, and also 'local.topic'
is configured, then both the data got from the rule and the MQTT messages that matches
'local.topic' will be forwarded.




Creation options.

**bridges.mqtt.$name.resource_opts.worker_pool_size**

  *Type*: `non_neg_integer`

  *Default*: `16`

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

  *Type*: `emqx_connector_http:pool_type`

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

  The URL of the HTTP Bridge.<br />
Template with variables is allowed in the path, but variables cannot be used in the scheme, host,
or port part.<br />
For example, <code> http://localhost:9901/${topic} </code> is allowed, but
<code> http://${host}:9901/message </code> or <code> http://localhost:${port}/message </code>
is not allowed.


**bridges.webhook.$name.direction**

  *Type*: `egress`

  Deprecated since 5.0.12.


**bridges.webhook.$name.local_topic**

  *Type*: `string`

  The MQTT topic filter to be forwarded to the HTTP server. All MQTT 'PUBLISH' messages with the topic
matching the local_topic will be forwarded.<br />
NOTE: if this bridge is used as the action of a rule (EMQX rule engine), and also local_topic is
configured, then both the data got from the rule and the MQTT messages that match local_topic
will be forwarded.


**bridges.webhook.$name.method**

  *Type*: `enum`

  *Default*: `post`

  *Optional*: `post | put | get | delete`

  The method of the HTTP request. All the available methods are: post, put, get, delete.<br />
Template with variables is allowed.


**bridges.webhook.$name.headers**

  *Type*: `map`

  *Default*: `{"keep-alive":"timeout=5","content-type":"application/json","connection":"keep-alive","cache-control":"no-cache","accept":"application/json"}`

  The headers of the HTTP request.<br />
Template with variables is allowed.


**bridges.webhook.$name.body**

  *Type*: `string`

  The body of the HTTP request.<br />
If not provided, the body will be a JSON object of all the available fields.<br />
There, 'all the available fields' means the context of a MQTT message when
this webhook is triggered by receiving a MQTT message (the `local_topic` is set),
or the context of the event when this webhook is triggered by a rule (i.e. this
webhook is used as an action of a rule).<br />
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

  *Type*: `non_neg_integer`

  *Default*: `16`

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



### Data Bridge Connector




**connector-http:request.method**

  *Type*: `string`

  HTTP method.


**connector-http:request.path**

  *Type*: `string`

  URL path.


**connector-http:request.body**

  *Type*: `string`

  HTTP request body.


**connector-http:request.headers**

  *Type*: `map`

  List of HTTP headers.


**connector-http:request.max_retries**

  *Type*: `non_neg_integer`

  Max retry times if error on sending request.


**connector-http:request.request_timeout**

  *Type*: `timeout_duration_ms`

  HTTP request timeout.




The egress config defines how this bridge forwards messages from the local broker to the remote broker.<br />
Template with variables is allowed in 'remote.topic', 'local.qos', 'local.retain', 'local.payload'.<br />
NOTE: if this bridge is used as the action of a rule, and also 'local.topic'
is configured, then both the data got from the rule and the MQTT messages that matches
'local.topic' will be forwarded.

**bridges.mqtt.$name.egress.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the pool of MQTT clients that will publish messages to the remote broker.<br />
Each MQTT client will be assigned 'clientid' of the form '${clientid_prefix}:${bridge_name}:egress:${node}:${n}'
where 'n' is the number of a client inside the pool.


**bridges.mqtt.$name.egress.local**

  *Type*: `connector-mqtt:egress_local`

  The configs about receiving messages from local broker.


**bridges.mqtt.$name.egress.remote**

  *Type*: `connector-mqtt:egress_remote`

  The configs about sending message to the remote broker.




The configs about receiving messages from local broker.

**bridges.mqtt.$name.egress.local.topic**

  *Type*: `string`

  The local topic to be forwarded to the remote broker




The configs about sending message to the remote broker.

**bridges.mqtt.$name.egress.remote.topic**

  *Type*: `string`

  Forward to which topic of the remote broker.<br />
Template with variables is allowed.


**bridges.mqtt.$name.egress.remote.qos**

  *Type*: `qos | string`

  *Default*: `1`

  The QoS of the MQTT message to be sent.<br />
Template with variables is allowed.


**bridges.mqtt.$name.egress.remote.retain**

  *Type*: `boolean | string`

  *Default*: `false`

  The 'retain' flag of the MQTT message to be sent.<br />
Template with variables is allowed.


**bridges.mqtt.$name.egress.remote.payload**

  *Type*: `string`

  The payload of the MQTT message to be sent.<br />
Template with variables is allowed.




The ingress config defines how this bridge receive messages from the remote MQTT broker, and then
        send them to the local broker.<br />
        Template with variables is allowed in 'remote.qos', 'local.topic', 'local.qos', 'local.retain', 'local.payload'.<br />
        NOTE: if this bridge is used as the input of a rule, and also 'local.topic' is
        configured, then messages got from the remote broker will be sent to both the 'local.topic' and
        the rule.

**bridges.mqtt.$name.ingress.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the pool of MQTT clients that will ingest messages from the remote broker.<br />
This value will be respected only if 'remote.topic' is a shared subscription topic or topic-filter
(for example `$share/name1/topic1` or `$share/name2/topic2/#`), otherwise only a single MQTT client will be used.
Each MQTT client will be assigned 'clientid' of the form '${clientid_prefix}:${bridge_name}:ingress:${node}:${n}'
where 'n' is the number of a client inside the pool.
NOTE: Non-shared subscription will not work well when EMQX is clustered.


**bridges.mqtt.$name.ingress.remote**

  *Type*: `connector-mqtt:ingress_remote`

  The configs about subscribing to the remote broker.


**bridges.mqtt.$name.ingress.local**

  *Type*: `connector-mqtt:ingress_local`

  The configs about sending message to the local broker.




The configs about sending message to the local broker.

**bridges.mqtt.$name.ingress.local.topic**

  *Type*: `string`

  Send messages to which topic of the local broker.<br />
Template with variables is allowed.


**bridges.mqtt.$name.ingress.local.qos**

  *Type*: `qos | string`

  *Default*: `${qos}`

  The QoS of the MQTT message to be sent.<br />
Template with variables is allowed.


**bridges.mqtt.$name.ingress.local.retain**

  *Type*: `boolean | string`

  *Default*: `${retain}`

  The 'retain' flag of the MQTT message to be sent.<br />
Template with variables is allowed.


**bridges.mqtt.$name.ingress.local.payload**

  *Type*: `string`

  The payload of the MQTT message to be sent.<br />
Template with variables is allowed.




The configs about subscribing to the remote broker.

**bridges.mqtt.$name.ingress.remote.topic**

  *Type*: `string`

  Receive messages from which topic of the remote broker


**bridges.mqtt.$name.ingress.remote.qos**

  *Type*: `qos`

  *Default*: `1`

  The QoS level to be used when subscribing to the remote broker



## Plugin


Manage EMQX plugins.<br />
Plugins can be pre-built as a part of EMQX package,
or installed as a standalone package in a location specified by
<code>install_dir</code> config key<br />
The standalone-installed plugins are referred to as 'external' plugins.

**plugins.states**

  *Type*: `array`

  *Default*: `[]`

  An array of plugins in the desired states.<br />
The plugins are started in the defined order


**plugins.install_dir**

  *Type*: `string`

  *Default*: `plugins`

  The installation directory for the external plugins.
The plugin beam files and configuration files should reside in
the subdirectory named as <code>emqx_foo_bar-0.1.0</code>.
<br />
NOTE: For security reasons, this directory should **NOT** be writable
by anyone except <code>emqx</code> (or any user which runs EMQX).


**plugins.check_interval**

  *Type*: `duration`

  Deprecated since 5.0.24.




A per-plugin config to describe the desired state of the plugin.

**plugins.states.$INDEX.name_vsn**

  *Type*: `string`

  The {name}-{version} of the plugin.<br />
It should match the plugin application name-version as the for the plugin release package name<br />
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

  Trusted PEM format CA certificates bundle file.<br />
The certificates in this file are used to verify the TLS peer's certificates.
Append new certificates to the file if new CAs are to be trusted.
There is no need to restart EMQX to have the updated file loaded, because
the system regularly checks if file has been updated (and reload).<br />
NOTE: invalidating (deleting) a certificate from the file will not affect
already established connections.


**exhook.servers.$INDEX.ssl.certfile**

  *Type*: `string`

  PEM format certificates chain file.<br />
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

  Enable TLS session reuse.<br />
Has no effect when TLS version is configured (or negotiated) to 1.3


**exhook.servers.$INDEX.ssl.depth**

  *Type*: `non_neg_integer`

  *Default*: `10`

  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br />
if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br />
if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.


**exhook.servers.$INDEX.ssl.password**

  *Type*: `string`

  String containing the user's password. Only used if the private key file is password-protected.


**exhook.servers.$INDEX.ssl.versions**

  *Type*: `array`

  *Default*: `["tlsv1.3","tlsv1.2"]`

  All TLS/DTLS versions to be supported.<br />
NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config.<br />
In case PSK cipher suites are intended, make sure to configure
<code>['tlsv1.2', 'tlsv1.1']</code> here.


**exhook.servers.$INDEX.ssl.ciphers**

  *Type*: `array`

  *Default*: `[]`

  This config holds TLS cipher suite names separated by comma,
or as an array of strings. e.g.
<code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> or
<code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>.
<br />
Ciphers (and their ordering) define the way in which the
client and server encrypts information over the network connection.
Selecting a good cipher suite is critical for the
application's data security, confidentiality and performance.

The names should be in OpenSSL string format (not RFC format).
All default values and examples provided by EMQX config
documentation are all in OpenSSL format.<br />

NOTE: Certain cipher suites are only compatible with
specific TLS <code>versions</code> ('tlsv1.1', 'tlsv1.2' or 'tlsv1.3')
incompatible cipher suites will be silently dropped.
For instance, if only 'tlsv1.3' is given in the <code>versions</code>,
configuring cipher suites for other versions will have no effect.
<br />

NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config<br />
If PSK cipher suites are intended, 'tlsv1.3' should be disabled from <code>versions</code>.<br />
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
you drop support for the insecure renegotiation, prone to MitM attacks.<br />
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

  Specify the host name to be used in TLS Server Name Indication extension.<br />
For instance, when connecting to "server.example.net", the genuine server
which accepts the connection and performs TLS handshake may differ from the
host the TLS client initially connects to, e.g. when connecting to an IP address
or when the host has multiple resolvable DNS records <br />
If not specified, it will default to the host name string which is used
to establish the connection, unless it is IP addressed used.<br />
The host name is then also used in the host name verification of the peer
certificate.<br /> The special value 'disable' prevents the Server Name
Indication extension from being sent and disables the hostname
verification check.



## Others

### SSL/TLS configuration for clients


Socket options for SSL clients.

**ssl_client_opts.cacertfile**

  *Type*: `string`

  Trusted PEM format CA certificates bundle file.<br />
The certificates in this file are used to verify the TLS peer's certificates.
Append new certificates to the file if new CAs are to be trusted.
There is no need to restart EMQX to have the updated file loaded, because
the system regularly checks if file has been updated (and reload).<br />
NOTE: invalidating (deleting) a certificate from the file will not affect
already established connections.


**ssl_client_opts.certfile**

  *Type*: `string`

  PEM format certificates chain file.<br />
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

  Enable TLS session reuse.<br />
Has no effect when TLS version is configured (or negotiated) to 1.3


**ssl_client_opts.depth**

  *Type*: `non_neg_integer`

  *Default*: `10`

  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br />
if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br />
if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.


**ssl_client_opts.password**

  *Type*: `string`

  String containing the user's password. Only used if the private key file is password-protected.


**ssl_client_opts.versions**

  *Type*: `array`

  *Default*: `["tlsv1.3","tlsv1.2"]`

  All TLS/DTLS versions to be supported.<br />
NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config.<br />
In case PSK cipher suites are intended, make sure to configure
<code>['tlsv1.2', 'tlsv1.1']</code> here.


**ssl_client_opts.ciphers**

  *Type*: `array`

  *Default*: `[]`

  This config holds TLS cipher suite names separated by comma,
or as an array of strings. e.g.
<code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> or
<code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>.
<br />
Ciphers (and their ordering) define the way in which the
client and server encrypts information over the network connection.
Selecting a good cipher suite is critical for the
application's data security, confidentiality and performance.

The names should be in OpenSSL string format (not RFC format).
All default values and examples provided by EMQX config
documentation are all in OpenSSL format.<br />

NOTE: Certain cipher suites are only compatible with
specific TLS <code>versions</code> ('tlsv1.1', 'tlsv1.2' or 'tlsv1.3')
incompatible cipher suites will be silently dropped.
For instance, if only 'tlsv1.3' is given in the <code>versions</code>,
configuring cipher suites for other versions will have no effect.
<br />

NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config<br />
If PSK cipher suites are intended, 'tlsv1.3' should be disabled from <code>versions</code>.<br />
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
you drop support for the insecure renegotiation, prone to MitM attacks.<br />
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

  Specify the host name to be used in TLS Server Name Indication extension.<br />
For instance, when connecting to "server.example.net", the genuine server
which accepts the connection and performs TLS handshake may differ from the
host the TLS client initially connects to, e.g. when connecting to an IP address
or when the host has multiple resolvable DNS records <br />
If not specified, it will default to the host name string which is used
to establish the connection, unless it is IP addressed used.<br />
The host name is then also used in the host name verification of the peer
certificate.<br /> The special value 'disable' prevents the Server Name
Indication extension from being sent and disables the hostname
verification check.



### SSL/TLS configuration for the listener


Socket options for SSL connections.

**listener_ssl_opts.cacertfile**

  *Type*: `string`

  *Default*: `${EMQX_ETC_DIR}/certs/cacert.pem`

  Trusted PEM format CA certificates bundle file.<br />
The certificates in this file are used to verify the TLS peer's certificates.
Append new certificates to the file if new CAs are to be trusted.
There is no need to restart EMQX to have the updated file loaded, because
the system regularly checks if file has been updated (and reload).<br />
NOTE: invalidating (deleting) a certificate from the file will not affect
already established connections.


**listener_ssl_opts.certfile**

  *Type*: `string`

  *Default*: `${EMQX_ETC_DIR}/certs/cert.pem`

  PEM format certificates chain file.<br />
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

  Enable TLS session reuse.<br />
Has no effect when TLS version is configured (or negotiated) to 1.3


**listener_ssl_opts.depth**

  *Type*: `non_neg_integer`

  *Default*: `10`

  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br />
if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br />
if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.


**listener_ssl_opts.password**

  *Type*: `string`

  String containing the user's password. Only used if the private key file is password-protected.


**listener_ssl_opts.versions**

  *Type*: `array`

  *Default*: `["tlsv1.3","tlsv1.2"]`

  All TLS/DTLS versions to be supported.<br />
NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config.<br />
In case PSK cipher suites are intended, make sure to configure
<code>['tlsv1.2', 'tlsv1.1']</code> here.


**listener_ssl_opts.ciphers**

  *Type*: `array`

  *Default*: `[]`

  This config holds TLS cipher suite names separated by comma,
or as an array of strings. e.g.
<code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> or
<code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>.
<br />
Ciphers (and their ordering) define the way in which the
client and server encrypts information over the network connection.
Selecting a good cipher suite is critical for the
application's data security, confidentiality and performance.

The names should be in OpenSSL string format (not RFC format).
All default values and examples provided by EMQX config
documentation are all in OpenSSL format.<br />

NOTE: Certain cipher suites are only compatible with
specific TLS <code>versions</code> ('tlsv1.1', 'tlsv1.2' or 'tlsv1.3')
incompatible cipher suites will be silently dropped.
For instance, if only 'tlsv1.3' is given in the <code>versions</code>,
configuring cipher suites for other versions will have no effect.
<br />

NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config<br />
If PSK cipher suites are intended, 'tlsv1.3' should be disabled from <code>versions</code>.<br />
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
you drop support for the insecure renegotiation, prone to MitM attacks.<br />
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
are used.<br />
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
the number of messages the underlying cipher suite can encipher.<br />
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

  Specify the {active, N} option for this Socket.<br />
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

  If <code>true</code>, compress WebSocket messages using <code>zlib</code>.<br />
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
 <br />Note: WeChat applet needs to disable this verification.


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

  List of allowed origins.<br />See <code>check_origin_enable</code>.


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

  Trusted PEM format CA certificates bundle file.<br />
The certificates in this file are used to verify the TLS peer's certificates.
Append new certificates to the file if new CAs are to be trusted.
There is no need to restart EMQX to have the updated file loaded, because
the system regularly checks if file has been updated (and reload).<br />
NOTE: invalidating (deleting) a certificate from the file will not affect
already established connections.


**listeners.wss.$name.ssl_options.certfile**

  *Type*: `string`

  *Default*: `${EMQX_ETC_DIR}/certs/cert.pem`

  PEM format certificates chain file.<br />
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

  Enable TLS session reuse.<br />
Has no effect when TLS version is configured (or negotiated) to 1.3


**listeners.wss.$name.ssl_options.depth**

  *Type*: `non_neg_integer`

  *Default*: `10`

  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br />
if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br />
if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.


**listeners.wss.$name.ssl_options.password**

  *Type*: `string`

  String containing the user's password. Only used if the private key file is password-protected.


**listeners.wss.$name.ssl_options.versions**

  *Type*: `array`

  *Default*: `["tlsv1.3","tlsv1.2"]`

  All TLS/DTLS versions to be supported.<br />
NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config.<br />
In case PSK cipher suites are intended, make sure to configure
<code>['tlsv1.2', 'tlsv1.1']</code> here.


**listeners.wss.$name.ssl_options.ciphers**

  *Type*: `array`

  *Default*: `[]`

  This config holds TLS cipher suite names separated by comma,
or as an array of strings. e.g.
<code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> or
<code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>.
<br />
Ciphers (and their ordering) define the way in which the
client and server encrypts information over the network connection.
Selecting a good cipher suite is critical for the
application's data security, confidentiality and performance.

The names should be in OpenSSL string format (not RFC format).
All default values and examples provided by EMQX config
documentation are all in OpenSSL format.<br />

NOTE: Certain cipher suites are only compatible with
specific TLS <code>versions</code> ('tlsv1.1', 'tlsv1.2' or 'tlsv1.3')
incompatible cipher suites will be silently dropped.
For instance, if only 'tlsv1.3' is given in the <code>versions</code>,
configuring cipher suites for other versions will have no effect.
<br />

NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config<br />
If PSK cipher suites are intended, 'tlsv1.3' should be disabled from <code>versions</code>.<br />
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
you drop support for the insecure renegotiation, prone to MitM attacks.<br />
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
are used.<br />
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
the number of messages the underlying cipher suite can encipher.<br />
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

  Specifies the size of the compression state.<br />
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

