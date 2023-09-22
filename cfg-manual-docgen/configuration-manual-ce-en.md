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









### Export files to local storage





### Export files to S3 storage







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

  *Default*: `enabled`

  *Optional*: `enabled | disabled`

  Enable or disable Mnesia metrics collector


**prometheus.vm_statistics_collector**

  *Type*: `enum`

  *Default*: `enabled`

  *Optional*: `enabled | disabled`

  Enable or disable VM statistics collector.


**prometheus.vm_system_info_collector**

  *Type*: `enum`

  *Default*: `enabled`

  *Optional*: `enabled | disabled`

  Enable or disable VM system info collector.


**prometheus.vm_memory_collector**

  *Type*: `enum`

  *Default*: `enabled`

  *Optional*: `enabled | disabled`

  Enable or disable VM memory metrics collector.


**prometheus.vm_msacc_collector**

  *Type*: `enum`

  *Default*: `enabled`

  *Optional*: `enabled | disabled`

  Enable or disable VM microstate accounting metrics collector.



## Integration With OpenTelemetry

@opentelemetry

@opentelemetry:exporter


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

**authentication.$INDEX.mechanism**

  *Type*: `password_based`

  Authentication mechanism.


**authentication.$INDEX.backend**

  *Type*: `built_in_database`

  Backend type.


**authentication.$INDEX.user_id_type**

  *Type*: `enum`

  *Default*: `username`

  *Optional*: `clientid | username`

  Specify whether to use `clientid` or `username` for authentication.


**authentication.$INDEX.password_hash_algorithm**

  *Type*: [authn-hash:bcrypt_rw](#authn-hash:bcrypt_rw) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *Default*: `{"salt_position":"prefix","name":"sha256"}`

  Options for password hash creation and verification.


**authentication.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.



### MySQL


Configuration of authenticator using MySQL as authentication data source.

**authentication.$INDEX.mechanism**

  *Type*: `password_based`

  Authentication mechanism.


**authentication.$INDEX.backend**

  *Type*: `mysql`

  Backend type.


**authentication.$INDEX.password_hash_algorithm**

  *Type*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *Default*: `{"salt_position":"prefix","name":"sha256"}`

  Options for password hash verification.


**authentication.$INDEX.query**

  *Type*: `string`

  SQL used to query data for authentication, such as password hash.


**authentication.$INDEX.query_timeout**

  *Type*: `duration_ms`

  *Default*: `5s`

  Timeout for the SQL query.


**authentication.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.


**authentication.$INDEX.server**

  *Type*: `string`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The MySQL default port 3306 is used if `[:Port]` is not specified.


**authentication.$INDEX.database**

  *Type*: `string`

  Database name.


**authentication.$INDEX.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authentication.$INDEX.username**

  *Type*: `string`

  *Default*: `root`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**authentication.$INDEX.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**authentication.$INDEX.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.


**authentication.$INDEX.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



### MongoDB

#### MongoDB Single Node


Configuration of authenticator using MongoDB (Standalone) as authentication data source.

**authentication.$INDEX.mechanism**

  *Type*: `password_based`

  Authentication mechanism.


**authentication.$INDEX.backend**

  *Type*: `mongodb`

  Backend type.


**authentication.$INDEX.collection**

  *Type*: `string`

  Collection used to store authentication data.


**authentication.$INDEX.filter**

  *Type*: `map`

  *Default*: `{}`

  Conditional expression that defines the filter condition in the query.
Filter supports the following placeholders:
- <code>${username}</code>: Will be replaced at runtime with <code>Username</code> used by the client when connecting
- <code>${clientid}</code>: Will be replaced at runtime with <code>Client ID</code> used by the client when connecting


**authentication.$INDEX.password_hash_field**

  *Type*: `string`

  *Default*: `password_hash`

  Document field that contains password hash.


**authentication.$INDEX.salt_field**

  *Type*: `string`

  *Default*: `salt`

  Document field that contains the password salt.


**authentication.$INDEX.is_superuser_field**

  *Type*: `string`

  *Default*: `is_superuser`

  Document field that defines if the user has superuser privileges.


**authentication.$INDEX.password_hash_algorithm**

  *Type*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *Default*: `{"salt_position":"prefix","name":"sha256"}`

  Options for password hash verification.


**authentication.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.


**authentication.$INDEX.mongo_type**

  *Type*: `single`

  *Default*: `single`

  Standalone instance. Must be set to 'single' when MongoDB server is running in standalone mode.


**authentication.$INDEX.server**

  *Type*: `string`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The MongoDB default port 27017 is used if `[:Port]` is not specified.


**authentication.$INDEX.w_mode**

  *Type*: `enum`

  *Default*: `unsafe`

  *Optional*: `unsafe | safe`

  Write mode.


**authentication.$INDEX.srv_record**

  *Type*: `boolean`

  *Default*: `false`

  Use DNS SRV record.


**authentication.$INDEX.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authentication.$INDEX.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**authentication.$INDEX.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**authentication.$INDEX.use_legacy_protocol**

  *Type*: `enum`

  *Default*: `auto`

  *Optional*: `auto | true | false`

  Whether to use MongoDB's legacy protocol for communicating with the database.  The default is to attempt to automatically determine if the newer protocol is supported.


**authentication.$INDEX.auth_source**

  *Type*: `string`

  Database name associated with the user's credentials.


**authentication.$INDEX.database**

  *Type*: `string`

  Database name.


**authentication.$INDEX.topology**

  *Type*: `topology`


**authentication.$INDEX.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



#### MongoDB Replica Set


Configuration of authenticator using MongoDB (Replica Set) as authentication data source.

**authentication.$INDEX.mechanism**

  *Type*: `password_based`

  Authentication mechanism.


**authentication.$INDEX.backend**

  *Type*: `mongodb`

  Backend type.


**authentication.$INDEX.collection**

  *Type*: `string`

  Collection used to store authentication data.


**authentication.$INDEX.filter**

  *Type*: `map`

  *Default*: `{}`

  Conditional expression that defines the filter condition in the query.
Filter supports the following placeholders:
- <code>${username}</code>: Will be replaced at runtime with <code>Username</code> used by the client when connecting
- <code>${clientid}</code>: Will be replaced at runtime with <code>Client ID</code> used by the client when connecting


**authentication.$INDEX.password_hash_field**

  *Type*: `string`

  *Default*: `password_hash`

  Document field that contains password hash.


**authentication.$INDEX.salt_field**

  *Type*: `string`

  *Default*: `salt`

  Document field that contains the password salt.


**authentication.$INDEX.is_superuser_field**

  *Type*: `string`

  *Default*: `is_superuser`

  Document field that defines if the user has superuser privileges.


**authentication.$INDEX.password_hash_algorithm**

  *Type*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *Default*: `{"salt_position":"prefix","name":"sha256"}`

  Options for password hash verification.


**authentication.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.


**authentication.$INDEX.mongo_type**

  *Type*: `rs`

  *Default*: `rs`

  Replica set. Must be set to 'rs' when MongoDB server is running in 'replica set' mode.


**authentication.$INDEX.servers**

  *Type*: `string`

  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
A host entry has the following form: `Host[:Port]`.
The MongoDB default port 27017 is used if `[:Port]` is not specified.


**authentication.$INDEX.w_mode**

  *Type*: `enum`

  *Default*: `unsafe`

  *Optional*: `unsafe | safe`

  Write mode.


**authentication.$INDEX.r_mode**

  *Type*: `enum`

  *Default*: `master`

  *Optional*: `master | slave_ok`

  Read mode.


**authentication.$INDEX.replica_set_name**

  *Type*: `string`

  Name of the replica set.


**authentication.$INDEX.srv_record**

  *Type*: `boolean`

  *Default*: `false`

  Use DNS SRV record.


**authentication.$INDEX.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authentication.$INDEX.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**authentication.$INDEX.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**authentication.$INDEX.use_legacy_protocol**

  *Type*: `enum`

  *Default*: `auto`

  *Optional*: `auto | true | false`

  Whether to use MongoDB's legacy protocol for communicating with the database.  The default is to attempt to automatically determine if the newer protocol is supported.


**authentication.$INDEX.auth_source**

  *Type*: `string`

  Database name associated with the user's credentials.


**authentication.$INDEX.database**

  *Type*: `string`

  Database name.


**authentication.$INDEX.topology**

  *Type*: `topology`


**authentication.$INDEX.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.

" 

#### MongoDB Sharded Cluster


Configuration of authenticator using MongoDB (Sharded Cluster) as authentication data source.

**authentication.$INDEX.mechanism**

  *Type*: `password_based`

  Authentication mechanism.


**authentication.$INDEX.backend**

  *Type*: `mongodb`

  Backend type.


**authentication.$INDEX.collection**

  *Type*: `string`

  Collection used to store authentication data.


**authentication.$INDEX.filter**

  *Type*: `map`

  *Default*: `{}`

  Conditional expression that defines the filter condition in the query.
Filter supports the following placeholders:
- <code>${username}</code>: Will be replaced at runtime with <code>Username</code> used by the client when connecting
- <code>${clientid}</code>: Will be replaced at runtime with <code>Client ID</code> used by the client when connecting


**authentication.$INDEX.password_hash_field**

  *Type*: `string`

  *Default*: `password_hash`

  Document field that contains password hash.


**authentication.$INDEX.salt_field**

  *Type*: `string`

  *Default*: `salt`

  Document field that contains the password salt.


**authentication.$INDEX.is_superuser_field**

  *Type*: `string`

  *Default*: `is_superuser`

  Document field that defines if the user has superuser privileges.


**authentication.$INDEX.password_hash_algorithm**

  *Type*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *Default*: `{"salt_position":"prefix","name":"sha256"}`

  Options for password hash verification.


**authentication.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.


**authentication.$INDEX.mongo_type**

  *Type*: `sharded`

  *Default*: `sharded`

  Sharded cluster. Must be set to 'sharded' when MongoDB server is running in 'sharded' mode.


**authentication.$INDEX.servers**

  *Type*: `string`

  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
A host entry has the following form: `Host[:Port]`.
The MongoDB default port 27017 is used if `[:Port]` is not specified.


**authentication.$INDEX.w_mode**

  *Type*: `enum`

  *Default*: `unsafe`

  *Optional*: `unsafe | safe`

  Write mode.


**authentication.$INDEX.srv_record**

  *Type*: `boolean`

  *Default*: `false`

  Use DNS SRV record.


**authentication.$INDEX.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authentication.$INDEX.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**authentication.$INDEX.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**authentication.$INDEX.use_legacy_protocol**

  *Type*: `enum`

  *Default*: `auto`

  *Optional*: `auto | true | false`

  Whether to use MongoDB's legacy protocol for communicating with the database.  The default is to attempt to automatically determine if the newer protocol is supported.


**authentication.$INDEX.auth_source**

  *Type*: `string`

  Database name associated with the user's credentials.


**authentication.$INDEX.database**

  *Type*: `string`

  Database name.


**authentication.$INDEX.topology**

  *Type*: `topology`


**authentication.$INDEX.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



### PostgreSQL


Configuration of authenticator using PostgreSQL as authentication data source.

**authentication.$INDEX.mechanism**

  *Type*: `password_based`

  Authentication mechanism.


**authentication.$INDEX.backend**

  *Type*: `postgresql`

  Backend type.


**authentication.$INDEX.password_hash_algorithm**

  *Type*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *Default*: `{"salt_position":"prefix","name":"sha256"}`

  Options for password hash verification.


**authentication.$INDEX.query**

  *Type*: `string`

  SQL used to query data for authentication, such as password hash.


**authentication.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.


**authentication.$INDEX.server**

  *Type*: `string`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The PostgreSQL default port 5432 is used if `[:Port]` is not specified.


**authentication.$INDEX.database**

  *Type*: `string`

  Database name.


**authentication.$INDEX.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authentication.$INDEX.username**

  *Type*: `string`

  The username associated with the bridge in the external database used for authentication or identification purposes.


**authentication.$INDEX.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**authentication.$INDEX.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.


**authentication.$INDEX.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



### Redis

#### Redis Single Node


Configuration of authenticator using Redis (Standalone) as authentication data source.

**authentication.$INDEX.mechanism**

  *Type*: `password_based`

  Authentication mechanism.


**authentication.$INDEX.backend**

  *Type*: `redis`

  Backend type.


**authentication.$INDEX.cmd**

  *Type*: `string`

  The Redis Command used to query data for authentication such as password hash, currently only supports <code>HGET</code> and <code>HMGET</code>.


**authentication.$INDEX.password_hash_algorithm**

  *Type*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *Default*: `{"salt_position":"prefix","name":"sha256"}`

  Options for password hash verification.


**authentication.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.


**authentication.$INDEX.server**

  *Type*: `string`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The Redis default port 6379 is used if `[:Port]` is not specified.


**authentication.$INDEX.redis_type**

  *Type*: `single`

  *Default*: `single`

  Single mode. Must be set to 'single' when Redis server is running in single mode.


**authentication.$INDEX.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authentication.$INDEX.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**authentication.$INDEX.database**

  *Type*: `non_neg_integer`

  *Default*: `0`

  Redis database ID.


**authentication.$INDEX.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.


**authentication.$INDEX.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



#### Redis Cluster 


Configuration of authenticator using Redis (Cluster) as authentication data source.

**authentication.$INDEX.mechanism**

  *Type*: `password_based`

  Authentication mechanism.


**authentication.$INDEX.backend**

  *Type*: `redis`

  Backend type.


**authentication.$INDEX.cmd**

  *Type*: `string`

  The Redis Command used to query data for authentication such as password hash, currently only supports <code>HGET</code> and <code>HMGET</code>.


**authentication.$INDEX.password_hash_algorithm**

  *Type*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *Default*: `{"salt_position":"prefix","name":"sha256"}`

  Options for password hash verification.


**authentication.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.


**authentication.$INDEX.servers**

  *Type*: `string`

  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
A host entry has the following form: `Host[:Port]`.
The Redis default port 6379 is used if `[:Port]` is not specified.


**authentication.$INDEX.redis_type**

  *Type*: `cluster`

  *Default*: `cluster`

  Cluster mode. Must be set to 'cluster' when Redis server is running in clustered mode.


**authentication.$INDEX.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authentication.$INDEX.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**authentication.$INDEX.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.


**authentication.$INDEX.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



#### Redis Sentinel 


Configuration of authenticator using Redis (Sentinel) as authentication data source.

**authentication.$INDEX.mechanism**

  *Type*: `password_based`

  Authentication mechanism.


**authentication.$INDEX.backend**

  *Type*: `redis`

  Backend type.


**authentication.$INDEX.cmd**

  *Type*: `string`

  The Redis Command used to query data for authentication such as password hash, currently only supports <code>HGET</code> and <code>HMGET</code>.


**authentication.$INDEX.password_hash_algorithm**

  *Type*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *Default*: `{"salt_position":"prefix","name":"sha256"}`

  Options for password hash verification.


**authentication.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.


**authentication.$INDEX.servers**

  *Type*: `string`

  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
A host entry has the following form: `Host[:Port]`.
The Redis default port 6379 is used if `[:Port]` is not specified.


**authentication.$INDEX.redis_type**

  *Type*: `sentinel`

  *Default*: `sentinel`

  Sentinel mode. Must be set to 'sentinel' when Redis server is running in sentinel mode.


**authentication.$INDEX.sentinel**

  *Type*: `string`

  The cluster name in Redis sentinel mode.


**authentication.$INDEX.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authentication.$INDEX.password**

  *Type*: `string`

  The password associated with the bridge, used for authentication with the external database.


**authentication.$INDEX.database**

  *Type*: `non_neg_integer`

  *Default*: `0`

  Redis database ID.


**authentication.$INDEX.auto_reconnect**

  *Type*: `boolean`

  Deprecated since v5.0.15.


**authentication.$INDEX.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



### HTTP Service

#### HTTP GET Method


Configuration of authenticator using HTTP Server as authentication service (Using GET request).

**authentication.$INDEX.method**

  *Type*: `get`

  HTTP request method.


**authentication.$INDEX.headers**

  *Type*: `map`

  *Default*: `{"keep-alive":"timeout=30, max=1000","connection":"keep-alive","cache-control":"no-cache","accept":"application/json"}`

  List of HTTP headers (without <code>content-type</code>).


**authentication.$INDEX.mechanism**

  *Type*: `password_based`

  Authentication mechanism.


**authentication.$INDEX.backend**

  *Type*: `http`

  Backend type.


**authentication.$INDEX.url**

  *Type*: `string`

  URL of the HTTP server.


**authentication.$INDEX.body**

  *Type*: `#{term => binary()}`

  HTTP request body.


**authentication.$INDEX.request_timeout**

  *Type*: `duration_ms`

  *Default*: `5s`

  HTTP request timeout.


**authentication.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.


**authentication.$INDEX.connect_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  The timeout when connecting to the HTTP server.


**authentication.$INDEX.enable_pipelining**

  *Type*: `pos_integer`

  *Default*: `100`

  A positive integer. Whether to send HTTP requests continuously, when set to 1, it means that after each HTTP request is sent, you need to wait for the server to return and then continue to send the next request.


**authentication.$INDEX.max_retries**

  *Type*: `non_neg_integer`

  Deprecated since 5.0.4.


**authentication.$INDEX.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  The pool size.


**authentication.$INDEX.request**

  *Type*: `connector-http:request`

  Configure HTTP request parameters.


**authentication.$INDEX.retry_interval**

  *Type*: `timeout_duration`

  Deprecated since 5.0.4.


**authentication.$INDEX.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



#### HTTP POST Method


Configuration of authenticator using HTTP Server as authentication service (Using POST request).

**authentication.$INDEX.method**

  *Type*: `post`

  HTTP request method.


**authentication.$INDEX.headers**

  *Type*: `map`

  *Default*: `{"keep-alive":"timeout=30, max=1000","content-type":"application/json","connection":"keep-alive","cache-control":"no-cache","accept":"application/json"}`

  List of HTTP Headers.


**authentication.$INDEX.mechanism**

  *Type*: `password_based`

  Authentication mechanism.


**authentication.$INDEX.backend**

  *Type*: `http`

  Backend type.


**authentication.$INDEX.url**

  *Type*: `string`

  URL of the HTTP server.


**authentication.$INDEX.body**

  *Type*: `#{term => binary()}`

  HTTP request body.


**authentication.$INDEX.request_timeout**

  *Type*: `duration_ms`

  *Default*: `5s`

  HTTP request timeout.


**authentication.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.


**authentication.$INDEX.connect_timeout**

  *Type*: `timeout_duration_ms`

  *Default*: `15s`

  The timeout when connecting to the HTTP server.


**authentication.$INDEX.enable_pipelining**

  *Type*: `pos_integer`

  *Default*: `100`

  A positive integer. Whether to send HTTP requests continuously, when set to 1, it means that after each HTTP request is sent, you need to wait for the server to return and then continue to send the next request.


**authentication.$INDEX.max_retries**

  *Type*: `non_neg_integer`

  Deprecated since 5.0.4.


**authentication.$INDEX.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  The pool size.


**authentication.$INDEX.request**

  *Type*: `connector-http:request`

  Configure HTTP request parameters.


**authentication.$INDEX.retry_interval**

  *Type*: `timeout_duration`

  Deprecated since 5.0.4.


**authentication.$INDEX.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL connection settings.



{% emqxee %}

### LDAP



{% endemqxee %}

### Appendix: Hash Config for Credentials


Settings for simple algorithms.

**authentication.$INDEX.password_hash_algorithm.name**

  *Type*: `enum`

  *Optional*: `plain | md5 | sha | sha256 | sha512`

  Simple password hashing algorithm.


**authentication.$INDEX.password_hash_algorithm.salt_position**

  *Type*: `enum`

  *Default*: `prefix`

  *Optional*: `disable | prefix | suffix`

  Salt position for PLAIN, MD5, SHA, SHA256 and SHA512 algorithms.




Settings for bcrypt password hashing algorithm.

**authentication.$INDEX.password_hash_algorithm.name**

  *Type*: `bcrypt`

  BCRYPT password hashing.




Settings for bcrypt password hashing algorithm (for DB backends with write capability).

**authentication.$INDEX.password_hash_algorithm.name**

  *Type*: `bcrypt`

  BCRYPT password hashing.


**authentication.$INDEX.password_hash_algorithm.salt_rounds**

  *Type*: `integer`

  *Default*: `10`

  Salt rounds for BCRYPT password generation.




Settings for PBKDF2 password hashing algorithm.

**authentication.$INDEX.password_hash_algorithm.name**

  *Type*: `pbkdf2`

  PBKDF2 password hashing.


**authentication.$INDEX.password_hash_algorithm.mac_fun**

  *Type*: `enum`

  *Optional*: `md4 | md5 | ripemd160 | sha | sha224 | sha256 | sha384 | sha512`

  Specifies mac_fun for PBKDF2 hashing algorithm.


**authentication.$INDEX.password_hash_algorithm.iterations**

  *Type*: `integer`

  Iteration count for PBKDF2 hashing algorithm.


**authentication.$INDEX.password_hash_algorithm.dk_length**

  *Type*: `integer`

  Derived length for PBKDF2 hashing algorithm. If not specified, calculated automatically based on `mac_fun`.



## Authentication - JWT


Configuration when the JWT for authentication is issued using the HMAC algorithm.

**authentication.$INDEX.algorithm**

  *Type*: `enum`

  *Optional*: `hmac-based`

  JWT signing algorithm, Supports HMAC (configured as <code>hmac-based</code>) and RSA, ECDSA (configured as <code>public-key</code>).


**authentication.$INDEX.secret**

  *Type*: `string`

  The key to verify the JWT using HMAC algorithm.


**authentication.$INDEX.secret_base64_encoded**

  *Type*: `boolean`

  *Default*: `false`

  Whether secret is base64 encoded.


**authentication.$INDEX.mechanism**

  *Type*: `jwt`

  Authentication mechanism.


**authentication.$INDEX.acl_claim_name**

  *Type*: `string`

  *Default*: `acl`

  JWT claim name to use for getting ACL rules.


**authentication.$INDEX.verify_claims**

  *Type*: `[term]`

  *Default*: `[]`

  A list of custom claims to validate, which is a list of name/value pairs.
Values can use the following placeholders:
- <code>${username}</code>: Will be replaced at runtime with <code>Username</code> used by the client when connecting
- <code>${clientid}</code>: Will be replaced at runtime with <code>Client ID</code> used by the client when connecting
Authentication will verify that the value of claims in the JWT (taken from the Password field) matches what is required in <code>verify_claims</code>.


**authentication.$INDEX.from**

  *Type*: `enum`

  *Default*: `password`

  *Optional*: `username | password`

  Field to take JWT from.


**authentication.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.



Configuration when JWTs used for authentication need to be fetched from the JWKS endpoint.

**authentication.$INDEX.use_jwks**

  *Type*: `enum`

  *Optional*: `true`

  Whether to use JWKS.


**authentication.$INDEX.endpoint**

  *Type*: `string`

  JWKS endpoint, it's a read-only endpoint that returns the server's public key set in the JWKS format.


**authentication.$INDEX.pool_size**

  *Type*: `pos_integer`

  *Default*: `8`

  Size of the connection pool towards the bridge target service.


**authentication.$INDEX.refresh_interval**

  *Type*: `integer`

  *Default*: `300`

  JWKS refresh interval.


**authentication.$INDEX.ssl**

  *Type*: [ssl_client_opts](#ssl-tls-configuration-for-clients)

  *Default*: `{"enable":false}`

  SSL options.


**authentication.$INDEX.mechanism**

  *Type*: `jwt`

  Authentication mechanism.


**authentication.$INDEX.acl_claim_name**

  *Type*: `string`

  *Default*: `acl`

  JWT claim name to use for getting ACL rules.


**authentication.$INDEX.verify_claims**

  *Type*: `[term]`

  *Default*: `[]`

  A list of custom claims to validate, which is a list of name/value pairs.
Values can use the following placeholders:
- <code>${username}</code>: Will be replaced at runtime with <code>Username</code> used by the client when connecting
- <code>${clientid}</code>: Will be replaced at runtime with <code>Client ID</code> used by the client when connecting
Authentication will verify that the value of claims in the JWT (taken from the Password field) matches what is required in <code>verify_claims</code>.


**authentication.$INDEX.from**

  *Type*: `enum`

  *Default*: `password`

  *Optional*: `username | password`

  Field to take JWT from.


**authentication.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.

" 

Configuration when the JWT for authentication is issued using RSA or ECDSA algorithm.

**authentication.$INDEX.algorithm**

  *Type*: `enum`

  *Optional*: `public-key`

  JWT signing algorithm, Supports HMAC (configured as <code>hmac-based</code>) and RSA, ECDSA (configured as <code>public-key</code>).


**authentication.$INDEX.public_key**

  *Type*: `string`

  The public key used to verify the JWT.


**authentication.$INDEX.mechanism**

  *Type*: `jwt`

  Authentication mechanism.


**authentication.$INDEX.acl_claim_name**

  *Type*: `string`

  *Default*: `acl`

  JWT claim name to use for getting ACL rules.


**authentication.$INDEX.verify_claims**

  *Type*: `[term]`

  *Default*: `[]`

  A list of custom claims to validate, which is a list of name/value pairs.
Values can use the following placeholders:
- <code>${username}</code>: Will be replaced at runtime with <code>Username</code> used by the client when connecting
- <code>${clientid}</code>: Will be replaced at runtime with <code>Client ID</code> used by the client when connecting
Authentication will verify that the value of claims in the JWT (taken from the Password field) matches what is required in <code>verify_claims</code>.


**authentication.$INDEX.from**

  *Type*: `enum`

  *Default*: `password`

  *Optional*: `username | password`

  Field to take JWT from.


**authentication.$INDEX.enable**

  *Type*: `boolean`

  *Default*: `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.



## Authentication - Enhanced


Settings for Salted Challenge Response Authentication Mechanism
(SCRAM) authentication.

**authentication.$INDEX.mechanism**

  *Type*: `scram`

  Authentication mechanism.


**authentication.$INDEX.backend**

  *Type*: `built_in_database`

  Backend type.


**authentication.$INDEX.algorithm**

  *Type*: `enum`

  *Default*: `sha256`

  *Optional*: `sha256 | sha512`

  Hashing algorithm.


**authentication.$INDEX.iteration_count**

  *Type*: `non_neg_integer`

  *Default*: `4096`

  Iteration count.


**authentication.$INDEX.enable**

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



### Protobuf



### Avro



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

























### Pulsar













### RocketMQ



### RabbitMQ





### Azure Event Hubs











### Amazon Kinesis





### Google PubSub













### MySQL



### Redis













### MongoDB









### InfluxDB





### PostgreSQL



### TDengine



### TimescaleDB

### Apache IoTDB







### MatrixDB

### OpenTSDB



### GreptimeDB



### ClickHouse





### DynamoDB





### Cassandra



### Microsoft SQL Server





### Oracle Database



### HStreamDB



{% endemqxee %}

### Appendix: Common configurations



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


