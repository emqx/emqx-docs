# 集群安全

## 设置节点 Cookie

Erlang 节点间通过 cookie 进行互连认证。cookie 是一个字符串，只有 cookie 相同的两个节点才能建立连接。[上节](#node-and-distributed-erlang) 中我们曾经使用 `-setcookie my_nodes` 参数给四个节点设置了相同的 cookie: `my_nodes`。

详见: <http://erlang.org/doc/reference_manual/distributed.html>

## 防火墙设置

### 集群节点发现端口

若预先设置了环境变量 WITH_EPMD=1, 启动 emqx 时会使用启动 epmd (监听端口 4369) 做节点发现。称为 `epmd 模式`。

若环境变量 WITH_EPMD 没有设置，则启动 emqx 时不启用 epmd，而使用 emqx ekka 的节点发现，这也是 4.0 之后的默认节点发现方式。称为 `ekka 模式`。

**epmd 模式：**

如果集群节点间存在防火墙，防火墙需要为每个节点开通 TCP 4369 端口，用来让各节点能互相访问。

防火墙还需要开通一个 TCP 从 `node.dist_listen_min`(包含) 到 `node.dist_listen_max`(包含) 的端口段，
这两个配置的默认值都是 `6369`。

**ekka 模式（4.0 版本之后的默认模式）：**

跟`empd 模式`不同，在`ekka 模式`下，集群发现端口的映射关系是约定好的，而不是动态的。
`node.dist_listen_min` and `node.dist_listen_max` 两个配置在`ekka 模式`下不起作用。

如果集群节点间存在防火墙，防火墙需要放开这个约定的端口。约定端口的规则如下：

```
ListeningPort = BasePort + Offset
```

其中 `BasePort` 为 4370 (不可配置), `Offset` 为节点名的数字后缀. 如果节点名没有数字后缀的话，
`Offsset` 为 0。

举例来说, 如果 `emqx.conf` 里配置了节点名：`node.name = emqx@192.168.0.12`，那么监听端口为 `4370`，
但对于 `emqx1` (或者 `emqx-1`) 端口就是 `4371`，以此类推。

### 集群 PRC 端口

每个节点还需要监听一个 RPC 端口，也需要被防火墙也放开。跟上面说的`ekka 模式`下的集群发现端口一样，这个 RPC 端口也是约定式的。

RPC 端口的规则跟`ekka 模式`下的集群发现端口类似，只不过 `BasePort = 5370`。

就是说，如果 `emqx.conf` 里配置了节点名：`node.name = emqx@192.168.0.12`，那么监听端口为 `5370`，
但对于 `emqx1` (或者 `emqx-1`) 端口就是 `5371`，以此类推。

## EMQX 集群协议设置

Erlang 集群中各节点可通过 TCPv4、TCPv6 或 TLS 方式连接，可在 `etc/emqx.conf`
中配置连接方式:

| 配置名                | 类型     | 默认值              | 描述                                                         |
| --------------------- | -------- | ------------------- | ------------------------------------------------------------ |
| cluster.proto_dist    | enum     | `inet_tcp`          | 分布式协议，可选值：<br />  - inet_tcp: 使用 TCP IPv4<br/>  - inet6_tcp: 使用 TCP IPv6<br/>  - inet_tls: 使用 TLS |
| node.ssl_dist_optfile | 文件路径 | `etc/ssl_dist.conf` | 当 `cluster.proto_dist` 选定为 inet_tls 时，需要配置 `etc/ssl_dist.conf` 文件，指定 TLS 证书等 |