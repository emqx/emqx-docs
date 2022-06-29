# 集群安全

当涉及到EMQX集群的安全时，主要有有两个方面需要考虑。

* 确保每个节点在集群中监听的端口安全。
* 对Erlang的 cookie进行保密, 见`node.cookie`配置。

::: tip 
一个好的实践是，通过配置防火墙规则来保持集群端口的内部防火墙规则，如AWS安全组，或iptables。
:::

## 集群内通信端口

为了形成一个集群，EMQX节点需要通过一些常规端口进行互联

如果集群节点之间有防火墙，常规监听端口应该允许集群中的其他节点连通。

EMQX节点之间有**两种不同的通道**进行通信。

### Erlang 分布式传输端口

::: tip
EMQX使用一个传统的端口映射机制。
但并**不使用**[Erlang Port Mapper Daemon, EPMD](https://www.erlang.org/doc/man/epmd.html)
:::

Erlang分布端口。`ListeningPort = BasePort + Offset`。
其中`BasePort`是4370(默认不可以配置)，`Offset`是节点名称的后缀。
如果节点名称没有数字后缀，`Offsset`就是0。

例如，在`emqx.conf`中的`node.name = emqx@192.168.0.12`应该使
节点的监听端口为`4370`，而`emqx1`（或`emqx-1`）的端口为`4371`，以此类推。

### 集群RPC端口

默认情况下，每个emqx节点有一个RPC监听端口。
防火墙应该配置成允许访问。

端口映射规则类似于Erlang分布式端口映射规则。
只有 "BasePort "是 "5370"。

也就是说，在`emqx.conf`中的`node.name = emqx@192.168.0.12`应该使节点的
监听端口`5370`，端口`5371`用于`emqx1`（或`emqx-1`），以此类推。

::: tip
docker容器中的EMQX使用静态端口`5369`进行集群RPC。
:::

### 使用TLS为集群RPC传输层

::: warning
TLS是以增加CPU负载和RAM使用为代价的。
:::

要为集群RPC配置TLS，应在emqx.conf中设置以下配置

确保在`emqx.conf`中的配置如下

```
rpc {
  driver = ssl
  certfile = /path/to/cert/domain.pem
  cacertfile = /path/to/cert/ca.pem
  keyfile = /path/to/cert/domain.key
}
```

以下是创建证书和自签名CA的步骤。

1. 使用`openssl`工具创建一个根CA。

   ```
   # Create self-signed root CA:
   openssl req -nodes -x509 -sha256 -days 1825 -newkey rsa:2048 -keyout rootCA.key -out rootCA.pem -subj "/O=LocalOrg/CN=LocalOrg-Root-CA"
   ```

2. 使用在步骤1创建的rootCA.pem为节点生成CA签名的证书。

   ```
   # Create a private key:
   openssl genrsa -out domain.key 2048
   # Create openssl extfile:
   cat <<EOF > domain.ext
   authorityKeyIdentifier=keyid,issuer
   basicConstraints=CA:FALSE
   subjectAltName = @alt_names
   [alt_names]
   DNS.1 = backplane
   EOF
   # Create a CSR:
   openssl req -key domain.key -new -out domain.csr -subj "/O=LocalOrg"
   # Sign the CSR with the Root CA:
   openssl x509 -req -CA rootCA.pem -CAkey rootCA.key -in domain.csr -out domain.pem -days 365 -CAcreateserial -extfile domain.ext
   ```
   
   集群中的所有节点必须使用由同一CA签署的证书。

3. 把生成的`domain.pem`、`domain.key`和`rootCA.pem`文件放到集群的每个节点上的`/var/lib/emqx/ssl`。
   确保emqx用户可以读取这些文件，并且权限设置为`600`。 


### 为Erlang分布式协议使用TLS

::: warning
TLS是以增加CPU负载和RAM使用为代价的
:::

Erlang分布被EMQX节点用来同步数据库更新
以及在整个集群中的临时性，如收集运行时指标等。

* 确保`ssl_dist.conf`文件有正确的密钥和证书的路径。
* 确保配置`cluster.proto_dist`被设置为`inet_tls`。

