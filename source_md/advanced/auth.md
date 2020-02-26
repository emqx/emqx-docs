---
# 标题
title: 认证
# 编写日期
date: 2020-02-19 18:40:28
# 作者 Github 名称
author: terry-xiaoyu
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref: undefined
---

# 认证 {#authentication}

认证（认证鉴权）指的是当一个客户端连接到 EMQ X Broker 的时候，通过服务器端的配置来控制客户端连接服务器的权限。EMQ X Broker 的认证支持包括两个层面：

- MQTT 协议本身在 CONNECT 报文中指定用户名和密码，EMQ X Broker 以插件形式支持基于 Username、ClientID、HTTP、JWT、LDAP 及各类数据库如 MongoDB、MySQL、PostgreSQL、Redis 等多种形式的认证。
- 在传输层上，TLS 可以保证使用客户端证书的客户端到服务器的身份验证，并确保服务器向客户端验证服务器证书。也支持基于 PSK 的 TLS/DTLS 认证。

## EMQ X Broker 认证与认证链 {#auth-and-auth-chain}

EMQ X Broker 认证相关插件名称以 `emqx_auth` 开头。比如 `emqx_auth_username` 是使用 emqx 内部数据库 Mnesia 对用户名做认证的插件，`emqx_auth_redis` 是使用 Redis 数据库对用户名做认证的插件。

当同时启用多个认证插件时，EMQ X Broker 将按照插件开启先后顺序进行链式认证，一旦认证成功或者失败(插件返回 allow 或者 deny) 就终止认证链并允许客户端接入，如果插件无法认证用户 (可能该用户不在数据库里，这时候插件返回 ignore) 则转给认证链里的下一个插件，直到最后一个插件。认证链的认证过程如下所示：

```
                     ignore                       ignore      ignore
[emqx_auth_username] ------> [emqx_auth_clientid] ------> ... ------> [allow_anonymous?]
        |                            |                     |             Yes / \ No
        |                            |                     |                /   \
   allow deny                   allow deny            allow deny        allow   deny
```

当认证链没有被任何插件终结，即所有插件都返回了 ignore，EMQ X Broker 则会判断是否启用了匿名登录，如启用则允许登录，否则禁止登录。

匿名登录默认是启用的，可在 `etc/emqx.conf` 中禁用：

```
allow_anonymous = false
```

## TLS 认证 {#auth-tls}

MQTT TLS 的默认端口是 8883：

```
listener.ssl.external = 8883
```

配置证书和 CA：

```
listener.ssl.external.keyfile = etc/certs/key.pem
listener.ssl.external.certfile = etc/certs/cert.pem
listener.ssl.external.cacertfile = etc/certs/cacert.pem
```

注意，默认的 etc/certs 目录下面的 `key.pem`、`cert.pem` 和 `cacert.pem` 是 EMQ X Broker 生成的自签名证书，所以在使用支持 TLS 的客户端测试的时候，需要将上面的 CA 证书 `etc/certs/cacert.pem` 配置到客户端。

服务端支持的 cipher 列表需要显式指定，默认的列表与 Mozilla 的服务端 cipher 列表一致：

```
listener.ssl.external.ciphers = ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384,ECDHE-ECDSA-AES256-SHA384,ECDHE-RSA-AES256-SHA384,ECDHE-ECDSA-DES-CBC3-SHA,ECDH-ECDSA-AES256-GCM-SHA384,ECDH-RSA-AES256-GCM-SHA384,ECDH-ECDSA-AES256-SHA384,ECDH-RSA-AES256-SHA384,DHE-DSS-AES256-GCM-SHA384,DHE-DSS-AES256-SHA256,AES256-GCM-SHA384,AES256-SHA256,ECDHE-ECDSA-AES128-GCM-SHA256,ECDHE-RSA-AES128-GCM-SHA256,ECDHE-ECDSA-AES128-SHA256,ECDHE-RSA-AES128-SHA256,ECDH-ECDSA-AES128-GCM-SHA256,ECDH-RSA-AES128-GCM-SHA256,ECDH-ECDSA-AES128-SHA256,ECDH-RSA-AES128-SHA256,DHE-DSS-AES128-GCM-SHA256,DHE-DSS-AES128-SHA256,AES128-GCM-SHA256,AES128-SHA256,ECDHE-ECDSA-AES256-SHA,ECDHE-RSA-AES256-SHA,DHE-DSS-AES256-SHA,ECDH-ECDSA-AES256-SHA,ECDH-RSA-AES256-SHA,AES256-SHA,ECDHE-ECDSA-AES128-SHA,ECDHE-RSA-AES128-SHA,DHE-DSS-AES128-SHA,ECDH-ECDSA-AES128-SHA,ECDH-RSA-AES128-SHA,AES128-SHA
```

## PSK 认证 {#auth-tls-psk}

如果希望使用 PSK 认证，需要将 [TLS 认证](#auth-tls) 中的 `listener.ssl.external.ciphers` 注释掉，然后配置 `listener.ssl.external.psk_ciphers`：

```
#listener.ssl.external.ciphers = ECDHE-ECDSA-AES256-GCM-SHA384,...
listener.ssl.external.psk_ciphers = PSK-AES128-CBC-SHA,PSK-AES256-CBC-SHA,PSK-3DES-EDE-CBC-SHA,PSK-RC4-SHA

```

然后启用 emqx_psk_file 插件：

```
$ emqx_ctl plugins load emqx_psk_file
```

PSK 的配置文件为 `etc/psk.txt`，使用冒号`:` 分隔 PSK ID 和 PSK：

```
client1:1234
client2:abcd
```
