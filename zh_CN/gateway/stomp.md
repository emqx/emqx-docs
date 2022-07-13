# Stomp 协议网关

## 简介

Stomp 网关基于 [Stomp v1.2](https://stomp.github.io/stomp-specification-1.2.html) 版本实现，并兼容 Stomp v1.0 和 v1.1 的标准。

## 快速开始

EMQX 5.0 中，可以通过 Dashboard 配置并启用 Stomp 网关。

也可以通过 HTTP-API 来启用，例如：
```bash
curl -X 'POST' 'http://127.0.0.1:18083/api/v5/gateway' \
  -u admin:public \
  -H 'Content-Type: application/json' \
  -d '{
  "name": "stomp",
  "enable": true,
  "mountpoint": "stomp/",
  "listeners": [
    {
      "type": "tcp",
      "name": "default",
      "bind": "61613",
      "max_conn_rate": 1000,
      "max_connections": 1024000
    }
  ]
}'
```

或在 emqx.conf 中配置启用，例如：

```properties
gateway.stomp {

  mountpoint = "stomp/"

  listeners.tcp.default {
    bind = 61613
    acceptors = 16
    max_connections = 1024000
    max_conn_rate = 1000
  }
}
```

::: tip
通过配置文件进行配置网关，需要在每个节点中进行配置；通过 Dashboard 或者 HTTP API 管理则会在整个集群中生效。
:::

Stomp 网关支持 TCP、SSL 类型的监听器，其完整可配置的参数列表参考：[网关配置 - 监听器](../admin/cfg.md)


## 认证

由于 Stomp 协议的连接报文已定义了用户名和密码的概念，所以它支持以下多种认证器类型，例如：
- [内置数据库认证](../security/authn/mnesia.md)
- [MySQL 认证](../security/authn/mysql.md)
- [MongoDB 认证](../security/authn/mongodb.md)
- [PostgreSQL 认证](../security/authn/postgresql.md)
- [Redis 认证](../security/authn/redis.md)
- [HTTP Server 认证](../security/authn/http.md)
- [JWT 认证](../security/authn/jwt.md)

Stomp 网关使用 STOMP 协议的 CONNECT 或 STOMP 报文中的信息来生成客户端的认证信息。默认情况下：

- Client ID：为随机生成的字符串。
- Username：为 CONNECT 或 STOMP 报文 Headers 中的 `login` 字段的值。
- Password：为 CONNECT 或 STOMP 报文 Headers 中的 `passcode` 字段的值。

例如，通过 HTTP-API 为 Stomp 网关创建一个内置数据库认证：

```bash
curl -X 'POST' \
  'http://127.0.0.1:18083/api/v5/gateway/stomp/authentication' \
  -u admin:public
  -H 'accept: application/json' \
  -H 'Content-Type: application/json' \
  -d '{
  "backend": "built_in_database",
  "mechanism": "password_based",
  "password_hash_algorithm": {
    "name": "sha256",
    "salt_position": "suffix"
  },
  "user_id_type": "username"
}'
```

或通过配置为 Stomp 网关添加一个内置数据库认证：
```properties
gateway.stomp {

  authentication {
    backend = built_in_database
    mechanism = password_based
    password_hash_algorithm {
      name = sha256
      salt_position = suffix
    }
    user_id_type = username
  }
}
```

与 MQTT 协议不同，**网关仅支持创建一个认证器，而不是认证器列表（或认证链）**。当不启用任何认证器时，表示允许所有的 Stomp 客户端都具有接入的权限。

其他类型的认证器的配置格式参考：[安全- 认证器](../security/authn/authn.md)

## 发布订阅

Stomp 协议完全兼容发布订阅的消息模式，Stomp 网关使用：

- Stomp 协议的 SEND 报文作为消息发布。其主题为 SEND 报文中的 `destination` 字段，消息内容为 SEND 报文的消息体内容，QoS 固定为 0。
- Stomp 协议的 SUBSCRIBE 报文作为订阅请求。其主题为 SUBSCRIBE 报文中的 `destination` 字段，QoS 固定为 0。且支持 MQTT 协议中定义的通配符。
- Stomp 协议的 UNSUBSCRIBE 报文作为取消订阅请求。其主题为 UNSUBSCRIBE 报文中的`destination` 字段。

网关内无独立的发布订阅的权限控制，其对主题的权限控制需要统一在 [授权（Authorization）](../security/authz/authz.md) 中管理。

## 用户层接口

- 详细配置说明参考：[网关配置 - Stomp 网关](../admin/cfg.md)
- 详细 HTTP API 接口参考：[HTTP API - 网关](../admin/api.md)

## 客户端库

- [erlang-stomp-client](https://github.com/KodiEhf/erlang-stomp-client)
- [stomp.py](https://github.com/jasonrbriggs/stomp.py)
