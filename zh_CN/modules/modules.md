# 模块管理

EMQX 发行包中提供了丰富了功能模块，包括 认证鉴权、协议接入、消息下发、多语言扩展、运维监控、内部模块等。
模块管理页面可以启动、关闭模块，还可以进行模块的配置和数据管理。

## 模块列表

目前 EMQX 发行包提供的模块包括：

- 认证鉴权
  - [内置访问控制文件](./internal_acl.md)
  - [MySQL 认证/访问控制](./mysql_authentication.md)
  - [PostgreSQL 认证/访问控制](./pgsql_authentication.md)
  - [Redis 认证/访问控制](./redis_authentication.md)
  - [HTTP 认证/访问控制](./http_authentication.md)
  - [内置数据库 认证/访问控制](./mnesia_authentication.md)
  - [MongoDB 认证/访问控制](./mongo_authentication.md)
  - [PSK File 认证](./psk_authentication.md)
  - [LDAP 认证/访问控制](./ldap_authentication.md)
  - [JWT 认证](./jwt_authentication.md)
- 协议接入
  - [LwM2M 协议网关](./lwm2m_protocol.md)
  - [MQTT-SN 协议网关](./mqtt_sn_protocol.md)
  - [TCP 协议网关](./tcp_protocol.md)
  - [JT/T808 协议网关](./jt808_protocol.md)
  - [GB/T32960 网关](./gbt32960_protocol.md)
  - [CoAP 协议网关](./coap_protocol.md)
  - [Stomp 协议网关](./stomp_protocol.md)
- 消息下发
  - [Kafka 消费组](./kafka_consumer.md)
  - [Pulsar 消费组](./pulsar_consumer.md)
  - [MQTT 订阅者](./mqtt_subscriber.md)
- 多语言扩展
  - [协议接入](./exproto.md)
  - [钩子](./exhook.md)
- 运维监控
  - [Prometheus Agent](../tutorial/prometheus.md)
- 内部模块
  - [热配置](./hot_confs.md)
  - [主题监控](./topic_metrics.md)
  - [MQTT 上下线通知](./presence.md)
  - [MQTT 代理订阅](./subscription.md)
  - [MQTT 主题重写](./topic_rewrite.md)
  - [MQTT 保留消息](./retainer.md)
  - [MQTT 延迟发布](./delayed_publish.md)
  


## 启停模块

目前启动模块有以下两种方式：

1.  默认加载
2.  使用 Dashboard 启停模块


**开启默认加载**

如需在 EMQX 启动时就默认启动某模块，则直接在 `data/loaded_modules` 添加需要启动的模块名称。

例如，目前 EMQX 自动加载的模块有：

```json
[
    {
    "name": "internal_acl",
    "enable": true,
    "configs": {"acl_rule_file": "{{ acl_file }}"}
    },
    {
        "name": "presence",
        "enable": true,
        "configs": {"qos": 0}
    },
    {
        "name": "recon",
        "enable": true,
        "configs": {}
    },
    {
        "name": "retainer",
        "enable": true,
        "configs": {
            "expiry_interval": 0,
            "max_payload_size": "1MB",
            "max_retained_messages": 0,
            "storage_type": "ram"
        }
    }
]
```

**使用 Dashboard 启停模块**

若开启了 Dashbord 的模块，可以直接通过访问 `http://localhost:18083/modules` 中的模块管理页面启停模块。

