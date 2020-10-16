# 模块管理

EMQ X 发行包中提供了丰富了功能模块，包括 认证鉴权、协议接入、消息下发、多语言扩展、运维监控、内部模块等。
模块管理页面可以启动、关闭模块，还可以进行模块的配置和数据管理。

## 模块列表

目前 EMQ X 发行包提供的模块包括：

- 认证鉴权
  - 内置访问控制文件
  - MySQL 认证/访问控制
  - PostgreSQL 认证/访问控制
  - Redis 认证/访问控制
  - HTTP 认证/访问控制
  - 内置数据库 认证/访问控制
  - MongoDB 认证/访问控制
  - LDAP 认证/访问控制
  - JWT 认证
- 协议接入
  - LwM2M 协议网关
  - MQTT-SN 协议网关
  - TCP 协议网关
  - JT/T808 协议网关
  - CoAP 协议网关
  - Stomp 协议网关
- 消息下发
  - Kafka 消费组
  - Pulsar 消费组
  - MQTT 订阅者
- 多语言扩展
  - 协议接入
  - 钩子
- 运维监控
  - Recon
  - Prometheus Agent
- 内部模块
  - 主题监控
  - MQTT 增强认证
  - MQTT 上下线通知
  - MQTT 代理订阅
  - MQTT 主题重写
  - MQTT 保留消息
  - MQTT 延迟发布


## 启停模块

目前启动模块有以下两种方式：

1.  默认加载
2.  使用 Dashboard 启停模块


**开启默认加载**

如需在 EMQ X 启动时就默认启动某模块，则直接在 `data/loaded_modules` 添加需要启动的模块名称。

例如，目前 EMQ X 自动加载的模块有：

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

