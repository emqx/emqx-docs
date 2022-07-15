# MQTT

MQTT 桥接是 EMQX 与其他 MQTT 服务通讯的通道，既可以是 EMQX，也可以是支持 MQTT 协议的其他服务。MQTT 桥接既可以订阅外部服务的消息，也可以发布消息到外部服务。但仅支持单向的工作模式，只能成为生产者，或消费者。如果需要双向桥接，需要创建多个方向不同的 MQTT 桥接，来完成数据的双向流通。

## 使用配置文件创建 MQTT Bridge

### `ingress` 类型 MQTT Bridge 配置参数列表

从外部服务桥接消息到本地。
配置示例：

```js
bridges {
  mqtt {
    "mqtt_bridge_name" {
      direction = "ingress"
      remote_topic = "remote/topic/t1"
      remote_qos = 0
      payload = "${payload}"
      local_topic = "local/topic/t1"
      local_qos = 0
      retain = false
      connector {
        server = "183.134.197.178:1883"
        username = "user1"
        password = "pwd1"
        proto_ver = "v4"
        clean_start = true
        keepalive = "60s"
        reconnect_interval = "10s"
        retry_interval = "1s"
        mode = "cluster_shareload"
        bridge_mode = false
        replayq {
          offload = false
          seg_bytes = "100MB"
        }
        ssl {
            enable = false
            verify = "verify_none"
        }
      }
    }
  }
}
```

| 参数名 | 描述 | 类型 | 必填 | 取值范围 |
| -- | -- | -- | -- | -- |
| direction  | 桥接方向：</br>ingress 表示从外部服务订阅消息，发布到本地</br>egress 表示将消息从本地发布到外部服务 | String | 是 | ingress |
| connector | MQTT 连接器 | connector() |  是 | Connecter 配置参数列表 |
| remote_topic | 订阅外部服务的 Topic | String | 是 |  - |
| remote_qos | 订阅的外部服务 QoS | Integer |  是 | 0 \| 1 \| 2 |
| local_topic | 发布到本地的 Topic | String | 是 | - |
| local_qos | 发布到本地的 QoS | Integer |  是 | 0 \| 1 \| 2 |
| retain | 发布到本地的 Retain 标记 | Boolean |  是 | - |
| payload | 发布到本地的 Payload | String |  是 | - |

### `egress` 类型 MQTT Bridge 配置参数列表

将本地消息桥接至外部服务。
配置示例：

```js
bridges {
  mqtt {
    "mqtt_bridge_name" {
      direction = "egress"
      remote_topic = "remote/topic/t1"
      remote_qos = 0
      payload = "${payload}"
      local_topic = "local/topic/t1"
      retain = false
      connector {
        server = "183.134.197.178:1883"
        username = "user1"
        password = "pwd1"
        proto_ver = "v4"
        clean_start = true
        keepalive = "60s"
        reconnect_interval = "10s"
        retry_interval = "1s"
        mode = "cluster_shareload"
        bridge_mode = false
        replayq {
          offload = false
          seg_bytes = "100MB"
        }
        ssl {
            enable = false
            verify = "verify_none"
        }
      }
    }
  }
}
```

| 参数名 | 描述 | 类型 | 必填 | 取值范围 |
| -- | -- | -- | -- | -- |
| direction  | 桥接方向：</br>ingress 表示从外部服务订阅消息，发布到本地</br>egress 表示将消息从本地发布到外部服务 | String | 是 | egress |
| connector | MQTT 连接器 | connector() |  是 | 参考 Connecter 配置参数列表 |
| remote_topic | 发布到外部服务的 Topic | String | 是 |  - |
| remote_qos |  发布到外部服务 QoS | Integer |  是 | 0 \| 1 \| 2 |
| retain | 发布到外部服务的 Retain 标记 | Boolean |  是 | - |
| local_topic | 获取数据的本地 Topic | String | 是 | - |
| payload | 发布到外部服务的 Payload | String |  是 | - |

### Connecter 配置参数列表

桥接使用的连接器。

| 参数名 | 描述 | 类型 | 必填 | 取值范围 |
| -- | -- | -- | -- | -- |
| server | 外部服务地址，ip:port | String | 是 | [0-255].[0-255].[0-255].[0-255]:[0-65535] |
| mode | 连接器模式 | String | 否 | cluster_shareload |
| reconnect_interval | 自动重连间隔时间 | Integer | 否 | - |
| proto_ver | 协议版本 | String | 否 |  v3 \| v4 \| v5 |
| bridge_mode | 桥接模式，仅在外部服务为 EMQX 时生效，可以提高订阅的并发性能 | Boolean | 否 | - |
| username | 连接使用的用户名 | String | 否 | - |
| password | 连接使用的密码 | String | 否 | - |
| clean_start | 设置连接使用的 clean_session 属性 | Boolean | 否 | - |
| keepalive | 连接心跳周期 | Integer | 否 | - |
| retry_interval | 重试间隔 | Integer | 否 | - |
| max_inflight | 最大消息窗口数量，在 MQTT V5 协议中为 `Receive Maximum` | Integer | 否 | - |
| replayq | 本地消息缓存 | replayq() | 否 | 参考 replayq 配置参数列表 |
| ssl | 加密连接证书配置 | ssl() | 否 | - |

### replayq 配置参数列表

| 参数名 | 描述 | 类型 | 必填 | 取值范围 |
| -- | -- | -- | -- | -- |
| dir | 本地缓存的文件目录，设置为 `false` 表示关闭 | String \| `false` | 否 | String \| false |
| seg_bytes | 本地缓存的文件大小限制，当超出限制后，会创建一个新的文件来保存新的缓存消息 | String | 否 | - |
| offload | 是否开启过载模式，开启后消息会先使用内存来保存，仅当缓存的数据超过 `seg_bytes` 设置的上线后，才会写入文件 | Boolean | 否 | - |

### SSL 配置

参考 [SSL](../security/ssl.md)

## 使用 Bridge

TODO: 
