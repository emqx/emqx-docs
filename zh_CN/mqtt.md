# MQTT 协议

## MQTT 轻量发布订阅消息协议

### 概览

MQTT 是一个轻量的发布订阅模式消息传输协议，专门针对低带宽和不稳定网络环境的物联网应用设计。

MQTT 官网: [ http://mqtt.org ](http://mqtt.org)

MQTT V3.1.1 协议规范: [http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html](http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html)

### 特点

1. 开放消息协议，简单易实现
2. 发布订阅模式，一对多消息发布
3. 基于 TCP/IP 网络连接
4. 1 字节固定报头，2 字节心跳报文，报文结构紧凑
5. 消息 QoS 支持，可靠传输保证

### 应用

MQTT 协议广泛应用于物联网、移动互联网、智能硬件、车联网、电力能源等领域。

1. 物联网 M2M 通信，物联网大数据采集
2. Android 消息推送，WEB 消息推送
3. 移动即时消息，例如 Facebook Messenger
4. 智能硬件、智能家具、智能电器
5. 车联网通信，电动车站桩采集
6. 智慧城市、远程医疗、远程教育
7. 电力、石油与能源等行业市场

## MQTT 基于主题(Topic)消息路由

MQTT 协议基于主题(Topic)进行消息路由，主题(Topic)类似 URL 路径，例如:

    chat/room/1

    sensor/10/temperature

    sensor/+/temperature

    $SYS/broker/metrics/packets/received

    $SYS/broker/metrics/#

主题(Topic)通过'/'分割层级，支持'+', '#'通配符:

    '+': 表示通配一个层级，例如a/+，匹配a/x, a/y

    '#': 表示通配多个层级，例如a/#，匹配a/x, a/b/c/d

订阅者与发布者之间通过主题路由消息进行通信，例如采用 mosquitto 命令行发布订阅消息:

    mosquitto_sub -t a/b/+ -q 1

    mosquitto_pub -t a/b/c -m hello -q 1

::: tip
订阅者可以订阅含通配符主题，但发布者不允许向含通配符主题发布消息。
:::

## MQTT V3.1.1 协议报文

### 报文结构

| 固定报头(Fixed header)    |
| ------------------------- |
| 可变报头(Variable header) |
| 报文有效载荷(Payload)     |

### 固定报头

```
+----------+-----+-----+-----+-----+-----+-----+-----+-----+
| Bit      |  7  |  6  |  5  |  4  |  3  |  2  |  1  |  0  |
+----------+-----+-----+-----+-----+-----+-----+-----+-----+
| byte1    |   MQTT Packet type    |         Flags         |
+----------+-----------------------+-----------------------+
| byte2... |   Remaining Length                            |
+----------+-----------------------------------------------+
```

### 报文类型

| 类型名称    | 类型值 | 报文说明      |
| ----------- | ------ | ------------- |
| CONNECT     | 1      | 发起连接      |
| CONNACK     | 2      | 连接回执      |
| PUBLISH     | 3      | 发布消息      |
| PUBACK      | 4      | 发布回执      |
| PUBREC      | 5      | QoS2 消息回执 |
| PUBREL      | 6      | QoS2 消息释放 |
| PUBCOMP     | 7      | QoS2 消息完成 |
| SUBSCRIBE   | 8      | 订阅主题      |
| SUBACK      | 9      | 订阅回执      |
| UNSUBSCRIBE | 10     | 取消订阅      |
| UNSUBACK    | 11     | 取消订阅回执  |
| PINGREQ     | 12     | PING 请求     |
| PINGRESP    | 13     | PING 响应     |
| DISCONNECT  | 14     | 断开连接      |

### PUBLISH 发布消息

PUBLISH 报文承载客户端与服务器间双向的发布消息。
PUBACK 报文用于接收端确认 QoS1 报文，PUBREC/PUBREL/PUBCOMP 报文用于 QoS2 消息流程。

### PINGREQ/PINGRESP 心跳

客户端在无报文发送时，按保活周期(KeepAlive)定时向服务端发送 PINGREQ 心跳报文，服务端响应 PINGRESP 报文。PINGREQ/PINGRESP 报文均 2 个字节。

## MQTT 消息 QoS

MQTT 发布消息 QoS 保证不是端到端的，是客户端与服务器之间的。订阅者收到 MQTT 消息的 QoS 级别，最终取决于发布消息的 QoS 和主题订阅的 QoS。

| 发布消息的 QoS | 主题订阅的 QoS | 接收消息的 QoS |
| -------------- | -------------- | -------------- |
| 0              | 0              | 0              |
| 0              | 1              | 0              |
| 0              | 2              | 0              |
| 1              | 0              | 0              |
| 1              | 1              | 1              |
| 1              | 2              | 1              |
| 2              | 0              | 0              |
| 2              | 1              | 1              |
| 2              | 2              | 2              |

### Qos0 消息发布订阅

![image](./_static/images/qos0_seq.png)

### Qos1 消息发布订阅

![image](./_static/images/qos1_seq.png)

### Qos2 消息发布订阅

![image](./_static/images/qos2_seq.png)

## MQTT 会话(Clean Session)

MQTT 客户端向服务器发起 CONNECT 请求时，可以通过'Clean Session'标志设置会话。

'Clean Session'设置为 0，表示创建一个持久会话，在客户端断开连接时，会话仍然保持并保存离线消息，直到会话超时注销。

'Clean Session'设置为 1，表示创建一个新的临时会话，在客户端断开时，会话自动销毁。

## MQTT 连接保活心跳

MQTT 客户端向服务器发起 CONNECT 请求时，通过 KeepAlive 参数设置保活周期。

客户端在无报文发送时，按 KeepAlive 周期定时发送 2 字节的 PINGREQ 心跳报文，服务端收到 PINGREQ 报文后，回复 2 字节的 PINGRESP 报文。

服务端在 1.5 个心跳周期内，既没有收到客户端发布订阅报文，也没有收到 PINGREQ 心跳报文时，主动心跳超时断开客户端 TCP 连接。

::: tip
emqttd 消息服务器默认按最长 2.5 心跳周期超时设计。
:::

## MQTT 遗愿消息(Last Will)

MQTT 客户端向服务器端 CONNECT 请求时，可以设置是否发送遗愿消息(Will
Message)标志，和遗愿消息主题(Topic)与内容(Payload)。

MQTT 客户端异常下线时(客户端断开前未向服务器发送 DISCONNECT 消息)，MQTT 消息服务器会发布遗愿消息。

## MQTT 保留消息(Retained Message)

MQTT 客户端向服务器发布(PUBLISH)消息时，可以设置保留消息(Retained Message)标志。保留消息(Retained
Message)会驻留在消息服务器，后来的订阅者订阅主题时仍可以接收该消息。

例如 mosquitto 命令行发布一条保留消息到主题'a/b/c':

    mosquitto_pub -r -q 1 -t a/b/c -m 'hello'

之后连接上来的 MQTT 客户端订阅主题'a/b/c'时候，仍可收到该消息:

    $ mosquitto_sub -t a/b/c -q 1
    hello

保留消息(Retained Message)有两种清除方式:

1. 客户端向有保留消息的主题发布一个空消息:
   mosquitto_pub -r -q 1 -t a/b/c -m ''
2. 消息服务器设置保留消息的超期时间。

## MQTT WebSocket 连接

MQTT 协议除支持 TCP 传输层外，还支持 WebSocket 作为传输层。通过 WebSocket 浏览器可以直连 MQTT 消息服务器，发布订阅模式与其他 MQTT 客户端通信。

MQTT 协议的 WebSocket 连接，必须采用 binary 模式，并携带子协议 Header:

    Sec-WebSocket-Protocol: mqttv3.1 或 mqttv3.1.1

## MQTT 协议客户端库

### emqtt 客户端库

emqtt 项目组: [ https://github.com/emqtt ](https://github.com/emqtt)

| [ emqttc ](https://github.com/emqtt/emqttc)       | Erlang MQTT 客户端库     |
| ------------------------------------------------- | ------------------------ |
| [ CocoaMQTT ](https://github.com/emqtt/CocoaMQTT) | Swift 语言 MQTT 客户端库 |
| [ QMQTT ](https://github.com/emqtt/qmqtt)         | QT 框架 MQTT 客户端库    |

### Eclipse Paho 客户端库

Paho 官网: [ http://www.eclipse.org/paho/ ](http://www.eclipse.org/paho/)

### mqtt.org 官网客户端库

mqtt.org: [ https://github.com/mqtt/mqtt.github.io/wiki/libraries](https://github.com/mqtt/mqtt.github.io/wiki/libraries)

## MQTT 与 XMPP 协议对比

MQTT 协议设计简单轻量、路由灵活，将在移动互联网物联网消息领域，全面取代 PC 时代的 XMPP 协议:

1. MQTT 协议一个字节固定报头，两个字节心跳报文，报文体积小编解码容易。XMPP 协议基于繁重的 XML，报文体积大且交互繁琐。
2. MQTT 协议基于主题(Topic)发布订阅模式消息路由，相比 XMPP 基于 JID 的点对点消息路由更为灵活。
3. MQTT 协议未定义报文内容格式，可以承载 JSON、二进制等不同类型报文。XMPP 协议采用 XML 承载报文，二进制必须 Base64 编码等处理。
4. MQTT 协议支持消息收发确认和 QoS 保证，XMPP 主协议并未定义类似机制。MQTT 协议有更好的消息可靠性保证。
