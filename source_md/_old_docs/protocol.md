# 协议介绍 (Protocol)

## MQTT 协议

### 概览

MQTT 是一个轻量的发布订阅模式消息传输协议，专门针对低带宽和不稳定网络环境的物联网应用设计。

MQTT 官网: <http://mqtt.org>

MQTT V3.1.1 协议规范:
<http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html>

### 特点

1.  开放消息协议，简单易实现
2.  发布订阅模式，一对多消息发布
3.  基于 TCP/IP 网络连接
4.  1 字节固定报头，2 字节心跳报文，报文结构紧凑
5.  消息 QoS 支持，可靠传输保证

### 应用

MQTT 协议广泛应用于物联网、移动互联网、智能硬件、车联网、电力能源等领域。

1.  物联网 M2M 通信，物联网大数据采集
2.  Android 消息推送，WEB 消息推送
3.  移动即时消息，例如 Facebook Messenger
4.  智能硬件、智能家具、智能电器
5.  车联网通信，电动车站桩采集
6.  智慧城市、远程医疗、远程教育
7.  电力、石油与能源等行业市场

### MQTT 基于主题 (Topic) 消息路由

MQTT 协议基于主题 (Topic) 进行消息路由，主题 (Topic) 类似 URL 路径，例如:

    chat/room/1
    
    sensor/10/temperature
    
    sensor/+/temperature
    
    $SYS/broker/metrics/packets/received
    
    $SYS/broker/metrics/#

主题 (Topic) 通过 '/' 分割层级，支持 '+', '\#' 通配符:

    '+': 表示通配一个层级，例如 a/+，匹配 a/x, a/y
    
    '#': 表示通配多个层级，例如 a/#，匹配 a/x, a/b/c/d

订阅者与发布者之间通过主题路由消息进行通信，例如采用 mosquitto 命令行发布订阅消息:

    mosquitto_sub -t a/b/+ -q 1
    
    mosquitto_pub -t a/b/c -m hello -q 1

<div class="note">

<div class="admonition-title">

Note

</div>

订阅者可以订阅含通配符主题，但发布者不允许向含通配符主题发布消息。

</div>

### MQTT V3.1.1 协议报文

#### 报文结构

|                       |
| --------------------- |
| 固定报头 (Fixed header)    |
| 可变报头 (Variable header) |
| 报文有效载荷 (Payload)       |

#### 固定报头

<table style="width:82%;">
<colgroup>
<col style="width: 15%" />
<col style="width: 8%" />
<col style="width: 8%" />
<col style="width: 8%" />
<col style="width: 8%" />
<col style="width: 8%" />
<col style="width: 8%" />
<col style="width: 8%" />
<col style="width: 8%" />
</colgroup>
<tbody>
<tr class="odd">
<td>Bit</td>
<td><blockquote>
<p>7</p>
</blockquote></td>
<td><blockquote>
<p>6</p>
</blockquote></td>
<td><blockquote>
<p>5</p>
</blockquote></td>
<td><blockquote>
<p>4</p>
</blockquote></td>
<td><blockquote>
<p>3</p>
</blockquote></td>
<td><blockquote>
<p>2</p>
</blockquote></td>
<td><blockquote>
<p>1</p>
</blockquote></td>
<td><blockquote>
<p>0</p>
</blockquote></td>
</tr>
<tr class="even">
<td>byte1</td>
<td><blockquote>
<p>MQT</p>
</blockquote></td>
<td>T Pack</td>
<td>et typ</td>
<td>e</td>
<td></td>
<td><blockquote>
<p>Fla</p>
</blockquote></td>
<td>gs</td>
<td></td>
</tr>
<tr class="odd">
<td>byte2...</td>
<td><blockquote>
<p>Rem</p>
</blockquote></td>
<td>aining</td>
<td>Lengt</td>
<td>h</td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
</tbody>
</table>

#### 报文类型

|             |     |          |
| ----------- | --- | -------- |
| 类型名称        | 类型值 | 报文说明     |
| CONNECT     | 1   | 发起连接     |
| CONNACK     | 2   | 连接回执     |
| PUBLISH     | 3   | 发布消息     |
| PUBACK      | 4   | 发布回执     |
| PUBREC      | 5   | QoS2 消息回执 |
| PUBREL      | 6   | QoS2 消息释放 |
| PUBCOMP     | 7   | QoS2 消息完成 |
| SUBSCRIBE   | 8   | 订阅主题     |
| SUBACK      | 9   | 订阅回执     |
| UNSUBSCRIBE | 10  | 取消订阅     |
| UNSUBACK    | 11  | 取消订阅回执   |
| PINGREQ     | 12  | PING 请求   |
| PINGRESP    | 13  | PING 响应   |
| DISCONNECT  | 14  | 断开连接     |

#### PUBLISH 发布消息

PUBLISH 报文承载客户端与服务器间双向的发布消息。
PUBACK 报文用于接收端确认 QoS1 报文，PUBREC/PUBREL/PUBCOMP 报文用于 QoS2 消息流程。

#### PINGREQ/PINGRESP 心跳

客户端在无报文发送时，按保活周期 (KeepAlive) 定时向服务端发送 PINGREQ 心跳报文，服务端响应 PINGRESP 报文。PINGREQ/PINGRESP 报文均 2 个字节。

### MQTT 消息 QoS

MQTT 发布消息 QoS 保证不是端到端的，是客户端与服务器之间的。订阅者收到 MQTT 消息的 QoS 级别，最终取决于发布消息的 QoS 和主题订阅的 QoS。

<table style="width:67%;">
<colgroup>
<col style="width: 22%" />
<col style="width: 22%" />
<col style="width: 22%" />
</colgroup>
<tbody>
<tr class="odd">
<td > 发布消息的 QoS</td>
<td > 主题订阅的 QoS</td>
<td > 接收消息的 QoS</td>
</tr>
<tr class="even">
<td><blockquote>
<p>0</p>
</blockquote></td>
<td><blockquote>
<p>0</p>
</blockquote></td>
<td><blockquote>
<p>0</p>
</blockquote></td>
</tr>
<tr class="odd">
<td><blockquote>
<p>0</p>
</blockquote></td>
<td><blockquote>
<p>1</p>
</blockquote></td>
<td><blockquote>
<p>0</p>
</blockquote></td>
</tr>
<tr class="even">
<td><blockquote>
<p>0</p>
</blockquote></td>
<td><blockquote>
<p>2</p>
</blockquote></td>
<td><blockquote>
<p>0</p>
</blockquote></td>
</tr>
<tr class="odd">
<td><blockquote>
<p>1</p>
</blockquote></td>
<td><blockquote>
<p>0</p>
</blockquote></td>
<td><blockquote>
<p>0</p>
</blockquote></td>
</tr>
<tr class="even">
<td><blockquote>
<p>1</p>
</blockquote></td>
<td><blockquote>
<p>1</p>
</blockquote></td>
<td><blockquote>
<p>1</p>
</blockquote></td>
</tr>
<tr class="odd">
<td><blockquote>
<p>1</p>
</blockquote></td>
<td><blockquote>
<p>2</p>
</blockquote></td>
<td><blockquote>
<p>1</p>
</blockquote></td>
</tr>
<tr class="even">
<td><blockquote>
<p>2</p>
</blockquote></td>
<td><blockquote>
<p>0</p>
</blockquote></td>
<td><blockquote>
<p>0</p>
</blockquote></td>
</tr>
<tr class="odd">
<td><blockquote>
<p>2</p>
</blockquote></td>
<td><blockquote>
<p>1</p>
</blockquote></td>
<td><blockquote>
<p>1</p>
</blockquote></td>
</tr>
<tr class="even">
<td><blockquote>
<p>2</p>
</blockquote></td>
<td><blockquote>
<p>2</p>
</blockquote></td>
<td><blockquote>
<p>2</p>
</blockquote></td>
</tr>
</tbody>
</table>

#### Qos0 消息发布订阅

![image](./_static/images/qos0_seq.png)

#### Qos1 消息发布订阅

![image](./_static/images/qos1_seq.png)

#### Qos2 消息发布订阅

![image](./_static/images/qos2_seq.png)

### MQTT 会话 (Clean Session)

MQTT 客户端向服务器发起 CONNECT 请求时，可以通过 'Clean Session' 标志设置会话。

'Clean Session' 设置为 0，表示创建一个持久会话，在客户端断开连接时，会话仍然保持并保存离线消息，直到会话超时注销。

'Clean
Session' 设置为 1，表示创建一个新的临时会话，在客户端断开时，会话自动销毁。

### MQTT 连接保活心跳

MQTT 客户端向服务器发起 CONNECT 请求时，通过 KeepAlive 参数设置保活周期。

客户端在无报文发送时，按 KeepAlive 周期定时发送 2 字节的 PINGREQ 心跳报文，服务端收到 PINGREQ 报文后，回复 2 字节的 PINGRESP 报文。

服务端在 1.5 个心跳周期内，既没有收到客户端发布订阅报文，也没有收到 PINGREQ 心跳报文时，主动心跳超时断开客户端 TCP 连接。

<div class="note">

<div class="admonition-title">

Note

</div>

emqttd 消息服务器默认按最长 2.5 心跳周期超时设计。

</div>

### MQTT 遗愿消息 (Last Will)

MQTT 客户端向服务器端 CONNECT 请求时，可以设置是否发送遗愿消息 (Will
Message) 标志，和遗愿消息主题 (Topic) 与内容 (Payload)。

MQTT 客户端异常下线时 (客户端断开前未向服务器发送 DISCONNECT 消息)，MQTT 消息服务器会发布遗愿消息。

### MQTT 保留消息 (Retained Message)

MQTT 客户端向服务器发布 (PUBLISH) 消息时，可以设置保留消息 (Retained Message) 标志。保留消息 (Retained
Message) 会驻留在消息服务器，后来的订阅者订阅主题时仍可以接收该消息。

例如 mosquitto 命令行发布一条保留消息到主题 'a/b/c':

    mosquitto_pub -r -q 1 -t a/b/c -m 'hello'

之后连接上来的 MQTT 客户端订阅主题 'a/b/c' 时候，仍可收到该消息:

    $ mosquitto_sub -t a/b/c -q 1
    hello

保留消息 (Retained
Message) 有两种清除方式:

1.  客户端向有保留消息的主题发布一个空消息:
    
        mosquitto_pub -r -q 1 -t a/b/c -m ''

2.  消息服务器设置保留消息的超期时间。

### MQTT WebSocket 连接

MQTT 协议除支持 TCP 传输层外，还支持 WebSocket 作为传输层。通过 WebSocket 浏览器可以直连 MQTT 消息服务器，发布订阅模式与其他 MQTT 客户端通信。

MQTT 协议的 WebSocket 连接，必须采用 binary 模式，并携带子协议 Header:

    Sec-WebSocket-Protocol: mqttv3.1 或 mqttv3.1.1

### MQTT 协议客户端库

#### emqtt 客户端库

emqtt 项目组: <https://github.com/emqtt>

|                                                 |                 |
| ----------------------------------------------- | --------------- |
| [emqttc](https://github.com/emqtt/emqttc)       | Erlang MQTT 客户端库 |
| [CocoaMQTT](https://github.com/emqtt/CocoaMQTT) | Swift 语言 MQTT 客户端库 |
| [QMQTT](https://github.com/emqtt/qmqtt)         | QT 框架 MQTT 客户端库    |

#### Eclipse Paho 客户端库

Paho 官网: <http://www.eclipse.org/paho/>

#### mqtt.org 官网客户端库

mqtt.org:
    <https://github.com/mqtt/mqtt.github.io/wiki/libraries>

### MQTT 与 XMPP 协议对比

MQTT 协议设计简单轻量、路由灵活，将在移动互联网物联网消息领域，全面取代 PC 时代的 XMPP 协议:

1.  MQTT 协议一个字节固定报头，两个字节心跳报文，报文体积小编解码容易。XMPP 协议基于繁重的 XML，报文体积大且交互繁琐。
2.  MQTT 协议基于主题 (Topic) 发布订阅模式消息路由，相比 XMPP 基于 JID 的点对点消息路由更为灵活。
3.  MQTT 协议未定义报文内容格式，可以承载 JSON、二进制等不同类型报文。XMPP 协议采用 XML 承载报文，二进制必须 Base64 编码等处理。
4.  MQTT 协议支持消息收发确认和 QoS 保证，XMPP 主协议并未定义类似机制。MQTT 协议有更好的消息可靠性保证。

## MQTT-SN 协议

MQTT-SN 协议是 MQTT 的直系亲属，它使用 UDP 进行通信，标准的端口是 1884。MQTT-SN
的主要目的是为了适应受限的设备和网络，比如一些传感器，只有很小的内存和
CPU，TCP 对于这些设备来说非常奢侈。还有一些网络，比如 ZIGBEE，报文的长度在 300 字节以下，无法承载太大的数据包。所以
MQTT-SN 的数据包更小巧。

MQTT-SN 的官方标准下载地址:
<http://mqtt.org/new/wp-content/uploads/2009/06/MQTT-SN_spec_v1.2.pdf>

### MQTT-SN 和 MQTT 的区别

MQTT-SN 的信令和 MQTT 大部分都相同，比如都有 Will, 都有 Connect/Subscribe/Publish 命令.

MQTT-SN 最大的不同是，Topic 使用 TopicId 来代替，而 TopicId 是一个 16 比特的数字。每一个数字对应一个
Topic, 设备和云端需要使用 REGISTER 命令映射 TopicId 和 Topic 的对应关系。

MQTT-SN 可以随时更改 Will 的内容，甚至可以取消。而 MQTT 只允许在 CONNECT 时设定 Will 的内容，
而且不允许更改.

MQTT-SN 的网络中有网关这种设备，它负责把 MQTT-SN 转换成 MQTT，和云端的 MQTT Broker 通信. MQTT-SN
的协议支持自动发现网关的功能。

MQTT-SN 还支持设备的睡眠功能，如果设备进入睡眠状态，无法接收 UDP 数据，网关将把下行的 PUBLISH
消息缓存起来，直到设备苏醒后再传送。

### EMQX-SN 网关插件

EMQX-SN 是 EMQ X 的一个网关插件，实现了 MQTT-SN 的大部分功能，它相当于一个在云端的 MQTT-SN 网关，直接和 EMQ
X Broker 相连。

#### 配置参数

File: etc/plugins/emqx\_sn.conf:

    mqtt.sn.port = 1884
    
    mqtt.sn.advertise_duration = 900
    
    mqtt.sn.gateway_id = 1
    
    mqtt.sn.username = mqtt_sn_user
    
    mqtt.sn.password = abc

|                             |                                    |
| --------------------------- | ---------------------------------- |
| mqtt.sn.port                | 指定 MQTT-SN 监听的端口号                  |
| mqtt.sn.advertise\_duration | ADVERTISE 消息的发送间隔 (秒)               |
| mqtt.sn.gateway\_id         | 网关 ID                              |
| mqtt.sn.username            | 这是可选的参数，指定所有 MQTT-SN 连接的用户名，用于鉴权模块 |
| mqtt.sn.password            | 这也是可选的参数，和 username 一起使用           |

#### 启动 emqx-sn

``` sourceCode 
./bin/emqx_ctl plugins load emqx_sn
```

### MQTT-SN 客户端库

1.  <https://github.com/eclipse/paho.mqtt-sn.embedded-c/>
2.  <https://github.com/ty4tw/MQTT-SN>
3.  <https://github.com/njh/mqtt-sn-tools>
4.  <https://github.com/arobenko/mqtt-sn>

## LWM2M 协议

LwM2M 全称是 Lightweight Machine-To-Machine，是由 Open Mobile Alliance (OMA)
定义的一套适用于物联网的轻量级协议，它提供了设备管理和通讯的功能，尤其适用于资源有限的终端设备。协议可以在
[这里](http://www.openmobilealliance.org/wp/) 下载。

LwM2M 基于 REST 架构，使用 CoAP 作为底层的传输协议，承载在 UDP 或者 SMS
上，因而报文结构简单小巧，并且在网络资源有限及无法确保设备始终在线的环境里同样适用。

![image](./_static/images/lwm2m_protocols.png)

LwM2M 最主要的实体包括 LwM2M Server 和 LwM2M Client。

LwM2M Server 作为服务器，部署在 M2M 服务供应商处或网络服务供应商处。LwM2M 定义了两种服务器

  - 一种是 LwM2M BOOTSTRAP SERVER，emqx-lwm2m 插件并未实现该服务器的功能。
  - 一种是 LwM2M SERVER，emqx-lwm2m 实现该服务器在 UDP 上的功能，SMS 并没有实现。

LwM2M Client 作为客户端，部署在各个 LwM2M 设备上。

在 LwM2M Server 和 LwM2M Client 之间，LwM2M 协议定义了 4 个接口。

1.  引导接口 Bootstrap：向 LwM2M 客户端提供注册到 LwM2M
    服务器的必要信息，例如服务器访问信息、客户端支持的资源信息等。
2.  客户端注册接口 Client Registration：使 LwM2M 客户端与 LwM2M 服务器互联，将 LwM2M
    客户端的相关信息存储在 LwM2M 服务器上。只有完成注册后，LwM2M
    客户端与服务器端之间的通信与管理才成为可能。
3.  设备管理与服务实现接口 Device Management and Service Enablement：该接口的主控方为 LwM2M
    服务器，服务器向客户端发送指令，客户端对指令做出回应并将回应消息发送给服务器。
4.  信息上报接口 Information Reporting：允许 LwM2M
    服务器端向客户端订阅资源信息，客户端接收订阅后按照约定的模式向服务器端报告自己的资源变化情况。

![image](./_static/images/lwm2m_arch.png)

LwM2M 把设备上的服务抽象为 Object 和 Resource, 在 XML 文件中定义各种 Object 的属性和功能。可以在
[这里](http://www.openmobilealliance.org/wp/OMNA/LwM2M/LwM2MRegistry.html)
找到 XML 的各种定义。

LwM2M 协议预定义了 8 种 Object 来满足基本的需求，分别是：

  - Security 安全对象
  - Server 服务器对象
  - Access Control 访问控制对象
  - Device 设备对象
  - Connectivity Monitoring 连通性监控对象
  - Firmware 固件对象
  - Location 位置对象
  - Connectivity Statistics 连通性统计对象

### EMQX-LWM2M 插件

EMQX-LWM2M 是 EMQ X 服务器的一个网关插件，实现了 LwM2M 的大部分功能。MQTT 客户端可以通过 EMQX-LWM2M
访问支持 LwM2M 的设备。设备也可以往 EMQX-LWM2M 上报 notification，为 EMQ X 后端的服务采集数据。

### MQTT 和 LwM2M 的转换

从 MQTT 客户端可以发送 Command 给 LwM2M 设备。MQTT 到 LwM2M 的命令使用如下的 topic

``` sourceCode 
"lwm2m/{?device_end_point_name}/command".
```

其中 MQTT Payload 是一个 json 格式的字符串，指定要发送的命令，更多的细节请参见 emqx-lwm2m 的文档。

LwM2M 设备的回复用如下 topic 传送

``` sourceCode 
"lwm2m/{?device_end_point_name}/response".
```

MQTT Payload 也是一个 json 格式的字符串，更多的细节请参见 emqx-lwm2m 的文档。

#### 配置参数

File: etc/emqx\_lwm2m.conf:

    lwm2m.port = 5683
    
    lwm2m.certfile = etc/certs/cert.pem
    
    lwm2m.keyfile = etc/certs/key.pem
    
    lwm2m.xml_dir =  etc/lwm2m_xml

|                |                                                  |
| -------------- | ------------------------------------------------ |
| lwm2m.port     | 指定 LwM2M 监听的端口号，为了避免和 emqx-coap 冲突，使用了非标准的 5783 端口 |
| lwm2m.certfile | DTLS 使用的证书                                       |
| lwm2m.keyfile  | DTLS 使用的秘钥                                       |
| lwm2m.xml\_dir | 存放 XML 文件的目录，这些 XML 用来定义 LwM2M Object            |

#### 启动 emqx-lwm2m

``` sourceCode 
./bin/emqx_ctl plugins load emqx_lwm2m
```

### LwM2M 的客户端库

  - <https://github.com/eclipse/wakaama>
  - <https://github.com/OpenMobileAlliance/OMA-LWM2M-DevKit>
  - <https://github.com/AVSystem/Anjay>
  - <http://www.eclipse.org/leshan/>
