---
# 编写日期
date: 2020-02-07 17:15:26
# 作者 Github 名称
author: wivwiv
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref: 
---

# 多协议网关插件

多协议网关插件是 EMQX 的重要组件，使 EMQX 能够过支持多种物联网通信协议。通过网关插件的协议转换、消息路由和处理功能，使用不同通信协议的设备和应用程序都可以连接到 EMQX，实现了跨协议的互操作性。本页介绍了 EMQX 支持的接入协议以及协议网关插件的使用和配置。

## MQTT-SN 协议

MQTT-SN 协议是 MQTT 的直系亲属，它使用 UDP 进行通信，标准的端口是1884。MQTT-SN 的主要目的是为了适应受限的设备和网络，比如一些传感器，只有很小的内存和 CPU，TCP 对于这些设备来说非常奢侈。还有一些网络，比如 ZIGBEE，报文的长度在300字节以下，无法承载太大的数据包。所以 MQTT-SN 的数据包更小巧。

MQTT-SN 的官方标准下载地址:
<http://mqtt.org/new/wp-content/uploads/2009/06/MQTT-SN_spec_v1.2.pdf>

### MQTT-SN 和 MQTT 的区别

MQTT-SN 的信令和 MQTT 大部分都相同，比如都有 Will, 都有 Connect/Subscribe/Publish 命令。MQTT-SN 最大的不同是，Topic 使用 TopicId 来代替，而 TopicId 是一个16比特的数字。每一个数字对应一个 Topic，设备和云端需要使用 REGISTER 命令映射 TopicId 和 Topic 的对应关系。

MQTT-SN 可以随时更改 Will 的内容，甚至可以取消，而 MQTT 只允许在 CONNECT 时设定 Will 的内容，
而且不允许更改。

MQTT-SN 的网络中有网关这种设备，它负责把 MQTT-SN 转换成 MQTT，和云端的 MQTT Broker 通信。 MQTT-SN 的协议支持自动发现网关的功能。

MQTT-SN 还支持设备的睡眠功能，如果设备进入睡眠状态，无法接收 UDP 数据，网关将把下行的 PUBLISH
消息缓存起来，直到设备苏醒后再传送。

### EMQX-SN 网关插件

EMQX-SN 是 EMQX 的一个网关插件，实现了 MQTT-SN 的大部分功能，它相当于一个在云端的 MQTT-SN 网关，直接和 EMQX Broker 相连。

#### 配置参数

```bash
# File: etc/plugins/emqx_sn.conf:

mqtt.sn.port = 1884

mqtt.sn.advertise_duration = 900

mqtt.sn.gateway_id = 1

mqtt.sn.username = mqtt_sn_user

mqtt.sn.password = abc
```

| 配置项                      |       说明                           |
| --------------------------- | ---------------------------------- |
| mqtt.sn.port                | 指定 MQTT-SN 监听的端口号                  |
| mqtt.sn.advertise_duration | ADVERTISE 消息的发送间隔(秒)               |
| mqtt.sn.gateway_id         | 网关 ID                              |
| mqtt.sn.username            | 这是可选的参数，指定所有 MQTT-SN 连接的用户名，用于鉴权模块 |
| mqtt.sn.password            | 这也是可选的参数，和 username 一起使用           |

#### 启动 emqx-sn

```bash
./bin/emqx_ctl plugins load emqx_sn
```

### MQTT-SN 客户端库

1. <https://github.com/eclipse/paho.mqtt-sn.embedded-c/>
2. <https://github.com/ty4tw/MQTT-SN>
3. <https://github.com/njh/mqtt-sn-tools>
4. <https://github.com/arobenko/mqtt-sn>

## LWM2M 协议

LwM2M 全称是 Lightweight Machine-To-Machine，是由 Open Mobile Alliance (OMA) 定义的一套适用于物联网的轻量级协议，它提供了设备管理和通讯的功能，尤其适用于资源有限的终端设备。协议可以在[这里](http://www.openmobilealliance.org/wp/)下载。

LwM2M 基于 REST 架构，使用 CoAP 作为底层的传输协议，承载在 UDP 或者 SMS 上，因而报文结构简单小巧，并且在网络资源有限及无法确保设备始终在线的环境里同样适用。

<!-- ![image](./_assets/lwm2m_protocols.png) -->

LwM2M 最主要的实体包括 LwM2M Server 和 LwM2M Client。

LwM2M Server 作为服务器，部署在 M2M 服务供应商处或网络服务供应商处。LwM2M 定义了两种服务器：

- 一种是 LwM2M BOOTSTRAP SERVER，emqx-lwm2m 插件并未实现该服务器的功能。
- 一种是 LwM2M SERVER，emqx-lwm2m 实现该服务器在 UDP 上的功能，SMS 并没有实现。

LwM2M Client 作为客户端，部署在各个 LwM2M 设备上。

在 LwM2M Server 和 LwM2M Client 之间，LwM2M 协议定义了4个接口。

1. 引导接口 Bootstrap：向 LwM2M 客户端提供注册到 LwM2M 服务器的必要信息，例如服务器访问信息、客户端支持的资源信息等。
2. 客户端注册接口 Client Registration：使 LwM2M 客户端与 LwM2M 服务器互联，将 LwM2M 客户端的相关信息存储在 LwM2M 服务器上。只有完成注册后，LwM2M 客户端与服务器端之间的通信与管理才成为可能。
3. 设备管理与服务实现接口 Device Management and Service Enablement：该接口的主控方为 LwM2M 服务器，服务器向客户端发送指令，客户端对指令做出回应并将回应消息发送给服务器。
4. 信息上报接口 Information Reporting：允许 LwM2M 服务器端向客户端订阅资源信息，客户端接收订阅后按照约定的模式向服务器端报告自己的资源变化情况。

<!-- ![image](./_assets/lwm2m_arch.png) -->

LwM2M 把设备上的服务抽象为 Object 和 Resource, 在 XML 文件中定义各种 Object 的属性和功能。可以在
[这里](http://www.openmobilealliance.org/wp/OMNA/LwM2M/LwM2MRegistry.html)找到 XML 的各种定义。

LwM2M 协议预定义了8种 Object 来满足基本的需求，分别是：

- Security 安全对象
- Server 服务器对象
- Access Control 访问控制对象
- Device 设备对象
- Connectivity Monitoring 连通性监控对象
- Firmware 固件对象
- Location 位置对象
- Connectivity Statistics 连通性统计对象

### EMQX-LWM2M 插件

EMQX-LWM2M 是 EMQX 服务器的一个网关插件，实现了 LwM2M 的大部分功能。MQTT 客户端可以通过 EMQX-LWM2M 访问支持 LwM2M 的设备。设备也可以往 EMQX-LWM2M 上报 notification，为 EMQX 后端的服务采集数据。

### MQTT 和 LwM2M 的转换

从 MQTT 客户端可以发送 Command 给 LwM2M 设备。MQTT 到 LwM2M 的命令使用如下的 topic：

```bash
"lwm2m/{?device_end_point_name}/command".
```

其中 MQTT Payload 是一个 json 格式的字符串，指定要发送的命令，更多的细节请参见 emqx-lwm2m 的文档。

LwM2M 设备的回复用如下 topic 传送：

```bash
"lwm2m/{?device_end_point_name}/response".
```

MQTT Payload 也是一个 json 格式的字符串，更多的细节请参见 emqx-lwm2m 的文档。

#### 配置参数

```bash
## File: etc/emqx_lwm2m.conf:

lwm2m.port = 5683

lwm2m.certfile = etc/certs/cert.pem

lwm2m.keyfile = etc/certs/key.pem

lwm2m.xml_dir =  etc/lwm2m_xml
```

|   配置项        |         说明                                      |
| -------------- | ------------------------------------------------ |
| lwm2m.port     | 指定 LwM2M 监听的端口号，为了避免和 emqx-coap 冲突，使用了非标准的5783端口 |
| lwm2m.certfile | DTLS 使用的证书                                       |
| lwm2m.keyfile  | DTLS 使用的秘钥                                       |
| lwm2m.xml\_dir | 存放 XML 文件的目录，这些 XML 用来定义 LwM2M Object            |

#### 启动 emqx-lwm2m

```bash
./bin/emqx_ctl plugins load emqx_lwm2m
```

### LwM2M 的客户端库

- <https://github.com/eclipse/wakaama>
- <https://github.com/OpenMobileAlliance/OMA-LWM2M-DevKit>
- <https://github.com/AVSystem/Anjay>
- <http://www.eclipse.org/leshan/>

## CoAP 协议

CoAP (Constrained Application Protocol) 是一种专为物联网环境设计的轻量级网络传输协议。它基于 UDP 通信，支持客户端/服务器模式，通过确认/非确认消息及资源观察实现可靠传输。CoAP 消息开销小，对无线性能和电池寿命要求较低。

CoAP 通过其轻量、高效的特性非常适合物联网环境，可以单独使用，也可以与其他协议相结合来实现不同的应用场景。

EMQX 通过插件提供 CoAP 接入支持，使用方式请参考 [EMQX CoAP](https://github.com/emqx/emqx/blob/main-v4.4/apps/emqx_coap/README.md)。

## STOMP 协议

STOMP (Simple Text Oriented Messaging Protocol) 是一种面向文本的消息中间件传输协议。它使用简单直观的文本格式进行通信，支持发布订阅和点对点两种消息模式。STOMP 易于实现和扩展，可以跨多种语言使用，常用于客户端和消息代理之间的异步消息传递。

STOMP 通过其简单、轻量级的文本通信方式，对消息中间件和异步通信提供了一种简单高效的支持，它易学易用的特点使其可以跨平台跨语言地应用到分布式系统中的消息传输，成为一种流行的文本机器间通信协议。

EMQX 通过插件提供 STOMP 接入支持，使用方式请参考 [EMQX STOMP](https://github.com/emqx/emqx/blob/main-v4.4/apps/emqx_stomp/README.md)。
