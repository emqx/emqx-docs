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

# MQTT 客户端库

本页选取各个编程语言中热门 MQTT 客户端库进行介绍说明，提供连接、发布、订阅、取消订阅基本功能代码示例。

- [MQTT C 客户端库](./c.md)
- [MQTT Java 客户端库](./java.md)
- [MQTT Go 客户端库](./go.md)
- [MQTT Erlang 客户端库](./erlang.md)
- [MQTT JavaScript 客户端库](./javascript.md)
- [MQTT Python 客户端库](./python.md)

MQTT 社区收录了完整的 MQTT 客户端库列表，本章节对热门的每个库都提供连接样例、支持度分析，你可以[点击此处](https://github.com/mqtt/mqtt.github.io/wiki/libraries)查看。


## MQTT 客户端生命周期

MQTT 客户端整个生命周期的行为可以概括为：建立连接、订阅主题、接收消息并处理、向指定主题发布消息、取消订阅、断开连接。

标准的客户端库在每个环节都暴露出相应的方法，不同库在相同环节所需方法参数含义大致相同，具体选用哪些参数、启用哪些功能特性需要用户深入了解 MQTT 协议特性并结合实际应用场景而定。

以一个客户端连接并发布、处理消息为例，每个环节大致需要进行的步骤：

- **建立连接**：
  
  - 指定 MQTT Broker 基本信息接入地址与端口
  - 指定传输类型是 TCP 还是 MQTT over WebSocket
  - 如果启用 TLS 需要选择协议版本并携带相应的的证书
  - Broker 启用了认证鉴权则客户端需要携带相应的 MQTT Username Password 信息
  - 配置客户端参数如 keepalive 时长、clean session 回话保留标志位、MQTT 协议版本、遗嘱消息（LWT）等
  
- **订阅主题**：连接建立成功后可以订阅主题，需要指定主题信息

  - 指定主题过滤器 Topic，订阅的时候支持主题通配符 `+` 与 `#` 的使用
  - 指定 QoS，根据客户端库和 Broker 的实现可选  Qos 0 1 2，注意部分 Broker 与云服务提供商不支持部分 QoS 级别，如 AWS IoT 、阿里云 IoT 套件、Azure IoT Hub 均不支持 QoS 2 级别消息
  - 订阅主题可能因为网络问题、Broker 端 ACL 规则限制而失败

- **接收消息并处理**：

  - 一般是在连接时指定处理函数，依据客户端库与平台的网络编程模型不同此部分处理方式略有不同

- **发布消息**：向指定主题发布消息

  - 指定目标主题，注意该主题不能包含通配符 `+` 或 `#`，若主题中包含通配符可能会导致消息发布失败、客户端断开等情况（视 Broker 与客户端库实现方式）
  - 指定消息 QoS 级别，同样存在不同 Broker 与平台支持的 QoS 级别不同，如 Azure IoT Hub 发布 QoS 2 的消息将断开客户端连接
  - 指定消息体内容，消息体内容大小不能超出 Broker 设置最大消息大小
  - 指定消息 Retain 保留消息标志位

- **取消订阅**：

  - 指定目标主题即可

- **断开连接**：

  - 客户端主动断开连接，服务器端将发布遗嘱消息（LWT）

