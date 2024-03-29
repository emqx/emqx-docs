# MQTT over QUIC

MQTT 基于 TCP 协议工作，这为消息传输提供了可靠性、有序性和无损特性。但在车联网（IoV）领域，对消息传输的实时性和高效性的要求日益凸显，TCP 的局限性也逐渐显现。随着车辆、传感器、基础设施的日益互联，构建更安全、智能、敏捷的交通生态需要克服 TCP 传输的瓶颈。

为此，EMQX 5.0 开创性地引入了 MQTT over QUIC 协议，在兼容 MQTT 协议所有功能的基础上，允许物联网客户端通过 QUIC 与 EMQX 建立连接并进行通信。这为客户端链接带来了显著的优势，采用 QUIC 的客户端可以提高连接与消息吞吐性能并减少延迟，特别是对于弱网、链路频繁变化、不稳定网络环境很常见的车联网场景。

::: tip
目前 MQTT over QUIC 还不是 MQTT 的标准协议，但它已经具备投入生产能力，并且 EMQ 正在 OASIS 中积极推动其标准化进程。
:::

## 了解 QUIC

QUIC 最初由 Google 开发，是为了满足日益增长的客户端对更高效、更安全和低延迟协议的需求。其设计旨在克服 TCP 和 TLS（网络通信的传统构建模块）的特定低效率问题，后来被互联网工程任务组（IETF）采纳为全球标准。

它是一种新的传输协议，提供更快的连接建立速度。通过使用更高效的算法，可以更快地将数据传输速率提高到稳定状态，从而改善了拥塞控制。此外，QUIC 使用基于流的多路复用架构，可以独立传输数据流，有助于避免队首阻塞，并可以提高在高丢包或延迟情况下的性能。

QUIC 作为下一代互联网传输协议，是 HTTP/3 的唯一底层传输协议，相较 TCP，它在减少连接开销和消息延迟的同时，显著提升整体吞吐量和移动连接的稳定性。因此，QUIC 也适合解决复杂网络环境下的通信问题。

## MQTT over QUIC 的应用场景

MQTT over QUIC 特别适用于对实时性和稳定性要求较高的业务。例如，在山区、矿场和隧道中行驶的联网车辆，当进入信号死角或被动切换基站时，连接会中断。MQTT over QUIC 借助 QUIC 的优势，能够克服传统 MQTT over TCP 在以下场景中的缺点：

- TCP/TLS 的连接建立缓慢：客户端和服务器之间的初始握手需要多次往返以建立连接。往返时间（RTT）对于连接建立速度至关重要。较长的 RTT 可导致延迟增加和连接建立缓慢。
- 由于拥塞窗口使用慢启动，导致流量缓慢增加。
- 队头阻塞：当一个数据包丢失时，整个传输会被阻塞，直到它被恢复，这会显著增加延迟。
- 无法感知上层协议：TCP 对所有数据传输都做同样的处理，无法区分使用同一网络连接的不同类型数据或业务。

## QUIC vs TCP/TLS 测试对比

在与 TCP/TLS 测试的对比中，MQTT over QUIC 的表现如下：

1. 网络时延较高时，QUIC 能够更快的建立连接、完成订阅。
2. 断开连接后，重新发起连接并恢复重连 QUIC 所需的时延更低。
3. 大规模连接/重连时，对于服务器 CPU 和内存使用 QUIC 均优于 TLS。
4. 网络切换时（客户端源 IP 地址/端口变化），TCP/TLS 下客户端重连响应非常慢并出现消息传输中断现象，而 QUIC 的处理则更加平顺，消息发送无任何影响。
5. 在弱网丢包、包传输乱序环境下，TLS 出现因网络环境差而导致的消息拥塞与丢失，而 QUIC 服务端接收的数据稍微有所抖动，但不丢失消息。

## 应用限制

目前 MQTT over QUIC 在 EMQX 的应用还有以下局限：

- 目前不支持保留会话状态。这意味着如果客户端需要重新连接，则必须通过数据流重新订阅先前订阅的主题。
- 如果数据流被任一方意外关闭，则不会保留 QoS 1 和 QoS 2 消息状态。

## 未来规划

目前 MQTT over QUIC 已经具备投入生产能力，已有用户开始进行深度测试集成并获得了良好反馈，如需体验，请参考[快速开始](./getting-started.md)。

然而，EMQX 还没有利用 QUIC 提供的所有功能，如流优先级、流量控制和不可靠的数据报文，这些功能将在以后的版本中得到解决，并希望能成为 OASIS 标准。

此外，需要进一步解决如何保留消息状态和在不重新连接的情况下恢复订阅这两种在[应用限制](#应用限制)中提到的局限。
