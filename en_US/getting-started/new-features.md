# New Features

EMQX 5.0 is a huge accomplishment for MQTT technology. The latest version has been verified in [test scenarios](https://www.emqx.com/en/blog/reaching-100m-mqtt-connections-with-emqx-5-0) to scale to 100 million concurrent device connections, which is a critically important milestone for IoT designers. It also comes with plenty of exciting new features and huge performance improvements, including a more powerful rule engine, enhanced security management, Mria database extension, and much more to enhance the scalability of IoT applications.

## Significant improvements in scalability and reliability

The latest version adopts a new Mria extension for Erlang’s [Mnesia database](https://github.com/erlang/otp/pull/5926) that increases horizontal scalability by defining two different node types: core nodes and replicant nodes. This new architecture allows EMQX 5.0 to better adapt to increasing demand in IoT networks. 

The latest performance testing shows it can easily support 100M connections with a single cluster—a 10-fold increase over previous versions—making it the world’s most scalable open-source MQTT broker.

![100m-benckmark](assets/100m-benckmark.png)

## The world’s first implementation of MQTT over QUIC

QUIC is a underlying transfer protocol for the next-generation HTTP/3 protocol used by modern web browsers. QUIC benefits IoT transmission scenarios by reducing connection overhead and latency compared to TCP, increasing overall throughput, and increasing stability for mobile connections.

With support for QUIC, EMQ hopes to maintain EMQX’s ability to provide the most advanced and competitive MQTT servers for the next generation of internet connectivity.

## New IoT Data Integration: Flow Editor & Bidirectional Data Bridging

EMQX is designed for high-performance, real-time data processing and integration for IoT platforms and applications. EMQX 5.0 is deeply refactored and optimized to make data integration easier and more flexible.

EMQX 5.0 integrates Webhook and data storage/bridging plug-ins together to manage both northbound and southbound data flow in a unified interface. Based on the same rule processing functions, users can also process southbound messages from the cloud to edge devices.

Also, EMQX 5.0 provides data integration visualization capabilities (Flows). Through the Dashboard, users can clearly see how IoT data is processed by the rules engine and how data flows to external data services or devices.

Subsequent versions will also support drag-and-drop orchestration of rules and data bridges (Flow Editor) on the Dashboard to easily connect IoT hardware data flows together through a visual interface.

![flow-editor](assets/flow-editor.png)

## 灵活多样认证授权，零开发投入保障 IoT 安全

EMQX 5.0  对认证授权的配置方式和使用流程进行了优化，内置实现了客户端认证授权功能：用户通过简单配置，无需编写代码即可对接各类数据源与认证服务，实现各个级别与各类场景下的安全配置，以更高的开发效率获得更安全的保障。

**认证授权的创新包括：**

- 在 Dashboard 上完成配置、调试与管理。
- 支持调整认证器与授权检查器顺序。
- 提供执行速度与次数指标统计，实现认证授权可观测性。
- 允许监听器单独配置认证，更灵活的接入能力。



## 易操作、可观测的 MQTT Dashboard

EMQX 5.0 带来了全新 UI 设计风格的 EMQX Dashboard。优化了关键数据和指标数据的显示方式与内容，在提升视觉体验的同时，也提供了更全面、强大、易用的内置功能，如对于连接、订阅和发布时的认证与权限管理，支持使用数据桥接并搭配规则引擎进行数据集成转化等。

**全新 Dashboard 包括以下更新**：

- 全新 UI / UX 设计：实时可观测性大幅提升

- 菜单结构优化：快速直达访问内容

- 数据监控与管理：重要数据一目了然

- 可视化管理访问控制：开箱即用的认证授权管理

- 强大数据集成能力：Flow 可视化编排与双向数据桥接

- 在线配置更新：保存即刻生效的配置热更新

- 自定义扩展能力：内置网关、插件和 Hooks

- 更加全面的诊断工具：及时发现问题并解决

## EMQX Operator  - 拥抱云原生的 EMQX 5.0

对于一个云原生应用来说，水平扩展和弹性集群是其应具备的重要特性。

现在，您可以通过 EMQ 发布的管理工具 [EMQX Kubernetes Operator](https://www.emqx.com/zh/emqx-kubernetes-operator)，利用 EMQX 5.0 的 Replicant 节点特性，在 Kubernetes 上通过 Deployment 资源实现无状态节点的部署，快速创建并管理可以承载大规模 MQTT 连接和消息吞吐的 EMQX 集群。

EMQX Kubernetes Operator 使部署和管理工作变成一种低成本、标准化、可重复性的能力，帮助用户高效实现集群扩容、无缝升级、故障处理和统一监控。

![Kubernetes MQTT](assets/emqx-operator-3935269.png)

## 全新网关框架：轻松实现多物联网协议接入

EMQX 5.0 中，我们重构了多协议接入的底层架构，统一了配置格式和管理接口，提供了一个全新的扩展网关框架。同时规范了各类网关的实现，使得各个网关功能定义更为清晰。

现在，所有网关由一个统一的框架提供通用操作的支持，包括：

- **统一的用户层接口：**该框架提供了风格统一的配置文件、HTTP API 和命令行接口。以监听器参数配置为例，4.x 版本中不同协议插件对于监听器暴露的参数各不相同，而在 5.0 版本中这些参数的风格都将是统一的。
- **统一的统计和监控指标：**提供了网关和客户端级别的统计指标，例如收发字节数、消息等。
- **独立的连接和会话管理：**每个网关都有在自己的客户端管理页面，且不同的网关允许使用相同的 Client ID ，而不是像 4.x 版本一样都混合在 MQTT 客户端列表中进行管理。
- **独立的客户端认证：**支持为每个网关配置独立的认证，不再像 4.x 像一样与 MQTT 客户端认证混合在一起。
- **易扩展和规格清晰化：**框架抽象了一套标准的概念和接口使自定义网关变得更加容易。

网关框架实现多种协议的接入和统一管理，进一步提升了 EMQX 的易用性，第三方协议也能够充分享受 EMQX 强大的数据集成、安全可靠的认证授权，以及亿级的水平扩展能力等诸多优势功能特性。

## **更多功能更新**

**More Intuitive User Experience: **

Ease-of-use improvements will be the most obvious change to users of EMQX 5.0. The Dashboard, with its new improved rule engine and action management UI/UX design, makes it easier to access the most frequently used functions according to users' roles. The concise and easy-to-read HOCON configuration file format, the OpenAPI 3.0 compliant REST API documents, more detailed monitoring metrics, log tracking, and slow subscription diagnostic tools will also bring developers a better experience.

**可操作性与可观测性大幅提升** 

在 Dashboard 上提供长达 7 天的详细监控指标，可以一键集成 Prometheus 与 Grafana、Datadog/StatsD 等指标监控告警系统。提供更多的诊断工具如慢订阅、在线追踪帮助用户快速排查生产环境中的问题，提供更友好的结构化日志以及 JSON 格式日志支持；

**更灵活的拓展定制方式**

 引入全新的插件架构，用户可用独立插件包的形式编译、分发、安装自己的拓展插件对 EMQX 进行自定义扩展；
