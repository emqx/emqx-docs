# 快速开始

本章节将指导您从下载安装开始，快速开始使用 EMQX。

## 版本选择

{% emqxce %}
EMQX 有如下版本：

- [开源版](https://www.emqx.com/zh/try?product=broker)
- [企业版](https://www.emqx.com/zh/try?product=enterprise)

{% endemqxce %}

除了私有部署外，EMQ 也提供了全托管的 MQTT 云服务 EMQX Cloud，您可以选择合适您的部署方式，快速开始使用。

:::: tabs type:card

{% emqxce %}
::: tab EMQX 开源版
大规模可弹性伸缩的云原生分布式物联网 MQTT 消息服务器，高效可靠连接海量物联网设备，高性能实时处理消息与事件流数据，助力构建关键业务的物联网平台与应用。

- 基于 APL 2.0 开放源码协议
- 完整 MQTT 3.1.0、3.1.1 和 5.0 规范，支持 MQTT-SN
- Masterless 高可用集群架构
- 高并发、低时延、高性能
- 可扩展的网关和插件体系

[下载安装](https://www.emqx.io/zh/downloads)
:::
{% endemqxce %}

::: tab EMQX Cloud
通过可靠、实时的物联网数据移动、处理和集成，连接您的海量物联网设备。加快您的物联网应用开发，免除基础设施管理维护负担。

- 全托管的 MQTT 5.0 服务
- 基于 SQL 的 IoT 数据集成
- 多种数据库与云服务集成
- 关键业务的高可用高可靠
- 在任何地方运行，随用随付

[免费试用](https://www.emqx.com/zh/try?product=cloud)
:::

::: tab EMQX 企业版

**「随处运行，无限连接，任意集成」** 云原生分布式物联网接入平台，一体化的分布式 MQTT 消息服务和强大的 IoT 规则引擎，为高可靠、高性能的物联网实时数据移动、处理和集成提供动力，助力企业快速构建关键业务的 IoT 平台与应用。

- 标准或专有多协议支持
- 基于 SQL 的 IoT 数据集成
- 数据持久化与数据桥接
- 管理与监控中心
- 7x24 小时技术支持服务

[**免费试用**](https://www.emqx.com/zh/try?product=enterprise)
:::

::::

## 安装 EMQX

### 在 EMQX Cloud 中运行

EMQX Cloud 是全球首个全托管的 MQTT 5.0 公有云服务。在 [EMQX Cloud](https://www.emqx.com/zh/cloud) 支持下，您可以在云上创建 EMQX 集群并使用 EMQX 企业版全部功能。这使您可以将更多的时间花费在业务对接上，而将较少的时间用于 EMQX 的运维和管理。

- [创建并登录 EMQX Cloud 账户](https://docs.emqx.com/zh/cloud/latest/quick_start/introduction.html#创建和登录-emq-x-cloud-账户)
- [创建免费试用部署](https://docs.emqx.com/zh/cloud/latest/quick_start/create_free_trial.html)

### 通过 Docker 容器运行

EMQX 提供了一个容器镜像，您可以在 [Docker Hub](https://hub.docker.com/r/emqx/emqx)上了解该镜像的详细信息。通过容器化部署是快速开始体验 EMQX 的最快方式。

启动 Docker 容器：

```bash
docker run -d --name emqx -p 1883:1883 -p 8083:8083 -p 8084:8084 -p 8883:8883 -p 18083:18083 emqx/emqx:latest
```

更多 Docker 安装、Docker Compose 快速搭建集群集群请参考[通过 Docker 运行 (包含简单的 docker-compose 集群)](./deploy/install.md#通过-docker-运行-包含简单的-docker-compose-集群)。

### Kubernetes 安装部署
<!-- TODO @wivwiv Update K8s link when EMQX Operator 5.0 document ready -->
对于使用 Kubernetes 的用户，EMQX 提供了 [EMQX Kubernetes Operator](https://www.emqx.com/zh/emqx-kubernetes-operator)。该 Operator 可以帮助您在 Kubernetes 环境下快速部署一个可用于生产环境的，功能完备的 EMQX 集群。

EMQX Kubernetes Operator 是基于 Kubernetes 原生 API 的应用编排工具，用于 EMQX 集群的自动化部署和生命周期管理。您可以查阅[文档](https://github.com/emqx/emqx-operator/blob/main/docs/zh_CN/getting-started/getting-started.md)来了解如何使用 Operator 部署 EMQX。

### Terraform 安装部署
<!-- TODO @wivwiv Update K8s link when EMQX Terraform 5.0 document ready -->
通过 Terraform 在主流公有云上一键部署包含 EMQX Enterprise 集群在内的所有基础设施。在公有云上部署 EMQX Enterprise 集群最快捷的方式：

- [在阿里云上部署](https://github.com/emqx/tf-alicloud)

- [在 AWS 上部署](https://github.com/emqx/tf-aws)

更多有关 Terraform 安装部署的信息请参考 [EMQX Terraform](https://www.emqx.com/zh/emqx-terraform)。

### 在虚拟机或物理机中运行

EMQX 可以直接部署在物理服务器或者虚拟机上。最小仅需 2 核 4G 的机器即可运行 EMQX 程序。可支持 CentOS、Debian、Ubuntu、macOS 等操作系统。

- [RedHat、CentOS、 RockyLinux、AmazonLinux 系统安装](./deploy/install.md#centos)
- [Ubuntu、Debian 安装](./deploy/install.md#ubuntu、debian)
- [MacOS、Windows、Linux tgz 包安装](./deploy/install.md#tgz-压缩包安装)

如果您需要 FreeBSD、国产硬件平台以及操作系统适配（如麒麟、深度、红旗等）或其他 Linux 发行版安装包，可参考 [源码编译安装](./deploy/install.md#源码编译安装) 或 [联系我们](https://www.emqx.com/zh/contact) 获取支持。

## 启动 EMQX

安装成功后，可通过 `systemctl` 或 `emqx` 命令来启动 EMQX。

EMQX 成功启动之后可以通过浏览器打开 [http://localhost:18083/](http://localhost:18083/)（将 localhost 替换为您实际 IP 地址）以访问 [EMQX Dashboard](./dashboard/introduction.md) 管理控制台，进行设备连接与相关指标监控管理。

### 后台启动 EMQX

```bash
emqx start
```

启动成功后可以使用 `emqx ping` 命令检测节点运行状态，返回 `pong` 则表示正常运行：

```bash
emqx ping
```

### systemctl 启动

```bash
sudo systemctl start emqx
```

检查服务是否正常工作：

```bash
sudo systemctl status emqx
```

### tgz 安装包启动

切换到 EMQX 解压目录，执行以下命令启动 EMQX：

```bash
./bin/emqx start
```

开发模式下可以使用 `console` 命令在控制台启动 EMQX，该模式可以实时查看 EMQX 启动和运行输出日志信息：

```bash
./bin/emqx console
```

## 使用 MQTT 客户端快速验证

EMQX 提供了标准的 MQTT 协议支持，启动后即可接入 MQTT 客户端，您可以使用以下客户端工具或客户端库接入 EMQX 进行消息通信以完成某些场景或功能的测试验证。

### Dashboard Websocket 工具

打开 Dashboard，进入 **问题分析 -> WebSocket 客户端** 页面中可以在浏览器中使用 MQTT over WebSokcet 客户端快速接入 EMQX。

WebSocket 客户端页面为您提供了一个简易但有效的 MQTT 测试工具，它包含了连接、订阅和发布功能，同时还能查看自己发送和接收的消息数据。

<!-- TODO @wivwiv Update screenshot -->

### MQTT X 桌面客户端工具

MQTTX 是一款优雅的跨平台 MQTT 5.0 开源桌面客户端工具，支持在 macOS、Linux 和 Windows 上运行。

MQTTX 有诸多特性，提供了简洁的图形界面和操作逻辑，支持 MQTT/MQTT over Websocket 接入以及单/双向 SSL 认证，同时支持 Payload 格式转换、自定义脚本模拟测试数据、 `$SYS` 系统主题自动订阅查看流量统计等诸多实用功能。

下载与使用可参考 [MQTTX 官网](https://mqttx.app/zh)。

![emqx-mqttx](./assets/emqx-mqttx.jpeg)

## EMQX 客户端库

以下是各个编程语言中热门 MQTT 客户端库介绍说明，各个库的连接（包含 TLS 连接）、发布、订阅、取消订阅基本功能代码示例。

### 客户端库介绍

- [MQTT C 客户端库](./development/c.md)
- [MQTT Java 客户端库](./development/java.md)
- [MQTT Go 客户端库](./development/go.md)
- [MQTT Erlang 客户端库](./development/erlang.md)
- [MQTT JavaScript 客户端库](./development/javascript.md)
- [MQTT Python 客户端库](./development/python.md)

### 客户端库项目工程代码示例

[MQTT-Client-Examples](https://github.com/emqx/MQTT-Client-Examples) 中提供了 MQTT 客户端库接入示例和工程项目代码示例：

- [Android](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-Android)
- [Csharp-MqttNet](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-Csharp-MqttNet)
- [ESP32](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-ESP32)
- [ESP8266](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-ESP8266)
- [Electron](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-Electron)
- [Flutter](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-Flutter)
- [Go](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-Go)
- [Java](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-Java)
- [PHP](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-PHP)
- [Qt](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-Qt)
- [SpringBoot](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-SpringBoot)
- [Vue.js](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-Vue.js)
- [swift](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-swift)

<!--
- [wechat-miniprogram](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-wechat-miniprogram)
!-->

## 进阶操作

<!-- TODO @wivwiv Update links after document is ready-->

完成基本的安装、启动、接入测试之后，您可以继续阅读以下操作文档进行进阶操作配置。

### 访问控制

访问控制与是大多数应用的重要组成部分，启用身份认证(Authentication)能有效阻止非法客户端的连接。授权(Authorization)可以对客户端发布/订阅操作进行精细的权限控制。

- [认证](./security/authn/authn.md)：支持用户名/密码认证、JWT 认证、MQTT 5.0 增强认证三种不同的认证方式，其他密码认证支持使用内置数据库、Redis、MySQL、PostgreSQL、MongoDB、HTTP Server 作为数据源。
- [授权](./security/authz/authz.md)：基于 Client ID、用户名或 IP 地址的访问控制，支持使用内置数据库、Redis、MySQL、PostgreSQL、MongoDB、HTTP Server 作为数据源。

### 数据集成

数据集成是 EMQX 在发布订阅模型的基础之上的数据处理与分发组件，通过简单的、可视化的配置，即可将消息流以及设备事件与 Kafka、RabbitMQ 等消息中间件，以及各类 SQL / NoSQL / 时序数据库等数据系统集成。

- [数据集成](./data-integration/introduction.md)。
- [规则](./data-integration/rules.md)。
- [数据桥接](./data-integration/data-bridges.md)。

### 管理接口

通过 Web 页面与 CLI、REST API 管理集群。

- [Dashboard](./dashboard/introduction.md)：Dashboard 使用手册。
- [CLI](./admin/cli.md)：通过 CLI 管理集群。
- [REST API](./admin/api.md)：符合 OpenAPI 3.0 规范的 REST API 文档。
- [配置文件](./admin/cfg.md)：配置文件与配置项文档。

### 运维部署

有关官方使用指南和最佳实践，请阅读以下指南。

- [系统调优](./deploy/tune.md)
- [生产部署](./deploy/install.md)
- [Prometheus 监控和警报](./observability/prometheus.md)
- [性能测试](./verif/benchmark.md)

### 常见问题解答

您可以访问 [EMQ 问答社区](https://askemq.com/) 参与交流，提出、解答 EMQX 以及 EMQ 相关产品使用问题，与 EMQX 用户交流物联网相关技术的使用经验。
