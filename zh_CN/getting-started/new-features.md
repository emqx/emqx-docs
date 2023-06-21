# 全新功能

本章节描述了 EMQX 5.1 及 EMQX 5.0 引入的全新功能。

{% emqxee %}

## 热升级

从 EMQX 企业版 5.1.0 开始，EMQX 支持了热升级功能，使您能够在不中断服务的情况下升级到较新的 EMQX 版本。

这意味着您可以平滑地将 EMQX 更新为更高版本，同时保持您的应用程序持续运行，无需停机时间。这一功能提供了更高的灵活性和可靠性，以满足您对系统升级的需求。

{% endemqxee %}

## 基于 MQTT 的文件传输

EMQX 5.1.0 引入了 MQTT 文件传输功能，支持通过 MQTT 协议传输文件。

该功能基于标准的 MQTT 协议扩展实现，无需改造现有的客户端与应用即可进行集成。客户端可以通过 MQTT 协议向特定主题传输文件分片，传输完成后服务端会对分片进行合并，并保存到本地磁盘或导出到兼容 S3 协议的对象存储中。

相比于 HTTP/FTP 协议，MQTT 具有低带宽消耗和资源占用少的特点，能够快速且高效的进行文件传输。统一的物联网数据通道也简化了系统架构，减少应用的复杂性和维护成本。

立即开始使用[基于 MQTT 的文件传输](../file-transfer/introduction.md)。

## 备份与恢复

EMQX 5.1.0 新增了一组用于备份与恢复的命令行工具，能够将内置数据库中的数据以及配置文件导出为一个压缩包，也能够将备份的数据和配置文件恢复到新的集群中。

创建备份：

```bash
$ ./bin/emqx ctl data export
...
Data has been successfully exported to data/backup/emqx-export-2023-06-21-14-07-31.592.tar.gz.
```

恢复备份：

```bash
./bin/emqx ctl data import <File>
```

尝试[备份与恢复](../operations/backup-restore.md)。

## CRL/OCPP Stapling

{% emqxce %}

从 5.0.22 版本开始，EMQX 支持针对 MQTT SSL 监听器设置 CRL/COSP Stapling 检查功能。

{% endemqxce %}

{% emqxee %}

从 5.0.3 版本开始，EMQX 支持针对 MQTT SSL 监听器设置 CRL/COSP Stapling 功能。

{% endemqxee %}

此前版本的 EMQX 提供了 SSL/TLS 支持，用户可以使用 X.509 证书实现客户端接入认证与通信安全加密。

持有数字证书的物联网设备，如果出现私钥泄漏、证书信息有误的情况，或者设备需要永久销毁时，需要吊销对应证书以确保不被非法利用，CRL 与 OCSP Stapling 就是解决这一问题的关键。

通过 CRL 与 OCSP Stapling 功能，您可以控制每一张证书的有效性，及时吊销非法客户端证书，为您的物联网应用提供灵活且高级别的安全保障。

立即开始使用 [CRL](../network/crl.md) 与 [OCSP Stapling](../network/ocsp.md)。

{% emqxee %}

## 节点疏散与集群负载重平衡

从 5.0.4 版本开始，EMQX 支持节点疏散与集群负载重平衡。

该功能允许用户在集群负载不平衡时重新分配每个节点的连接，亦或是因维护关闭节点之前强制将连接和会话迁移到其他节点，以避免因此带来的会话数据丢失。

节点疏散与集群负载重平衡为 EMQX 提供了灵活可控的运维实践，能够大大降低 EMQX 负载状态不均衡以及维护工作对业务的影响。

立即开始使用[节点疏散与集群负载重平衡](../deploy/cluster/rebalancing.md)。

{% endemqxee %}

## Mria 集群架构

支持全新的 Mria 集群架构，在此架构下 EMQX 水平扩展性得到指数级提升，单个集群可以轻松支持 [1 亿 MQTT 连接](https://www.emqx.com/zh/blog/reaching-100m-mqtt-connections-with-emqx-5-0)，这使得 EMQX 5.0 成为目前全球最具扩展性的 MQTT Broker。

<img src="./assets/100m-benckmark.png" alt="100M benchmark" style="zoom:33%;" />

在构建满足用户业务需求的更大规模集群的同时，Mria 架构还能够降低大规模部署下的脑裂风险以及脑裂后的影响，以提供更加稳定可靠的物联网数据接入服务。

立即开始[创建与管理集群](../deploy/cluster/create-cluster.md)。

## MQTT over QUIC 支持

以实验性功能将 QUIC 作为 MQTT 传输层，并设计了独特的消息传输机制和管理方式。

QUIC 非常适用于传统 TCP/IP 网络 UDP MTU 大小能够保证的弱网环境或者网络经常切换的环境。对于设备时刻处在移动中的物联网场景（如车联网、移动采集等），或是需要频繁断连不适合做长连接的场景（如设备需要定期休眠）来说，QUIC 都拥有巨大的潜力，是更为适合的底层协议选择。

立即开始[使用 MQTT over QUIC](../mqtt-over-quic/getting-started.md)。

## 全新物联网数据集成

规则引擎在原有 SQL 的基础上集成了 [jq](https://stedolan.github.io/jq/)，支持更多复杂格式 JSON 数据的处理。

{% emqxce %}

支持将数据发送到 Webhook，或与外部 MQTT 服务建立双向数据桥接。

{% endemqxce %}

{% emqxee %}

支持双向数据桥接，您可以实时地将物联网数据实时处理并发送到 40 多个云服务和企业系统，或者从其中获取数据，经处理后下发到指定 MQTT 主题中。

{% endemqxee %}

同时，EMQX 5.0 还提供了数据集成可视化查看能力（Flows）。通过 Dashboard 页面，您可以清晰看到设备与云端之间的物联网数据处理和流转步骤。

后续版本 EMQX 还将支持在 Dashboard 上以拖拽的方式编排规则和数据桥接（Flow Editor），通过可视化操作实现数据集成配置。

![Flow Editor：通过可视化编排规则处理数据流](./assets/flow-editor.png)

关于 EMQX 支持的桥接类型以及如何配置，可阅读[数据桥接章节](../data-integration/data-bridges.md)。

## 灵活多样认证授权

改进了认证授权流程，并提供了灵活的使用与配置方式。通过简单配置，无需编写代码即可对接各类数据源与认证服务，为您的物联网应用开启安全防护。

EMQX 5.0 认证授权包括以下特性：

- 支持在 Dashboard 完成整个集群的认证授权配置。
- 支持通过 Dashboard 管理认证凭证与授权数据。
- 支持调整认证器与授权检查器顺序。
- 提供执行速度与次数统计指标，实现认证授权可观测性。
- 允许监听器单独配置认证，更灵活的接入能力。

关于如何在 EMQX 进行认证和授权配置，可阅读[访问控制](../access-control/overview.md)章节。

## 全新 EMQX Dashboard

EMQX 5.0 带来了全新 UI 设计风格的 EMQX Dashboard，在提升视觉体验的同时，也极大提升了 EMQX 的易用性。

全新 Dashboard 包括以下更新：

- 新的 UI/UX 设计，丰富的样式与易于上手的使用交互。
- 优化菜单结构，快速直达访问内容。
- 更丰富的可视化系统，数据和状态一目了然。
- 开箱即用的认证授权配置与管理功能。
- 强大数据集成能力，Flow 可视化编排与双向数据桥接。
- 在线配置更新，在 Dashboard 上实现配置热更新。

## 云原生与 EMQX Operator

[EMQX Kubernetes Operator](https://www.emqx.com/zh/emqx-kubernetes-operator) 同步升级支持了 EMQX 5.0。

EMQX Kubernetes Operator 使部署和管理工作变成一种低成本、标准化、可重复性的能力，帮助您高效实现集群扩容、无缝升级、故障处理和统一监控。

了解 [EMQX Operator](https://www.emqx.com/zh/emqx-kubernetes-operator)。

## 全新网关框架

网关能够实现多协议的接入和设备管理，EMQX 5.0 重构了多协议接入的底层架构，使得各个网关功能定义更为清晰：

- 独立的统计指标，每个网关有自己的收发字节数、消息等指标。
- 独立的连接和会话管理能力，能够查看和管理每种协议客户端自有的属性。
- 独立的客户端认证，支持为每个网关配置独立的认证方式。
- 更简单的协议扩展机制，通过标准的概念和接口使扩展其他协议变得更加容易。

EMQX 5.0 网关实现多种协议的接入和统一管理，MQTT 之外的协议也能够充分享受 EMQX 强大的数据集成、安全可靠的认证授权，以及极高的水平扩展能力等诸多特性。

## 更多功能更新

- **全新极简配置**：配置文件更换为简洁易读的 HOCON 格式配置文件，`emqx.conf` 配置文件默认仅包含常用的配置项，以提高配置文件可读性和可维护性。
- **改进的 REST API**：提供符合 OpenAPI 3.0 规范的 REST API，以及清晰且丰富的 API 文档，为您带来更好的使用体验。
- **快速排查故障**：提供更多的诊断工具如慢订阅、在线追踪帮助用户快速排查生产环境中的问题。
- **结构化日志**：提供更友好的结构化日志以及 JSON 格式日志支持，错误日志包含唯一的 `msg` 方便定位问题原因。
- **更灵活的拓展机制**：引入全新的插件架构，能够以独立插件包的形式编译、分发、安装拓展插件；支持配置多个多语言钩子扩展 exhook，并提供完善的状态与指标监控。
