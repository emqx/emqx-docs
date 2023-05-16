# 安装

本章主要介绍如何在各个平台和系统中安装 EMQX，主要涉及以下主题：

- 安装：逐步介绍如何在各个支持的操作系统中安装 EMQX。具体可阅读[支持的操作系统与平台](#支持的操作系统与平台)。

- [启动 EMQX 并配置 License 文件](./start.md)：介绍安装后如何启动EMQX以及如何配置许可证。
- [EMQX 目录结构](./directory.md)：概述重要的文件和目录位置，方便未来的配置和维护任务。
- [基本命令](./command-line.md)：介绍 EMQX 基本操作命令，如启动、停止、重启等。
- [配置说明](./config.md)：整体介绍如何通过配置项修改 EMQX 配置，以便更好地满足业务需求。 
- [热升级](../modules/hot_confs.md)：使用热配置功能，可以在运行时通过 Dashboard 修改多数 EMQX 的配置项。
- [版本热升级](../advanced//relup.md)：通过热升级功能，用户可以在生产环境中快速且安全地升级 EMQX，避免因重启而影响系统可用性。
- [安装补丁包](../advanced/patches.md)：EMQX 支持通过补丁包进行临时修补，以便及时解决关键问题。
- [从 v4.2 升级](../changes/upgrade-4.3.md)：介绍如何从 4.2 版本升级到 4.3 版本。

## EMQX 程序包下载

EMQX 消息服务器每个版本会发布 CentOS、Ubuntu、Debian 平台程序包与 Docker 镜像。

下载地址: <https://www.emqx.com/zh/downloads>

## 支持的操作系统与平台

EMQX 可以跨平台的在多种操作系统和硬件平台上运行，具体参见下表：

| 操作系统                                                     | 支持版本                                        | x86_64/amd64 | arm64 |
| ------------------------------------------------------------ | ----------------------------------------------- | ------------ | ----- |
| [Debian](https://github.com/emqx/emqx-docs/blob/release-5.0/zh_CN/deploy/install-debian.md) | Debian 9<br>Debian 10                           | 是           | 是    |
| [CentOS](./centos.md)                                        | CentOS 6<br>CentOS 7 <br>CentOS 8               | 是           | 是    |
| [Ubuntu](https://github.com/emqx/emqx-docs/blob/release-5.0/zh_CN/deploy/install-ubuntu.md) | Ubuntu 16.04  <br>Ubuntu 18.04 <br>Ubuntu 20.04 | 是           | 是    |
| [macOS](https://github.com/emqx/emqx-docs/blob/release-5.0/zh_CN/deploy/install-macOS.md) |                                                 | 是           | 否    |
