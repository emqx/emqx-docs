# EMQX 企业版安装

EMQX 消息服务器可跨平台运行在 Linux 服务器上。

## EMQX 程序包下载

EMQX 消息服务器每个版本会发布 CentOS、Ubuntu、Debian 平台程序包与 Docker 镜像。

下载地址: <https://www.emqx.com/zh/downloads>

## 支持的操作系统与平台

EMQX 可以跨平台的在多种操作系统和硬件平台上运行，具体参见下表：

| 操作系统                                                     | 支持版本                                        | x86_64/amd64 | arm64 |
| ------------------------------------------------------------ | ----------------------------------------------- | ------------ | ----- |
| [CentOS](./centos.md)                                        | CentOS 6<br>CentOS 7 <br>CentOS 8               | 是           | 是    |
| [Ubuntu](https://github.com/emqx/emqx-docs/blob/release-5.0/zh_CN/deploy/install-ubuntu.md) | Ubuntu 16.04  <br>Ubuntu 18.04 <br>Ubuntu 20.04 | 是           | 是    |
| [Debian](https://github.com/emqx/emqx-docs/blob/release-5.0/zh_CN/deploy/install-debian.md) | Debian 9<br>Debian 10                           | 是           | 是    |
| [macOS](https://github.com/emqx/emqx-docs/blob/release-5.0/zh_CN/deploy/install-macOS.md) |                                                 | 是           | 否    |

除 EMQX 在操作系统改的安装方式外，您还可在本章了解以下主题：

- [启动 EMQX 并配置 License 文件](./start.md)
- [EMQX 目录结构](./directory.md)
- [通过命令行对 EMQX 进行操作](./command-line.md)
- [配置说明](./config.md)
- [热配置](../modules/hot_confs.md)
- [版本热升级](../advanced//relup.md)
- [安装补丁包](../advanced/patches.md)
- [升级指南](../changes/upgrade-4.3.md)
