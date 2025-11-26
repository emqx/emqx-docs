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
ref: undefined
---

# Installation

This chapter provides a comprehensive guide for installing and configuring EMQX. It includes the following key topics:

- Installation: Step-by-step instructions for installing EMQX on supported operating systems. For details on the supported operating systems, see [Supported operating systems](#supported-operating-systems)
- [Configure license](./license.md): Instructions on how to start EMQX after the installation and how to configure the license.
- [Directory](./directory.md): Overview of important file and directory locations for future configuration and maintenance tasks.
- [Basic commands](./command-line.md): Essential commands for working with EMQX and managing its operations.
- [Hot upgrade](../advanced/relup.md): By using the hot upgrade feature, users can quickly and safely upgrade the EMQX in the production environment, and avoid the decrease in system availability caused by restarting the service.
- [Hot patches](../advanced/patches.md): EMQX provides an ad-hoc patch mechanism to address critical issues promptly, even before they are officially released in future updates. the fix is included in a (future) official release.
- [Upgrade from 4.2](../changes/upgrade-4.3.md): Guidance on how to upgrade from v4.2

## Download

EMQX releases the corresponding Docker image and the installation packages for different operating systems or platforms in each release. You may click the link below to download.

EMQX website: https://www.emqx.com/en/try?product=enterprise

:::tip TIP

Besides the above deployment methods, you are also welcome to try our [EMQX Cloud](https://www.emqx.com/en/cloud), a fully managed MQTT service for IoT. You only need to [register for an account](https://www.emqx.com/en/signup?continue=https://www.emqx.com/en/cloud) before starting your MQTT services and connecting your IoT devices to any cloud with zero need for infrastructure maintenance.

:::

## Supported operating systems

EMQX Enterprise binary packages are released on below operating systems:

| Operating System      | Versions Supported                              | x86_64/amd64 | arm64 |
| --------------------- | ----------------------------------------------- | ------------ | ----- |
| [Debian](./debian.md) | Debian 9<br>Debian 10                           | Yes          | Yes   |
| [CentOS](./centos.md) | CentOS 6<br>CentOS 7 <br>CentOS 8               | Yes          | Yes   |
| [Ubuntu](./ubuntu.md) | Ubuntu 16.04  <br>Ubuntu 18.04 <br>Ubuntu 20.04 | Yes          | Yes   |
| [macOS](./macos.md)   |                                                 | Yes          | No    |



