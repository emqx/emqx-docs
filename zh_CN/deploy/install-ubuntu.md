# 在 Ubuntu 上安装 EMQX 企业版

本页将指导您如何在 Ubuntu 系统中下载安装并启动 EMQX。

支持的 Ubuntu 版本：

- Ubuntu 22.04
- Ubuntu 20.04
- Ubuntu 18.04

下文将以 Ubuntu 22.04 系统为例演示如何下载最新版 EMQX。如希望在其他支持系统中进行安装，或体验其他版本，请可前往 [EMQX 下载页面](https://www.emqx.com/zh/try?product=enterprise) 获取安装信息。

## 通过 deb 安装

1. 前往 [EMQX 官方下载页面](https://www.emqx.com/zh/try?product=enterprise&currentVersion=@EE_VERSION@&currentOS=Ubuntu=currentOS=Ubuntu22&utm_source=docs.emqx.com&utm_medium=referral&utm_campaign=enterprise-docs-install-to-try-enterprise)。

2. 在**下载 EMQX Enterprise** 中，**版本**选择 `@EE_VERSION@`，**系统**选择 `Ubuntu`，点击**免费下载**按钮。

3. 在安装与下载页面中，**安装方式**选择 `deb`，选择合适的 **CPU 架构**，点击**立即下载**。

   您也可以参照命令行指导步骤进行下载与安装。

### 启动 EMQX

启动为一个 systemd 服务：

```bash
sudo systemctl start emqx
```

### 卸载 EMQX

服务完成后，可通过如下命令卸载 EMQX：

```shell
sudo apt remove --purge emqx
```

## 通过 tar.gz 安装

1. 前往 [EMQX 官方下载页面](https://www.emqx.com/zh/try?product=enterprise&currentVersion=@EE_VERSION@&currentOS=Ubuntu=currentOS=Ubuntu22&utm_source=docs.emqx.com&utm_medium=referral&utm_campaign=enterprise-docs-install-to-try-enterprise)。

2. 在**下载 EMQX Enterprise** 中，**版本**选择 `@EE_VERSION@`，**系统**选择 `Ubuntu`，点击**免费下载**按钮。

3. 在安装与下载页面中，**安装方式**选择 `tar.gz`，选择合适的 **CPU 架构**，按照提示进行下载与安装。

安装完成后，可通过如下命令启动 EMQX。

```bash
./emqx/bin/emqx foreground
```
