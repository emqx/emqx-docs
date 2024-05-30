# 在 CentOS/RHEL 上安装 EMQX 企业版

本页将指导您如何在 CentOS/RHEL 系统中下载安装并启动 EMQX。

支持的 CentOS/RHEL 版本：

- Amazon Linux 2023
- Amazon Linux 2
- Rocky Linux 9 (RHEL 9)
- Rocky Linux 8 (RHEL 8)
- CentOS 7 (RHEL 7)

下文将以 CentOS 8 系统为例演示如何下载最新版 EMQX。如希望在其他系统中进行安装或希望安装其他版本，可前往 [EMQX 下载页面](https://www.emqx.com/zh/try?product=enterprise) 获取详细安装信息。

## 通过 rpm 安装

1. 前往 [EMQX 官方下载页面](https://www.emqx.com/zh/try?product=enterprise&currentVersion=@EE_VERSION@&currentOS=CentOS=currentOS=Centos8&utm_source=docs.emqx.com&utm_medium=referral&utm_campaign=enterprise-docs-install-to-try-enterprise)。

2. 在**下载 EMQX Enterprise** 中，**版本**选择 `@EE_VERSION@`，**系统**选择 `CentOS`，点击**免费下载**按钮。

3. 在安装与下载页面中，**安装方式**选择 `rpm`，选择合适的 **CPU 架构**，点击**立即下载**。

   您也可以参照命令行指导步骤进行下载与安装。

### 启动 EMQX

启动为一个 systemd 服务：

```bash
sudo systemctl start emqx
```

### 卸载 EMQX

服务完成后，可通过如下命令卸载 EMQX：

```shell
sudo yum remove emqx
```

## 通过 tar.gz 安装


1. 前往 [EMQX 官方下载页面](https://www.emqx.com/zh/try?product=enterprise&currentVersion=@EE_VERSION@&currentOS=CentOS=currentOS=Centos8&utm_source=docs.emqx.com&utm_medium=referral&utm_campaign=enterprise-docs-install-to-try-enterprise)。

2. 在**下载 EMQX Enterprise** 中，**版本**选择 `@EE_VERSION@`，**系统**选择 `CentOS`，点击**免费下载**按钮。

3. 在安装与下载页面中，**安装方式**选择 `tar.gz`，选择合适的 **CPU 架构**，点击**立即下载**。

   您也可以参照命令行指导步骤进行下载与安装。

### 启动 EMQX

安装完成后，可通过如下命令启动 EMQX。

```
./emqx/bin/emqx foreground
```
