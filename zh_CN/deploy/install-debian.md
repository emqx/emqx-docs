# 在 Debian 上安装 EMQX 企业版

本页将指导您如何在 Debian 系统中下载安装并启动 EMQX。

支持的 Debian 版本：

- Debian 12
- Debian 11
- Debian 10

## 通过 deb 安装

1. 前往 [EMQX 官方下载页面](https://www.emqx.com/zh/try?product=enterprise&currentVersion=@EE_VERSION@&currentOS=Debian=currentOS=Debian12&utm_source=docs.emqx.com&utm_medium=referral&utm_campaign=enterprise-docs-install-to-try-enterprise)。

2. 在**下载 EMQX Enterprise** 中，**版本**选择 `@EE_VERSION@`，**系统**选择 `Debian`，点击**免费下载**按钮。

3. 在安装与下载页面中，**安装方式**选择 `deb`，选择合适的 **CPU 架构**，点击**立即下载**。

   您也可以参照命令行指导步骤进行下载与安装。

### 启动 EMQX 

systemctl 启动：

```bash
sudo systemctl start emqx
```

### 卸载 EMQX

服务完成后，可通过如下命令卸载 EMQX：

```shell
sudo apt remove --purge emqx
```

## 通过 tar.gz 安装

1. 前往 [EMQX 官方下载页面](https://www.emqx.com/zh/try?product=enterprise&currentVersion=@EE_VERSION@&currentOS=Debian=currentOS=Debian12&utm_source=docs.emqx.com&utm_medium=referral&utm_campaign=enterprise-docs-install-to-try-enterprise)。

2. 在**下载 EMQX Enterprise** 中，**版本**选择 `@EE_VERSION@`，**系统**选择 `Debian`，点击**免费下载**按钮。

3. 在安装与下载页面中，**安装方式**选择 `tar.gz`，选择合适的 **CPU 架构**，点击**立即下载**。

   您也可以参照命令行指导步骤进行下载与安装。

安装完成后，可通过如下命令启动 EMQX。

```bash
./emqx/bin/emqx foreground
```
