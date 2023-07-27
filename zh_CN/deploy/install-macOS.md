# macOS

本章节将指导您如何通过 zip 包在 macOS 系统中下载安装并启动 EMQX。

支持的操作系统：

- macOS 12
- macOS 11

{% emqxce %}

## 安装 EMQX

下文将以 macOS 12 系统为例演示如何下载最新版 EMQX。如希望在其他系统中进行安装，可前往 [EMQX 下载页面](https://www.emqx.io/zh/downloads?os=macOS) 获取详细的安装信息。

1. 下载 [emqx-@CE_VERSION@-macos12-arm64.zip](https://www.emqx.com/zh/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-macos12-arm64.zip)。

```bash
wget https://www.emqx.com/zh/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-macos12-arm64.zip
```

2. 安装 EMQX。

```bash
mkdir -p emqx && unzip emqx-@CE_VERSION@-macos12-arm64.zip -d emqx
```

## 启动 EMQX

安装完成后，可通过如下命令启动 EMQX。

```bash
./emqx/bin/emqx start
```

{% endemqxce %}

{% emqxee %}

## 安装 EMQX

下文将以 macOS 12 系统为例演示如何下载最新版 EMQX。如希望在其他系统中进行安装，可前往 [EMQX 下载页面](https://www.emqx.com/zh/try?product=enterprise) 获取详细的安装信息。

1. 前往 [EMQ 官网](https://www.emqx.com/zh/try?product=enterprise&currentVersion=@EE_VERSION@&currentOS=macOS=currentOS=macOS12&utm_source=docs.emqx.com&utm_medium=referral&utm_campaign=enterprise-docs-install-to-try-enterprise)，**版本**选择 `@EE_VERSION@`，**系统**选择 `macOS`，点击**免费下载**按钮进入下载页面。

2. 在安装与下载页面中，**安装方式**选择 `zip`，选择合适的 **CPU 架构**，按照提示进行下载与安装。

## 启动 EMQX

安装完成后，可通过如下命令启动 EMQX。

```bash
./emqx/bin/emqx start
```

{% endemqxee %}
