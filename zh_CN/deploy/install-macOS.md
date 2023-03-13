# macOS

本章节将指导您如何通过 zip 包在 macOS 系统中下载安装并启动 EMQX。

支持的操作系统：

- macOS 12
- macOS 11

{% emqxce %}

## 安装 EMQX

下文将以 macOS 12 系统为例演示如何下载最新版 EMQX。如希望在其他系统中进行安装，可前往 [EMQX 下载页面](https://www.emqx.io/zh/downloads?os=macOS) 获取详细的安装信息。

1. 下载 [emqx-5.0.20-macos12-arm64.zip](https://www.emqx.com/zh/downloads/broker/5.0.20/emqx-5.0.20-macos12-arm64.zip)。

```bash
wget https://www.emqx.com/zh/downloads/broker/5.0.20/emqx-5.0.20-macos12-arm64.zip
```

2. 安装 EMQX。

```bash
mkdir -p emqx && unzip emqx-5.0.20-macos12-arm64.zip -d emqx
```

## 启动 EMQX

您可通过如下三种方式启动 EMQX

- 直接启动：

  ```bash
  $ emqx start
  EMQX 5.0.20 is started successfully!
  
  $ emqx_ctl status
  Node 'emqx@127.0.0.1' 5.0.20 is started
  ```

- systemctl 启动：

  ```bash
  sudo systemctl start emqx
  ```

- service 启动：

  ```bash
  sudo service emqx start
  ```

## 卸载 EMQX

服务完成后，可通过如下命令卸载 EMQX：

  ```shell
sudo apt remove --purge emqx
  ```

{% endemqxce %}

{% emqxee %}

## 安装 EMQX

下文将以 macOS 12 系统为例演示如何下载最新版 EMQX。如希望在其他系统中进行安装，可前往 [EMQX 下载页面](https://www.emqx.com/zh/try?product=enterprise) 获取详细的安装信息。

1. 下载 [emqx-enterprise-5.0.1-macos12-arm64.zip](https://www.emqx.com/zh/downloads/enterprise/5.0.1/emqx-enterprise-5.0.1-macos12-arm64.zip)。

```bash
wget https://www.emqx.com/zh/downloads/enterprise/5.0.1/emqx-enterprise-5.0.1-macos12-arm64.zip
```

2. 安装 EMQX。

```bash
mkdir -p emqx && unzip emqx-enterprise-5.0.1-macos12-arm64.zip -d emqx
```

## 启动 EMQX

您可通过如下三种方式启动 EMQX

- 直接启动：

  ```bash
  $ emqx start
  EMQX 5.0.1 is started successfully!
  
  $ emqx_ctl status
  Node 'emqx@127.0.0.1' 5.0.1 is started
  ```

- systemctl 启动：

  ```bash
  sudo systemctl start emqx
  ```

- service 启动：

  ```bash
  sudo service emqx start
  ```

## 卸载 EMQX

服务完成后，可通过如下命令卸载 EMQX：

  ```shell
sudo apt remove --purge emqx
  ```

{% endemqxee %}
