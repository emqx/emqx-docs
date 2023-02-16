# Ubuntu

本章节将指导您如何在 Ubuntu 系统中下载安装并启动 EMQX。

支持的 Ubuntu 版本：

{% emqxce %}

- Ubuntu 22.04
- Ubuntu 20.04
- Ubuntu 18.04

{% endemqxce %}

{% emqxee %}

- Ubuntu 20.04
- Ubuntu 18.04

{% endemqxee %}

下文将以 Ubuntu 20.04 系统为例演示如何下载最新版 EMQX。如希望在其他支持系统中进行安装，或体验其他版本，请可前往 [EMQX 下载页面](https://www.emqx.com/zh/try?product=enterprise) 获取安装信息。

{% emqxce %}

## 通过 deb 安装

请根据 CPU 架构选择对应安装方式。

:::: tabs type:card

::: tab amd64

1. 下载 [emqx-5.0.17-ubuntu20.04-amd64.deb](https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-ubuntu20.04-amd64.deb)。

   ```bash
   wget https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-ubuntu20.04-amd64.deb
   ```

2. 安装 EMQX。
   ```bash
   sudo apt install ./emqx-5.0.17-ubuntu20.04-amd64.deb
   ```

:::

::: tab arm64
1. 下载 [emqx-5.0.17-ubuntu20.04-arm64.deb](https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-ubuntu20.04-arm64.deb)。

   ```bash
   wget https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-ubuntu20.04-arm64.deb
   ```

2. 安装 EMQX。
   ```bash
   sudo apt install ./emqx-5.0.17-ubuntu20.04-arm64.deb
   ```

:::

::::

### 启动 EMQX

您可通过如下三种方式启动 EMQX

- 直接启动：<!--TODO @WIVWIV这里的版本我修改了，需要确认下，另外，这些启动方式是只适用于企业版还是开源版？区别是？-->

  ```bash
  $ emqx start
  EMQX 5.0.0 is started successfully!
  
  $ emqx_ctl status
  Node 'emqx@127.0.0.1' 5.0.0 is started
  ```

- systemctl 启动：

  ```bash
  sudo systemctl start emqx
  ```

- service 启动：

  ```bash
  sudo service emqx start
  ```

### 卸载 EMQX

服务完成后，可通过如下命令卸载 EMQX：

  ```shell
sudo apt remove --purge emqx
  ```

## 通过 tar.gz 安装

请根据 CPU 架构选择对应安装方式。

:::: tabs type:card

::: tab amd64

1. 下载 [emqx-5.0.17-ubuntu20.04-amd64.tar.gz](https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-ubuntu20.04-amd64.tar.gz)。

   ```bash
   wget https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-ubuntu20.04-amd64.tar.gz
   ```

2. 安装 EMQX。
   ```bash
   mkdir -p emqx && tar -zxvf emqx-5.0.17-ubuntu20.04-amd64.tar.gz -C emqx
   ```

:::

::: tab arm64
1. 下载 [emqx-5.0.17-ubuntu20.04-arm64.tar.gz](https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-ubuntu20.04-arm64.tar.gz)。

   ```bash
   wget https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-ubuntu20.04-arm64.tar.gz
   ```

2. 安装 EMQX。
   ```bash
   mkdir -p emqx && tar -zxvf emqx-5.0.17-ubuntu20.04-arm64.tar.gz -C emqx
   ```

:::

::::

安装完成后，可通过如下命令启动 EMQX。

```bash
./emqx/bin/emqx start
```

{% endemqxce %}

{% emqxee %}

## 通过 deb 安装

请根据 CPU 架构选择对应安装方式。

:::: tabs type:card

::: tab amd64

1. 下载 [emqx-enterprise-5.0.0-ubuntu20.04-amd64.deb](https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-ubuntu20.04-amd64.deb)。

   ```bash
   wget https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-ubuntu20.04-amd64.deb
   ```

2. 安装 EMQX。
   ```bash
   sudo apt install ./emqx-enterprise-5.0.0-ubuntu20.04-amd64.deb
   ```

:::

::: tab arm64
1. 下载 [emqx-enterprise-5.0.0-ubuntu20.04-arm64.deb](https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-ubuntu20.04-arm64.deb)。

   ```bash
   wget https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-ubuntu20.04-arm64.deb
   ```

2. 安装 EMQX。
   ```bash
   sudo apt install ./emqx-enterprise-5.0.0-ubuntu20.04-arm64.deb
   ```

:::

::::

### 启动 EMQX

您可通过如下三种方式启动 EMQX

- 直接启动：<!--TODO @WIVWIV这里的版本我修改了，需要确认下，另外，这些启动方式是只适用于企业版还是开源版？区别是？-->

  ```bash
  $ emqx start
  EMQX 5.0.0 is started successfully!

  $ emqx_ctl status
  Node 'emqx@127.0.0.1' 5.0.0 is started
  ```

- systemctl 启动：

  ```bash
  sudo systemctl start emqx
  ```

- service 启动：

  ```bash
  sudo service emqx start
  ```

### 卸载 EMQX

服务完成后，可通过如下命令卸载 EMQX：

  ```shell
  sudo apt remove --purge emqx
  ```

## 通过 tar.gz 安装

请根据 CPU 架构选择对应安装方式。

:::: tabs type:card

::: tab amd64

1.  下载 [emqx-enterprise-5.0.0-ubuntu20.04-amd64.tar.gz](https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-ubuntu20.04-amd64.tar.gz)。

   ```bash
   wget https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-ubuntu20.04-amd64.tar.gz
   ```

2. 安装 EMQX。
   ```
   mkdir -p emqx && tar -zxvf emqx-enterprise-5.0.0-ubuntu20.04-amd64.tar.gz -C emqx
   ```

:::

::: tab arm64
1. 下载 [emqx-enterprise-5.0.0-ubuntu20.04-arm64.tar.gz](https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-ubuntu20.04-arm64.tar.gz)。

   ```bash
   wget https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-ubuntu20.04-arm64.tar.gz
   ```

2. 安装 EMQX。
   ```bash
   mkdir -p emqx && tar -zxvf emqx-enterprise-5.0.0-ubuntu20.04-arm64.tar.gz -C emqx
   ```

:::

::::

安装完成后，可通过如下命令启动 EMQX。

```
./emqx/bin/emqx start
```

{% endemqxee %}
