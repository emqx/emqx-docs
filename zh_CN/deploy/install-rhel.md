# CentOS/RHEL

本页将指导您如何在 CentOS/RHEL 系统中下载安装并启动 EMQX。

支持的 CentOS/RHEL 版本：

- Amazon Linux 2023
- Amazon Linux 2
- Rocky Linux 9 (RHEL 9)
- Rocky Linux 8 (RHEL 8)
- CentOS 7 (RHEL 7)

{% emqxce %}

## 通过 Yum 源安装

EMQX 支持通过 Yum 源安装，您可通过以下 Yum 命令从中自动下载和安装 EMQX。

1. 通过以下命令配置 EMQX Yum 源：

   ```bash
   curl -s https://assets.emqx.com/scripts/install-emqx-rpm.sh | sudo bash
   ```

2. 安装以下依赖项：

   ```bash
   yum install epel-release -y
   yum install -y openssl11 openssl11-devel
   ```

3. 运行以下命令安装 EMQX：

   ```bash
   sudo yum install emqx -y
   ```

4. 运行以下命令启动 EMQX：

   ```bash
   sudo systemctl start emqx
   ```

## 通过 rpm 安装

EMQX 同时支持通过下载 rpm 安装包进行安装。本节以 CentOS 8 系统为例演示如何下载最新版 EMQX。如希望在其他支持系统中进行安装，或体验其他版本，可前往 [EMQX 下载页面](https://www.emqx.io/zh/downloads?os=CentOS)获取详细安装信息。

请根据 CPU 架构选择对应安装方式。

:::: tabs type:card

::: tab amd64

1. 下载 [emqx-@CE_VERSION@-el8-amd64.rpm](https://www.emqx.com/zh/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-el8-amd64.rpm)。

   ```bash
   wget https://www.emqx.com/zh/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-el8-amd64.rpm
   ```

2. 安装 EMQX。

   ```bash
   sudo yum install emqx-@CE_VERSION@-el8-amd64.rpm -y
   ```

:::

::: tab arm64
1. 下载 [emqx-@CE_VERSION@-el8-arm64.rpm](https://www.emqx.com/zh/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-el8-arm64.rpm)。

   ```bash
   wget https://www.emqx.com/zh/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-el8-arm64.rpm
   ```

2. 安装 EMQX。
   ```bash
   sudo yum install emqx-@CE_VERSION@-el8-arm64.rpm -y
   ```

:::

::::

### 启动 EMQX

您可通过如下 3 种方式启动 EMQX。

- 直接启动：

  ```bash
  $ emqx start
  EMQX @CE_VERSION@ is started successfully!
  
  $ emqx ctl status
  Node 'emqx@127.0.0.1' @CE_VERSION@ is started
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
sudo yum remove emqx
  ```

## 通过 tar.gz 安装

EMQX 同时支持通过下载 tar.gz 安装包进行安装。本节以 CentOS 8 系统为例演示如何下载最新版 EMQX。如希望在其他支持系统中进行安装，或体验其他版本，可前往 [EMQX 下载页面](https://www.emqx.io/zh/downloads?os=CentOS)获取详细安装信息。

请根据 CPU 架构选择对应安装方式。

:::: tabs type:card

::: tab amd64

1. 下载 [emqx-@CE_VERSION@-el8-amd64.tar.gz](https://www.emqx.com/zh/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-el8-amd64.tar.gz)。

   ```bash
   wget https://www.emqx.com/zh/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-el8-amd64.tar.gz
   ```

2. 安装 EMQX。
   ```bash
   mkdir -p emqx && tar -zxvf emqx-@CE_VERSION@-el8-amd64.tar.gz -C emqx
   ```

:::

::: tab arm64
1. 下载 [emqx-@CE_VERSION@-el8-arm64.tar.gz](https://www.emqx.com/zh/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-el8-arm64.tar.gz)。

   ```bash
   wget https://www.emqx.com/zh/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-el8-arm64.tar.gz
   ```

2. 安装 EMQX。
   ```bash
   mkdir -p emqx && tar -zxvf emqx-@CE_VERSION@-el8-arm64.tar.gz -C emqx
   ```

:::

::::

### 启动 EMQX

安装完成后，可通过如下命令启动 EMQX。

```bash
./emqx/bin/emqx start
```

{% endemqxce %}

{% emqxee %}

下文将以 CentOS 8 系统为例演示如何下载最新版 EMQX。如希望在其他系统中进行安装或希望安装其他版本，可前往 [EMQX 下载页面](https://www.emqx.com/zh/try?product=enterprise) 获取详细安装信息。

## 通过 rpm 安装

1. 前往 [EMQ 官网](https://www.emqx.com/zh/try?product=enterprise&currentVersion=@EE_VERSION@&currentOS=CentOS=currentOS=Centos8&utm_source=docs.emqx.com&utm_medium=referral&utm_campaign=enterprise-docs-install-to-try-enterprise)，**版本**选择 `@EE_VERSION@`，**系统**选择 `CentOS`，点击**免费下载**按钮进入下载页面。

2. 在安装与下载页面中，**安装方式**选择 `rpm`，选择合适的 **CPU 架构**，按照提示进行下载与安装。

### 启动 EMQX 

您可通过如下 3 种方式启动 EMQX。 
- 直接启动：

  ```bash
  $ emqx start
  EMQX @EE_VERSION@ is started successfully!

  $ emqx ctl status
  Node 'emqx@127.0.0.1' @EE_VERSION@ is started
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
  sudo yum remove emqx
  ```

## 通过 tar.gz 安装


1. 前往 [EMQ 官网](https://www.emqx.com/zh/try?product=enterprise&currentVersion=@EE_VERSION@&currentOS=CentOS=currentOS=Centos8&utm_source=docs.emqx.com&utm_medium=referral&utm_campaign=enterprise-docs-install-to-try-enterprise)，**版本**选择 `@EE_VERSION@`，**系统**选择 `CentOS`，点击**免费下载**按钮进入下载页面。

2. 在安装与下载页面中，**安装方式**选择 `tar.gz`，选择合适的 **CPU 架构**，按照提示进行下载与安装。

### 启动 EMQX

安装完成后，可通过如下命令启动 EMQX。

```
./emqx/bin/emqx start
```

{% endemqxee %}
