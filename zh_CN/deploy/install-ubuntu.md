# Ubuntu

本章节将指导您如何在 Ubuntu 系统中下载安装并启动 EMQX。

支持的 Ubuntu 版本：

- Ubuntu 22.04
- Ubuntu 20.04
- Ubuntu 18.04

{% emqxce %}

## 通过 Apt 源安装

EMQX 支持通过 Apt 源安装，免除了用户需要手动处理依赖关系和更新软件包等的困扰，具有更加方便、安全和易用等优点。如希望通过 Apt 源安装 EMQX，可参考如下步骤。

1. 通过以下命令配置 EMQX Apt 源：

   ```bash
   curl -s https://assets.emqx.com/scripts/install-emqx-deb.sh | sudo bash
   ```

2. 运行以下命令安装 EMQX：

   ```
   sudo apt-get install emqx
   ```

3. 运行以下命令启动 EMQX：

   ```
   sudo systemctl start emqx
   ```

   

## 通过下载包安装

EMQX 支持通过 deb 包或 tar.gz 包进行安装。下文将以 Ubuntu 22.04 系统为例演示如何下载最新版 EMQX。如希望在其他支持系统中进行安装，或体验其他版本，请可前往 [EMQX 下载页面](https://www.emqx.io/zh/downloads?os=Ubuntu) 获取安装信息。

### 通过 deb 安装

请根据 CPU 架构选择对应安装方式。

:::: tabs type:card

::: tab amd64

1. 下载 [emqx-@CE_VERSION@-ubuntu22.04-amd64.deb](https://www.emqx.com/zh/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-ubuntu22.04-amd64.deb)。

   ```bash
   wget https://www.emqx.com/zh/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-ubuntu22.04-amd64.deb
   ```

2. 安装 EMQX。
   ```bash
   sudo apt install ./emqx-@CE_VERSION@-ubuntu22.04-amd64.deb
   ```

:::

::: tab arm64
1. 下载 [emqx-@CE_VERSION@-ubuntu22.04-arm64.deb](https://www.emqx.com/zh/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-ubuntu22.04-arm64.deb)。

   ```bash
   wget https://www.emqx.com/zh/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-ubuntu22.04-arm64.deb
   ```

2. 安装 EMQX。
   ```bash
   sudo apt install ./emqx-@CE_VERSION@-ubuntu22.04-arm64.deb
   ```

:::

::::

#### 启动 EMQX

您可通过如下三种方式启动 EMQX

- 直接启动：

  ```bash
  $ emqx start
  EMQX @CE_VERSION@ is started successfully!
  
  $ emqx_ctl status
  Node 'emqx@127.0.0.1' @CE_VERSION@ is started
  ```

- systemctl 启动：

  ```bash
  sudo systemctl start emqx
  ```

- service 启动：

  ```bash
  sudo service emqx start
  ```

#### 卸载 EMQX

服务完成后，可通过如下命令卸载 EMQX：

  ```shell
sudo apt remove --purge emqx
  ```

### 通过 tar.gz 安装

请根据 CPU 架构选择对应安装方式。

:::: tabs type:card

::: tab amd64

1. 下载 [emqx-@CE_VERSION@-ubuntu22.04-amd64.tar.gz](https://www.emqx.com/zh/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-ubuntu22.04-amd64.tar.gz)。

   ```bash
   wget https://www.emqx.com/zh/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-ubuntu22.04-amd64.tar.gz
   ```

2. 安装 EMQX。
   ```bash
   mkdir -p emqx && tar -zxvf emqx-@CE_VERSION@-ubuntu22.04-amd64.tar.gz -C emqx
   ```

:::

::: tab arm64
1. 下载 [emqx-@CE_VERSION@-ubuntu22.04-arm64.tar.gz](https://www.emqx.com/zh/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-ubuntu22.04-arm64.tar.gz)。

   ```bash
   wget https://www.emqx.com/zh/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-ubuntu22.04-arm64.tar.gz
   ```

2. 安装 EMQX。
   ```bash
   mkdir -p emqx && tar -zxvf emqx-@CE_VERSION@-ubuntu22.04-arm64.tar.gz -C emqx
   ```

:::

::::

安装完成后，可通过如下命令启动 EMQX。

```bash
./emqx/bin/emqx start
```

{% endemqxce %}

{% emqxee %}

下文将以 Ubuntu 22.04 系统为例演示如何下载最新版 EMQX。如希望在其他支持系统中进行安装，或体验其他版本，请可前往 [EMQX 下载页面](https://www.emqx.com/zh/try?product=enterprise) 获取安装信息。

## 通过 deb 安装

请根据 CPU 架构选择对应安装方式。

:::: tabs type:card

::: tab amd64

1. 下载 [emqx-enterprise-@EE_VERSION@-ubuntu22.04-amd64.deb](https://www.emqx.com/zh/downloads/enterprise/@EE_VERSION@/emqx-enterprise-@EE_VERSION@-ubuntu22.04-amd64.deb)。

   ```bash
   wget https://www.emqx.com/zh/downloads/enterprise/@EE_VERSION@/emqx-enterprise-@EE_VERSION@-ubuntu22.04-amd64.deb
   ```

2. 安装 EMQX。
   ```bash
   sudo apt install ./emqx-enterprise-@EE_VERSION@-ubuntu22.04-amd64.deb
   ```

:::

::: tab arm64
1. 下载 [emqx-enterprise-@EE_VERSION@-ubuntu22.04-arm64.deb](https://www.emqx.com/zh/downloads/enterprise/@EE_VERSION@/emqx-enterprise-@EE_VERSION@-ubuntu22.04-arm64.deb)。

   ```bash
   wget https://www.emqx.com/zh/downloads/enterprise/@EE_VERSION@/emqx-enterprise-@EE_VERSION@-ubuntu22.04-arm64.deb
   ```

2. 安装 EMQX。
   ```bash
   sudo apt install ./emqx-enterprise-@EE_VERSION@-ubuntu22.04-arm64.deb
   ```

:::

::::

### 启动 EMQX

您可通过如下三种方式启动 EMQX

- 直接启动：

  ```bash
  $ emqx start
  EMQX @EE_VERSION@ is started successfully!

  $ emqx_ctl status
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
  sudo apt remove --purge emqx
  ```

## 通过 tar.gz 安装

请根据 CPU 架构选择对应安装方式。

:::: tabs type:card

::: tab amd64

1. 下载 [emqx-enterprise-@EE_VERSION@-ubuntu22.04-amd64.tar.gz](https://www.emqx.com/zh/downloads/enterprise/@EE_VERSION@/emqx-enterprise-@EE_VERSION@-ubuntu22.04-amd64.tar.gz)。

   ```bash
   wget https://www.emqx.com/zh/downloads/enterprise/@EE_VERSION@/emqx-enterprise-@EE_VERSION@-ubuntu22.04-amd64.tar.gz
   ```

   

2. 安装 EMQX。
   ```bash
   mkdir -p emqx && tar -zxvf emqx-enterprise-@EE_VERSION@-ubuntu22.04-amd64.tar.gz -C emqx
   ```

:::

::: tab arm64
1. 下载 [emqx-enterprise-@EE_VERSION@-ubuntu22.04-arm64.tar.gz](https://www.emqx.com/zh/downloads/enterprise/@EE_VERSION@/emqx-enterprise-@EE_VERSION@-ubuntu22.04-arm64.tar.gz)。

   ```bash
   wget https://www.emqx.com/zh/downloads/enterprise/@EE_VERSION@/emqx-enterprise-@EE_VERSION@-ubuntu22.04-arm64.tar.gz
   ```

2. 安装 EMQX。
   ```bash
   mkdir -p emqx && tar -zxvf emqx-enterprise-@EE_VERSION@-ubuntu22.04-arm64.tar.gz -C emqx
   ```

:::

::::

安装完成后，可通过如下命令启动 EMQX。

```
./emqx/bin/emqx start
```

{% endemqxee %}
