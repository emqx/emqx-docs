# Amazon Linux 2

本页将指导您如何在 Amazon Linux 2系统中下载安装并启动 EMQX。

{% emqxce %}

## 通过 rpm 安装

请根据 CPU 架构选择对应安装方式。

:::: tabs type:card

::: tab amd64

1. 下载 [emqx-@CE_VERSION@-amzn2-amd64.rpm](https://www.emqx.com/zh/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-amzn2-amd64.rpm)。

   ```bash
   wget https://www.emqx.com/zh/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-amzn2-amd64.rpm
   ```

2. 安装 EMQX。
   ```bash
   sudo yum install emqx-@CE_VERSION@-amzn2-amd64.rpm -y
   ```

:::

::: tab arm64
1. 下载 [emqx-@CE_VERSION@-amzn2-arm64.rpm](https://www.emqx.com/zh/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-amzn2-arm64.rpm)。

   ```bash
   wget https://www.emqx.com/zh/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-amzn2-arm64.rpm
   ```

2. 安装 EMQX。
   ```bash
   sudo yum install emqx-@CE_VERSION@-amzn2-arm64.rpm -y
   ```

:::

::::

### 启动 EMQX

您可通过如下三种方式启动 EMQX。

- 直接启动：

  ```bash
  $ emqx start
  EMQX @CE_VERSION@ is started successfully!
  
  $ emqx ctl status
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

### 卸载 EMQX

服务完成后，可通过如下命令卸载 EMQX：

  ```shell
sudo apt remove --purge emqx
  ```

## 通过 tar.gz 安装

请根据 CPU 架构选择对应安装方式。

:::: tabs type:card

::: tab amd64

1. 下载 [emqx-@CE_VERSION@-elixir-amzn2-amd64.tar.gz](https://www.emqx.com/zh/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-elixir-amzn2-amd64.tar.gz)。

   ```bash
   wget https://www.emqx.com/zh/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-elixir-amzn2-amd64.tar.gz
   ```

2. 安装 EMQX。
   ```bash
   mkdir -p emqx && tar -zxvf emqx-@CE_VERSION@-elixir-amzn2-amd64.tar.gz -C emqx
   ```

:::

::: tab arm64
1. 下载 [emqx-@CE_VERSION@-amzn2-arm64.tar.gz](https://www.emqx.com/zh/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-amzn2-arm64.tar.gz)。

   ```bash
   wget https://www.emqx.com/zh/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-amzn2-arm64.tar.gz
   ```

2. 安装 EMQX。
   ```bash
   mkdir -p emqx && tar -zxvf emqx-@CE_VERSION@-amzn2-arm64.tar.gz -C emqx
   ```

:::

::::

安装完成后，您可通过如下命令启动 EMQX:

```bash
sudo systemctl start emqx
```

{% endemqxce %}

{% emqxee %}

## 通过 rpm 安装

1. 前往 [EMQ 官网](https://www.emqx.com/zh/try?product=enterprise&currentVersion=@EE_VERSION@&currentOS=AmazonLinux2=currentOS=AmazonLinux2&utm_source=docs.emqx.com&utm_medium=referral&utm_campaign=enterprise-docs-install-to-try-enterprise)，**版本**选择 `@EE_VERSION@`，**系统**选择 `Amazon Linux 2`，点击**免费下载**按钮进入下载页面。

2. 在安装与下载页面中，**安装方式**选择 `rpm`，选择合适的 **CPU 架构**，按照提示进行下载与安装。

### 启动 EMQX 

您可通过如下三种方式启动 EMQX。

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

## 通过 tar.gz 安装

1. 前往 [EMQ 官网](https://www.emqx.com/zh/try?product=enterprise&currentVersion=@EE_VERSION@&currentOS=AmazonLinux2=currentOS=AmazonLinux2&utm_source=docs.emqx.com&utm_medium=referral&utm_campaign=enterprise-docs-install-to-try-enterprise)，**版本**选择 `@EE_VERSION@`，**系统**选择 `Amazon Linux 2`，点击**免费下载**按钮进入下载页面。

2. 在安装与下载页面中，**安装方式**选择 `tar.gz`，选择合适的 **CPU 架构**，按照提示进行下载与安装。


### 启动 EMQX

安装完成后，可通过如下命令启动 EMQX。

```bash
./emqx/bin/emqx start
```

{% endemqxee %}
