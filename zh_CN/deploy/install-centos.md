# CentOS/RHEL

本章节将指导您如何在 CentOS/RHEL 系统中下载安装并启动 EMQX。

支持的 CentOS/RHEL 版本：

- CentOS 8
- CentOS 7

下文将以 CentOS 8 系统为例演示如何下载最新版 EMQX。如希望在 CentOS 7 系统中进行安装，只需将下方命令中的版本替换为对应版本即可，或前往 [EMQX 下载页面](https://www.emqx.com/zh/try?product=enterprise) 获取安装信息。

{% emqxce %}

## 通过 rpm 安装

请根据 CPU 架构选择对应安装方式。

:::: tabs type:card

::: tab amd64

1. 下载 [emqx-5.0.17-el8-amd64.rpm](https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-el8-amd64.rpm)https://www.emqx.com/downloads/broker/v5.0.14/emqx-5.0.14-el8-amd64.rpm)。

   ```
   wget https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-el8-amd64.rpm
   ```

2. 安装 EMQX。

   ```
   sudo yum install emqx-5.0.17-el8-amd64.rpm -y
   ```

:::

::: tab arm64
1. 下载 [emqx-5.0.17-el8-arm64.rpm](https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-el8-arm64.rpm)。

   ```bash
   wget https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-el8-arm64.rpm
   ```

2. 安装 EMQX。
   ```bash
   sudo yum install emqx-5.0.17-el8-arm64.rpm -y
   ```

:::

::::

安装完成后，可通过如下命令启动 EMQX。

  ```shell
  sudo systemctl start emqx
  ```

## 通过 tar.gz 安装

请根据 CPU 架构选择对应安装方式。

:::: tabs type:card

::: tab amd64

1. 下载 [emqx-5.0.17-el8-amd64.tar.gz](https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-el8-amd64.tar.gz)。

   ```bash
   wget https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-el8-amd64.tar.gz
   ```

2. 安装 EMQX。
   ```bash
   mkdir -p emqx && tar -zxvf emqx-5.0.17-el8-amd64.tar.gz -C emqx
   ```

:::

::: tab arm64
1. 下载 [emqx-5.0.17-el8-arm64.tar.gz](https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-el8-arm64.tar.gz)。

   ```bash
   wget https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-el8-arm64.tar.gz
   ```

2. 安装 EMQX。
   ```bash
   mkdir -p emqx && tar -zxvf emqx-5.0.17-el8-arm64.tar.gz -C emqx
   ```

:::

::::

安装完成后，可通过如下命令启动 EMQX。

```bash
./emqx/bin/emqx start
```

{% endemqxce %}

{% emqxee %}

## 通过 rpm 安装

请根据 CPU 架构选择对应安装方式。

:::: tabs type:card

::: tab amd64

1. 下载 [emqx-enterprise-5.0.0-el8-amd64.rpm](https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-el8-amd64.rpm)。

   ```bash
   wget https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-el8-amd64.rpm
   ```

2. 安装 EMQX。
   ```bash
   sudo yum install emqx-enterprise-5.0.0-el8-amd64.rpm -y
   ```

:::

::: tab arm64
1. 下载 [emqx-enterprise-5.0.0-el8-arm64.rpm](https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-el8-arm64.rpm)。

   ```bash
   wget https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-el8-arm64.rpm
   ```

2. 安装 EMQX。
   ```bash
   sudo yum install emqx-enterprise-5.0.0-el8-arm64.rpm -y
   ```

:::

::::

### 启动 EMQX 

您可通过如下三种方式启动 EMQX <!--TODO @WIVWIV这里的版本我修改了，需要确认下，另外，这些启动方式是只适用于企业版还是开源版？区别是？-->

- 直接启动：

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

1. 下载 [emqx-enterprise-5.0.0-debian11-amd64.tar.gz](https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-debian11-amd64.tar.gz)。

   ```bash
   wget https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-debian11-amd64.tar.gz
   ```

2. 安装 EMQX。
   ```
   wget https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-debian11-amd64.tar.gz
   ```

:::

::: tab arm64
1. 下载 [emqx-enterprise-5.0.0-debian11-arm64.tar.gz](https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-debian11-arm64.tar.gz)。

   ```bash
   wget https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-debian11-arm64.tar.gz
   ```

2. 安装 EMQX。
   ```bash
   mkdir -p emqx && tar -zxvf emqx-enterprise-5.0.0-debian11-arm64.tar.gz -C emqx
   ```

:::

::::

安装完成后，可通过如下命令启动 EMQX。

```
./emqx/bin/emqx start
```

{% endemqxee %}