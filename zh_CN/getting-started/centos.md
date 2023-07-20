# CentOS/RHEL

本章节将指导您如何在 CentOS/RHEL 系统中下载安装并启动 EMQX。

支持的 CentOS/RHEL 版本：

- CentOS 8
- CentOS 7

EMQX 支持通过下载包进行安装。下文将以 CentOS 7 系统为例演示如何下载 EMQX 4.3.19。如希望在其他支持系统中进行安装，或体验其他版本，可前往 [EMQX 下载页面](https://www.emqx.com/zh/try?product=enterprise) 获取详细安装信息。

## 通过 rpm 安装

请根据 CPU 架构选择对应安装方式。

:::: tabs type:card

::: tab amd64

1. 下载 [emqx-ee-centos7-4.3.19-amd64.rpm](https://www.emqx.com/zh/downloads/enterprise/4.3.19/emqx-ee-centos7-4.3.19-amd64.rpm).

   ```bash
   wget https://www.emqx.com/zh/downloads/enterprise/4.3.19/emqx-ee-centos7-4.3.19-amd64.rpm
   ```

2. 安装 EMQX。

   ```bash
   sudo yum install emqx-ee-centos7-4.3.19-amd64.rpm -y
   ```

:::

::: tab arm64

1. 下载 [emqx-ee-centos7-4.3.19-arm64.rpm](https://www.emqx.com/zh/downloads/enterprise/4.3.19/emqx-ee-centos7-4.3.19-arm64.rpm)。

   ```bash
   wget https://www.emqx.com/zh/downloads/enterprise/4.3.19/emqx-ee-centos7-4.3.19-arm64.rpm
   ```

2. 安装 EMQX。

   ```bash
   sudo yum install emqx-ee-centos7-4.3.19-arm64.rpm -y
   ```

:::

::::

### 启动 EMQX

您可通过如下三种方式启动 EMQX

- 直接启动：

  ```bash
  $ emqx start
  EMQX 4.3.19 is started successfully!
  
  $ emqx_ctl status
  Node 'emqx@127.0.0.1' 4.3.19 is started
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

请根据 CPU 架构选择对应安装方式。

:::: tabs type:card

::: tab amd64

1. 下载 [mqx-ee-centos7-4.3.19-amd64.zip](https://www.emqx.com/zh/downloads/enterprise/4.3.19/emqx-ee-centos7-4.3.19-amd64.zip)

   ```bash
   wget https://www.emqx.com/zh/downloads/enterprise/4.3.19/emqx-ee-centos7-4.3.19-amd64.zip
   ```

2. 安装 EMQX。

   ```bash
   unzip emqx-ee-centos7-4.3.19-amd64.zip
   ```

:::

::: tab arm64

1. 下载 [emqx-ee-centos7-4.3.19-arm64.zip](https://www.emqx.com/zh/downloads/enterprise/4.3.19/emqx-ee-centos7-4.3.19-arm64.zip)

   ```bash
   wget https://www.emqx.com/zh/downloads/enterprise/4.3.19/emqx-ee-centos7-4.3.19-arm64.zip
   ```

2. 安装 EMQX。

   ```bash
   unzip emqx-ee-centos7-4.3.19-arm64.zip
   ```

:::

::::

### 启动 EMQX

安装完成后，可通过如下命令启动 EMQX。

```bash
./emqx/bin/emqx start
```
