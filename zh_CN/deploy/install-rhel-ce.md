# 在 CentOS/RHEL 上安装 EMQX 开源版

本页将指导您如何在 CentOS/RHEL 系统中下载安装并启动 EMQX。

支持的 CentOS/RHEL 版本：

- Amazon Linux 2023
- Amazon Linux 2
- Rocky Linux 9 (RHEL 9)
- Rocky Linux 8 (RHEL 8)
- CentOS 7 (RHEL 7)

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

EMQX 同时支持通过下载 rpm 安装包进行安装。本节以 CentOS 8 系统为例演示如何下载最新版 EMQX。如希望在其他支持系统中进行安装，或体验其他版本，可前往 [EMQX 开源版下载页面](https://www.emqx.com/zh/downloads-and-install/broker)获取详细安装信息。

1. 前往官方下载页面，选择 [CentOS/RHEL 页签](https://www.emqx.com/zh/downloads-and-install/broker?os=RHEL)，选择 **Package**。
2. 选择最新版本 `@CE_VERSION@`，在**安装包类型**中根据所需的 CPU 架构选择 `RHEL 8 (CentOS 8) amd64` 或 `RHEL 8 (CentOS 8) arm64` -> `rpm` 安装包。
3. 点击下方的下载链接。您也可以参照命令行指导步骤进行下载与安装。

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

EMQX 同时支持通过下载 tar.gz 安装包进行安装。本节以 CentOS 8 系统为例演示如何下载最新版 EMQX。如希望在其他支持系统中进行安装，或体验其他版本，可前往 [EMQX 开源版下载页面](https://www.emqx.com/zh/downloads-and-install/broker)获取详细安装信息。

1. 前往官方下载页面，选择 [CentOS/RHEL 页签](https://www.emqx.com/zh/downloads-and-install/broker?os=RHEL)，选择 **Package**。
2. 选择最新版本 `@CE_VERSION@`，在**安装包类型**中根据所需的 CPU 架构选择 `RHEL 8 (CentOS 8) amd64` 或 `RHEL 8 (CentOS 8) arm64` -> `tar.gz` 安装包。
3. 点击下方的下载链接。您也可以参照命令行指导步骤进行下载与安装。

### 启动 EMQX

安装完成后，可通过如下命令启动 EMQX。

```bash
./emqx/bin/emqx foreground
```

