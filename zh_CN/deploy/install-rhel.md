# 在 CentOS/RHEL 上安装 EMQX

本页将指导您如何在 CentOS/RHEL 系统中下载安装并启动 EMQX。

支持的 CentOS/RHEL 版本：

- Amazon Linux 2023
- Amazon Linux 2
- Rocky Linux 9 (RHEL 9)
- Rocky Linux 8 (RHEL 8)
- CentOS 7 (RHEL 7)

下文将以 CentOS 8 系统为例演示如何下载最新版 EMQX。如希望在其他系统中进行安装或希望安装其他版本，可前往 [EMQX 企业版下载页面](https://www.emqx.com/zh/downloads-and-install/enterprise)获取详细安装信息。

## 通过 rpm 安装

1. 前往官方下载页面，选择 [CentOS/RHEL 页签](https://www.emqx.com/zh/downloads-and-install/enterprise?os=RHEL)。
2. 选择最新版本 `@EE_VERSION@`，在**安装包类型**中根据所需的 CPU 架构选择 `RHEL 8 (CentOS 8) amd64` 或 `RHEL 8 (CentOS 8) arm64` -> `rpm` 安装包。
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


1. 前往官方下载页面，选择 [CentOS/RHEL 页签](https://www.emqx.com/zh/downloads-and-install/enterprise?os=RHEL)。
2. 选择最新版本 `@EE_VERSION@`，在**安装包类型**中根据所需的 CPU 架构选择 `RHEL 8 (CentOS 8) amd64` 或 `RHEL 8 (CentOS 8) arm64` -> `tar.gz` 安装包。
3. 点击下方的下载链接。您也可以参照命令行指导步骤进行下载与安装。

### 启动 EMQX

安装完成后，可通过如下命令启动 EMQX。

```
./emqx/bin/emqx foreground
```
