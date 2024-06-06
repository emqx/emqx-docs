# 在 Debian 上安装 EMQX 企业版

本页将指导您如何在 Debian 系统中下载安装并启动最新版 EMQX。

支持的 Debian 版本：

- Debian 12
- Debian 11
- Debian 10

如希望在其他支持系统中进行安装，或体验其他版本，请可前往 [EMQX 企业版下载页面](https://www.emqx.com/zh/downloads-and-install/enterprise)获取安装信息。

## 通过 deb 安装

1. 前往官方下载页面，选择 [Ubuntu 页签](https://www.emqx.com/zh/downloads-and-install/enterprise?os=Debian)。
2. 选择最新版本 `@EE_VERSION@`，在**安装包类型**中根据需要的版本和 CPU 架构选择 `deb` 安装包。
3. 点击下方的下载链接。您也可以参照命令行指导步骤进行下载与安装。

### 启动 EMQX 

systemctl 启动：

```bash
sudo systemctl start emqx
```

### 卸载 EMQX

服务完成后，可通过如下命令卸载 EMQX：

```shell
sudo apt remove --purge emqx
```

## 通过 tar.gz 安装

1. 前往官方下载页面，选择 [Ubuntu 页签](https://www.emqx.com/zh/downloads-and-install/enterprise?os=Debian)。
2. 选择最新版本 `@EE_VERSION@`，在**安装包类型**中根据需要的版本和 CPU 架构选择 `tar.gz` 安装包。
3. 点击下方的下载链接。您也可以参照命令行指导步骤进行下载与安装。

安装完成后，可通过如下命令启动 EMQX。

```bash
./emqx/bin/emqx foreground
```
