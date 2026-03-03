# 在 Ubuntu 上安装 EMQX

本页将指导您如何在 Ubuntu 系统中下载安装并启动最新版 EMQX。

支持的 Ubuntu 版本：

- Ubuntu 24.04
- Ubuntu 22.04
- Ubuntu 20.04

## 使用 Apt 包管理器安装

EMQX 支持使用 Apt 包管理器进行安装，为用户提供了一种便捷且可靠的方式来管理 EMQX 的安装和更新。以下是使用 apt 安装 EMQX 的步骤：

1. 添加 EMQX 的 apt 仓库：

   ```bash
   curl -s https://packagecloud.io/install/repositories/emqx/emqx-enterprise5/script.deb.sh | sudo bash
   ```

2. 安装 EMQX：

   ```bash
   sudo apt-get install emqx
   ```

3. 启动 EMQX：

   ```bash
   sudo systemctl start emqx
   ```

## 手动安装软件包

EMQX 支持通过 `.deb` 软件包或 `.tar.gz` 压缩包进行安装。如希望在其他支持系统中进行安装，或体验其他版本，可前往 [EMQX 企业版下载页面](https://www.emqx.com/zh/downloads-and-install/enterprise)获取安装信息。

### 通过 deb 安装

1. 前往官方下载页面，选择 [Ubuntu 页签](https://www.emqx.com/zh/downloads-and-install/enterprise?os=Ubuntu)。
2. 选择最新版本 `@EE_VERSION@`，在**安装包类型**中根据需要的版本和 CPU 架构选择 `deb` 安装包。
3. 点击下方的下载链接。您也可以参照命令行指导步骤进行下载与安装。

#### 启动 EMQX

启动为一个 systemd 服务：

```bash
sudo systemctl start emqx
```

#### 卸载 EMQX

安装完成后，可通过如下命令卸载 EMQX：

```shell
sudo apt remove --purge emqx
```

### 通过 tar.gz 安装

1. 前往官方下载页面，选择 [Ubuntu 页签](https://www.emqx.com/zh/downloads-and-install/enterprise?os=Ubuntu)。
2. 选择最新版本 `@EE_VERSION@`，在**安装包类型**中根据需要的版本和 CPU 架构选择 `tar.gz`。
3. 点击下方的下载链接。您也可以参照命令行指导步骤进行下载与安装。

安装完成后，可通过如下命令启动 EMQX。

```bash
./emqx/bin/emqx foreground
```
