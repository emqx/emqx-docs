# 在 Debian 上安装 EMQX 开源版

本页将指导您如何在 Debian 系统中下载安装并启动 EMQX。

支持的 Debian 版本：

- Debian 12
- Debian 11
- Debian 10

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

EMQX 同时支持通过 deb 包或 tar.gz 包进行安装。如希望在其他支持系统中进行安装，或体验其他版本，可前往 [EMQX 开源版下载页面](https://www.emqx.com/zh/downloads-and-install/broker) 获取安装信息。

### 通过 deb 安装

1. 前往官方下载页面，选择 [Debian 页签](https://www.emqx.com/zh/downloads-and-install/broker?os=Debian)，选择 **Package**。
2. 选择最新版本 `@CE_VERSION@`，在**安装包类型**中根据需要的版本和 CPU 架构选择 `deb` 安装包。
3. 点击下方的下载链接。您也可以参照命令行指导步骤进行下载与安装。

#### 启动 EMQX

systemctl 启动：

```bash
sudo systemctl start emqx
```

#### 卸载 EMQX

服务完成后，可通过如下命令卸载 EMQX：

```shell
sudo apt remove --purge emqx
```

### 通过 tar.gz 安装

1. 前往官方下载页面，选择 [Debian 页签](https://www.emqx.com/zh/downloads-and-install/broker?os=Debian)，选择 **Package**。
2. 选择最新版本 `@CE_VERSION@`，在**安装包类型**中根据需要的版本和 CPU 架构选择 `tar.gz` 安装包。
3. 点击下方的下载链接。您也可以参照命令行指导步骤进行下载与安装。

安装完成后，可通过如下命令启动 EMQX。

```bash
./emqx/bin/emqx foreground
```
