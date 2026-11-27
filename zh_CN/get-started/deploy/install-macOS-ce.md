# 在 macOS 上安装 EMQX

::: tip

通过 Homebrew 安装 EMQX 开源版的方式已弃用。请使用下方的 ZIP 安装步骤。

:::

以下步骤介绍如何通过 ZIP 安装包在 macOS 系统中安装并启动 EMQX 开源版 `@CE_VERSION@`。

支持的操作系统和架构：

- macOS 15，Apple 芯片（arm64）

如果您想安装不同版本或在不同系统中安装，请访问 [EMQX 开源版下载页面](https://www.emqx.com/zh/downloads-and-install/broker)。

## 使用 Zip 软件包安装 EMQX

1. 前往官方下载页面，选择 [macOS 页签](https://www.emqx.com/zh/downloads-and-install/broker?os=macOS)，选择 **Package**。
2. 选择版本 `@CE_VERSION@`，然后选择适用于 macOS 15 arm64 的 ZIP 安装包。
3. 点击下方的下载链接。您也可以参照命令行指导步骤进行下载与安装。

## 启动和停止 EMQX

EMQX 可以以守护进程模式、前台模式或交互模式启动。请注意，默认情况下只能同时运行一个 EMQX 实例。

请在 ZIP 包解压后的 EMQX 目录中运行以下命令。

```bash
# 以守护进程模式启动
./bin/emqx start

# 以前台模式启动
./bin/emqx foreground

# 以交互模式启动，使用 Erlang shell
./bin/emqx console
```

如果以前台模式或交互模式启动，启动成功后，EMQX 将输出以下消息：

```bash
EMQX @CE_VERSION@ is running now!
```

你可能还会看到一些警告消息，这些消息是为生产环境的操作者准备的，如果 EMQX 在本地环境进行测试、实验或客户端开发，可以忽略这些消息：

```bash
ERROR: DB Backend is RLOG, but an incompatible OTP version has been detected. Falling back to using Mnesia DB backend.
WARNING: ulimit -n is 256; 1024 is the recommended minimum.
WARNING: Default (insecure) Erlang cookie is in use.
WARNING: Configure node.cookie in /path/to/emqx/etc/emqx.conf or override from environment variable EMQX_NODE__COOKIE
WARNING: NOTE: Use the same cookie for all nodes in the cluster.
```

你可以使用以下命令检查 EMQX 的状态：

```bash
./bin/emqx ctl status
```

打开你的 Web 浏览器，在地址栏中输入 `http://localhost:18083/`（`localhost` 可替换为你的 IP 地址）访问 [EMQX Dashboard](../../guides/dashboard/introduction.md)，从中你可以连接到客户端或查看运行状态。

默认的用户名和密码为 `admin` 和 `public`。登录后，你将被要求更改默认密码。

要停止 EMQX：

- 如果以守护进程模式启动，使用 `./bin/emqx stop`。
- 如果以前台模式启动，按下 Ctrl+C。
- 如果以交互模式启动，连续按下两次 Ctrl+C。
