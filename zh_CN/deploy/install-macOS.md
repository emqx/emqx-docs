# macOS

本页将指导您如何通过 zip 包在 macOS 系统中下载安装并启动 EMQX。

支持的操作系统：

- macOS 13 （只适用于 Homebrew 安装包）
- macOS 12
- macOS 11

{% emqxce %}

## 使用 Homebrew 安装 EMQX

[Homebrew](https://brew.sh/) 是一个免费且开源的软件包管理系统，可简化在 macOS 上安装软件的过程。

1. 如果你的 Mac 上还没有安装 Homebrew，可以在终端中运行以下命令来进行安装：

   ```bash
   bashCopy code
   /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
   ```

2. 安装 EMQX：

   ```bash
   bashCopy code
   brew install emqx
   ```

## 使用 Zip 软件包安装 EMQX

1. 下载 [适用于你的操作系统和架构的 EMQX 软件包](https://www.emqx.io/downloads?os=macOS)。这是适用于搭载 macOS 12（Monterey）和 Apple Silicon 的 Mac 电脑的 zip 软件包的链接：

   ```bash
   wget https://www.emqx.com/zh/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-macos12-arm64.zi
   ```

2. 安装 EMQX。

   ```bash
   mkdir -p emqx && unzip emqx-@CE_VERSION@-macos12-arm64.zip -d emqx
   ```

## 启动和停止 EMQX

EMQX 可以以守护进程模式、前台模式或交互模式启动。请注意，默认情况下只能同时运行一个 EMQX 实例。

如果你使用 Homebrew 安装了 EMQX，请按照下面的指示使用 `emqx` 命令。如果你使用 zip 包安装了 EMQX，请使用 `bin/emqx`（假设你在解压 emqx 文件的目录中）。

```bash
# 以守护进程模式启动
emqx start

# 以前台模式启动
emqx foreground

# 以交互模式启动，使用 Erlang shell
emqx console
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
WARNING: Configure node.cookie in /opt/homebrew/Cellar/emqx/@CE_VERSION@/etc/emqx.conf or override from environment variable EMQX_NODE__COOKIE
WARNING: NOTE: Use the same cookie for all nodes in the cluster.
```

你可以使用以下命令检查 EMQX 的状态：

```bash
emqx ctl status
```

打开你的 Web 浏览器，在地址栏中输入 `http://localhost:18083/`（`localhost` 可替换为你的 IP 地址）访问 [EMQX Dashboard](../dashboard/introduction.md)，从中你可以连接到客户端或查看运行状态。

默认的用户名和密码为 `admin` 和 `public`。登录后，你将被要求更改默认密码。

要停止 EMQX：

- 如果以守护进程模式启动，使用 `emqx stop` 或 `bin/emqx stop`。
- 如果以前台模式启动，按下 Ctrl+C。
- 如果以交互模式启动，连续按下两次 Ctrl+C。

{% endemqxce %}

{% emqxee %}

## 安装 EMQX

1. 下载 [emqx-enterprise-@EE_VERSION@-macos12-arm64.zip](https://www.emqx.com/zh/downloads/enterprise/@EE_VERSION@/emqx-enterprise-@EE_VERSION@-macos12-arm64.zip)。

```bash
wget https://www.emqx.com/zh/downloads/enterprise/@EE_VERSION@/emqx-enterprise-@EE_VERSION@-macos12-arm64.zip
```

2. 安装 EMQX。

```bash
mkdir -p emqx && unzip emqx-enterprise-@EE_VERSION@-macos12-arm64.zip -d emqx
```

## 启动和停止 EMQX

EMQX 可以以守护进程模式、前台模式或交互模式启动。请注意，默认配置下只能同时运行一个 EMQX 实例。

```bash
# 以守护进程模式启动
emqx start

# 以前台模式启动
emqx foreground

# 以交互模式启动，带有 Erlang shell
emqx console
```

如果以前台或交互模式启动，启动成功后，EMQX 将输出以下消息：

```bash
EMQX Enterprise @EE_VERSION@ is running now！
```

您可能还会看到一些警告消息，这些消息是为了生产环境的运维人员而设计的，如果 EMQX 用于本地环境的测试、实验或客户端开发，可以忽略这些警告消息：

```bash
ERROR: DB Backend is RLOG, but an incompatible OTP version has been detected. Falling back to using Mnesia DB backend.
WARNING: ulimit -n is 256; 1024 is the recommended minimum.
WARNING: Default (insecure) Erlang cookie is in use.
WARNING: Configure node.cookie in /path/to/emqx/etc/emqx.conf or override from environment variable EMQX_NODE__COOKIE
WARNING: NOTE: Use the same cookie for all nodes in the cluster.
```

您可以使用以下命令检查 EMQX 的状态：

```bash
emqx ctl status
```

打开您的网页浏览器，在地址栏中输入 `http://localhost:18083/`（`localhost` 可替换为您的 IP 地址）访问 [EMQX Dashboard](../dashboard/introduction.md)，您可以在控制台中连接客户端或检查运行状态。

默认的用户名和密码为 `admin` 和 `public`。登录后，系统将提示您修改默认密码。

要停止 EMQX：

- 如果以守护进程模式启动，使用 `emqx stop` 或 `bin/emqx stop`。
- 如果以前台模式启动，按下 Ctrl+C。
- 如果以交互模式启动，连续按下两次 Ctrl+C。

{% endemqxee %}
