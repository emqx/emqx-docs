# macOS

本页将指导您如何通过 zip 包在 macOS 系统中下载安装并启动最新版 EMQX。

支持的操作系统：

- macOS 14
- macOS 13

如果您想安装不同版本或在不同系统中安装，请访问 [EMQX 企业版下载页面](https://www.emqx.com/zh/downloads-and-install/enterprise)。

## 安装 EMQX

1. 前往官方下载页面，选择 [macOS 页签](https://www.emqx.com/zh/downloads-and-install/enterprise?os=macOS)。
2. 选择最新版本 `@EE_VERSION@`，在**安装包类型**中根据需要的版本和 CPU 架构选择 `zip` 安装包。
3. 点击下方的下载链接。您也可以参照命令行指导步骤进行下载与安装。

## 启动和停止 EMQX

EMQX 可以以守护进程模式、前台模式或交互模式启动。请注意，默认配置下只能同时运行一个 EMQX 实例。

```bash
# 以守护进程模式启动
./bin/emqx start

# 以前台模式启动
./bin/emqx foreground

# 以交互模式启动，带有 Erlang shell
./bin/emqx console
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
./bin/emqx ctl status
```

打开您的网页浏览器，在地址栏中输入 `http://localhost:18083/`（`localhost` 可替换为您的 IP 地址）访问 [EMQX Dashboard](../dashboard/introduction.md)，您可以在控制台中连接客户端或检查运行状态。

默认的用户名和密码为 `admin` 和 `public`。登录后，系统将提示您修改默认密码。

要停止 EMQX：

- 如果以守护进程模式启动，使用 `emqx stop` 或 `bin/emqx stop`。
- 如果以前台模式启动，按下 Ctrl+C。
- 如果以交互模式启动，连续按下两次 Ctrl+C。
