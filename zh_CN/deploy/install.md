# 安装部署和迁移

本章将向您介绍如何下载和安装 EMQX，支持的操作系统和平台，以及安装完成后 EMQX 所在目录的文件夹结构，以便后续的配置和维护操作。本章还介绍了 EMQX 企业版 License 的配置，以及如何从 EMQX 4.4 迁移至 EMQX 5.1。

## 下载

EMQX 每个版本会发布各个操作系统与平台程序包以及 Docker 镜像，并在 EMQX 官网提供下载，您可以点击以下链接下载：

EMQX 开源版：<https://www.emqx.com/zh/downloads-and-install/broker>

EMQX 企业版：<https://www.emqx.com/zh/downloads-and-install/enterprise>

此外，您还可在我们的 [GitHub Release 页面](https://github.com/emqx/emqx/releases) 下载 alpha、beta、rc 等版本的安装包。

:::tip
除了私有部署外，我们也提供了全托管的 EMQX Cloud 服务，您只需几步注册即可轻松体验 EMQX 提供的 MQTT 消息服务，欢迎前往 [EMQX Cloud 门户](https://cloud.emqx.com/)页面免费试用。
:::

## 支持的操作系统与平台

EMQX 可以跨平台的在多种操作系统和硬件平台上运行，以下是支持情况：

| 操作系统                          | 支持版本                 | x86_64/amd64 | arm64 |
| :-------------------------------- | :----------------------- | :----------- | :---- |
| Ubuntu     | Ubuntu 18.04<br />Ubuntu 20.04<br />Ubuntu 22.04<br />Ubuntu 24.04 | 是   | 是  |
| Debian     | Debian 10<br />Debian 11<br />Debian 12          | 是   | 是  |
| CentOS/RHEL  | CentOS 7<br />Rocky Linux 8<br />Rocky Linux 9   | 是   | 是  |
| Amazon Linux | Amazon Linux 2<br />Amazon Linux 2023            | 是   | 是  |
| macOS       | macOS 13<br />macOS 14 | 是   | 是  |

<!-- ## 硬件规格

EMQX 的硬件要求根据客户端连接数、消息消息速率和消息大小以及启用的功能而异。
下面的最低硬件规格适用于运行 EMQX 并进行简单的功能验证，推荐配置能够支撑 10 万客户端连接以及每秒 10 万条消息吞吐。

| 项目         | 最低要求 | 推荐配置 |
| ------------ | -------- | -------- |
| **节点数**   | 1        | 2        |
| **CPU**      | 1 核     | 16 核    |
| **内存**     | 512 MB   | 32 GB    |
| **磁盘空间** | 1 GB     | 50 GB    |

::: tip

在生产环境中，您可通过我们的[配置估算计算器](https://www.emqx.com/zh/server-estimate)来计算不同连接与消息吞吐下的推荐硬件规格。

::: -->

## 安装环境

EMQX 所使用的 Erlang 虚拟机依赖于系统区域设置来启用各种功能的 Unicode 支持，包括交互式 Erlang Shell 中的[文件名](https://www.erlang.org/doc/apps/stdlib/unicode_usage.html#unicode-filenames)和[终端 IO](https://www.erlang.org/doc/apps/stdlib/unicode_usage.html#the-interactive-shell)。

如果您使用的是 Linux 操作系统，在启动 EMQX 前建议确认系统环境中已启用了 UTF-8 区域设置。关于如何在不同平台上启用 UTF-8 区域设置，点击下列标签：

:::: tabs

::: tab Amazon Linux

使用 [`cloud-init`](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/amazon-linux-ami-basics.html#amazon-linux-cloud-init) 配置启用 UTF-8 locale 区域设置：

```bash
cat <<EOF | sudo tee /etc/cloud/cloud.cfg.d/99_locale.cfg
#cloud-config
locale: C.utf8
EOF
```

:::

::: tab CentOS

通过 systemd，通常可以使用 `localectl` 命令启用 UTF-8 locale 区域设置：

```bash
sudo localectl set-locale LANG=C.UTF-8
```

:::

::: tab Debian

UTF-8 区域设置可以通过两种方式启用。

- 通过 systemd，通常使用 [`localectl`](https://www.freedesktop.org/software/systemd/man/localectl.html) 命令启用：

  ```bash
  sudo localectl set-locale LANG=C.UTF-8
  ```

- 或者，使用 [`update-locale`](https://manpages.debian.org/buster/locales/update-locale.8.en.html) 命令启用：

  ```bash
  sudo update-locale LANG=C.UTF-8
  ```

:::

::: tab Ubuntu

可以使用 [`update-locale`](https://manpages.ubuntu.com/manpages/jammy/man8/update-locale.8.html) 命令启用：

```bash
sudo update-locale LANG=C.UTF-8
```

:::

::::

## 端口占用

EMQX 默认使用以下端口，请确保这些端口未被其他应用程序占用，并按照需求开放防火墙以保证 EMQX 正常运行。

| 端口  | 协议 | 描述 |
| ----- | ---- | ---- |
| 1883  | TCP  | MQTT over TCP 监听器端口，主要用于未加密的 MQTT 连接。 |
| 8883  | TCP  | MQTT over SSL/TLS 监听器端口，用于加密的 MQTT 连接。 |
| 8083  | TCP  | MQTT over WebSocket 监听器端口，使 MQTT 能通过 WebSocket 进行通信。 |
| 8084  | TCP  | MQTT over WSS (WebSocket over SSL) 监听器端口，提供加密的 WebSocket 连接。 |
| 18083 | HTTP  | EMQX Dashboard 和 REST API 端口，用于管理控制台和 API 接口。 |
| 4370  | TCP  | Erlang 分布式传输端口，根据节点名称不同实际端口可能是 `BasePort (4370) + Offset`。 |
| 5370  | TCP  | 集群 RPC 端口（在 Docker 环境下为 5369），根据节点名称不同实际端口可能是 `BasePort (5370) + Offset`。 |

::: tip 提示
即使没有组建集群，EMQX 也会监听 4370 跟 5370 端口。这 2 个端口固定无法修改，且会根据节点名称（`Name@Host`）中 Name 部分的数字后缀决定 Offset，没有数字后缀则默认为 0。更多信息请参考[集群内通信端口](./cluster/security.md#集群内通信端口)。
:::

## 文件和目录

EMQX 安装完成后会创建一些目录用来存放运行文件和配置文件，存储数据以及记录日志。

不同安装方式得到的文件和目录位置有所不同，具体如下:

| 目录       | 描述              | 压缩包解压安装 | 二进制包安装             |
| ---------- | ----------------- | -------------- | ------------------------ |
| `etc`      | 静态配置文件    | `./etc`        | `/etc/emqx`      |
| `data`     | 数据和配置文件       | `./data`       | `/var/lib/emqx`     |
| `log`      | 日志文件          | `./log`        | `/var/log/emqx`          |
| `releases` | 启动相关的脚本    | `./releases`   | `/usr/lib/emqx/releases` |
| `bin`      | 可执行文件    | `./bin`        | `/usr/lib/emqx/bin`      |
| `lib`      | Erlang 代码       | `./lib`        | `/usr/lib/emqx/lib`      |
| `erts-*`   | Erlang 虚拟机文件 | `./erts-*`     | `/usr/lib/emqx/erts-*`   |
| `plugins`  | 插件              | `./plugins`    | `/usr/lib/emqx/plugins`  |

::: tip

1. 压缩包解压安装时，目录相对于软件所在目录；
2. Docker 容器使用压缩包解压安装的方式，软件安装于 `/opt/emqx` 目录中；
3. `data`、`log`、`plugins` 目录可以通过配置文件设置，建议将 `data` 目录挂载至高性能磁盘以获得更好的性能。但对于属于同一集群的节点， `data` 目录的配置应该相同。更多关于集群的介绍，见[集群章节](./cluster/introduction.md)。
   :::

接下来我们将详细介绍下其中的部分目录，其中包含的文件和子文件夹。

| 目录   | 描述                 | 权限 | 目录文件                                                     |
| ------ | -------------------- | ---- | ------------------------------------------------------------ |
|bin | 存放可执行文件       | 读   | `emqx` 和`emqx.cmd`：EMQX 的可执行文件，具体使用可以查看[命令行接口](../admin/cli.md)。 |
|etc | 存放配置文件         | 读   |`emqx.conf`：EMQX 的主配置文件，默认包含常用的配置项。<br /><br />`emqx-example-en.conf`：EMQX 示例配置文件，包含所有可选的配置项。<br /><br />`acl.conf`：默认 ACL 规则。<br /><br />`vm.args`：Erlang 虚拟机的运行参数。<br /><br />`certs/`：X.509 的密钥和证书文件。这些文件被用于 EMQX 的 SSL/TLS 监听器；当要与和外部系统集成时，也可用于建立 SSL/TLS 连接。 |
|data | 存放 EMQX 的运行数据 | 写   |`authz`：Dashboard 或 REST API 上传的 [基于文件进行授权](../access-control/authz/file.md) 规则内容。<br /><br />`certs`：Dashboard 或 REST API 上传的证书。<br /><br />`configs`：启动时生成的配置文件，或者从 Dashboard/REST API/CLI 进行功能设置时覆盖的配置文件。<br /><br />`mnesia`：内置数据库目录，用于存储自身运行数据，例如告警记录、客户端认证与权限数据、Dashboard 用户信息等数据，**一旦删除该目录，所有业务数据将丢失。**<br /><br />  —  可包含以节点命名的子目录，如 `emqx@127.0.0.1`；如节点被重新命名，应手动将旧的目录删除或移走。<br /><br />  —  可通过 `emqx_ctl mnesia` 命令查询 EMQX 中 Mnesia 数据库的系统信息，具体请查看 [管理命令 CLI](../admin/cli.md)。<br /><br />`patches`：用于存储热补丁 `.beam` 文件，用于补丁修复。<br /><br />`trace`: 在线日志追踪文件目录。<br /><br /><br />在生产环境中，建议定期备份该文件夹下除 `trace` 之外的所有目录。 |
|log  | 日志文件             | 读   |`emqx.log.*`：EMQX 运行时产生的日志文件，具体请查看[日志](../observability/log.md)。|

:::tip

EMQX 的配置项存储在 `etc` 和 `data/configs` 目录下，二者的主要区别是 `etc` 目录存储**只读**的配置文件，用户通过 Dashboard 和 REST API 提交的配置将被保存到 `data/configs` 目录下，并支持在运行时进行热更新。

- `etc/emqx.conf`
- `data/configs/cluster.hocon`

EMQX 读取这些配置并将其合并转化为 Erlang 原生配置文件格式，以便在运行时应用这些配置。

:::
