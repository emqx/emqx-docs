# 概览

本章将向您介绍如何下载和安装 EMQX，运行 EMQX 所需满足的最低硬件规则，以及安装完成后 EMQX 所在目录的文件夹结构，以便后续的配置和维护操作。

## 下载

{% emqxce %}

在每个 EMQX 版本中，我们都会针对不同的操作系统与平台发布不同的安装包选项，您可点击以下链接下载：

- 官网下载：<https://www.emqx.io/zh/downloads>
- GitHub Release：<https://github.com/emqx/emqx/releases>

此外，您还可在我们的 GitHub Release 页面下载 alpha、beta、rc 等版本的安装包。

{% endemqxce %}

{% emqxee %}
EMQX 每个版本会发布各个操作系统与平台程序包以及 Docker 镜像，并在 EMQ 官网提供下载，您可点击以下链接下载：

官网下载：<https://www.emqx.com/zh/try?product=enterprise>
{% endemqxee %}

:::tip
除了私有部署外，我们也提供了全托管的 EMQX Cloud 服务，您只需几步注册即可轻松体验 EMQX 提供的 MQTT 消息服务，欢迎前往 [EMQX Cloud 门户](https://cloud.emqx.com/)页面免费试用。
:::

## 支持的操作系统与平台

EMQX 可以跨平台的在多种操作系统和硬件平台上运行，以下是支持情况：

| 操作系统                                  | 支持版本                 | x86_64/amd64 | arm64 (Apple Silicon) |
| :---------------------------------------- | :----------------------- | :----------- | :-------------------- |
| [Ubuntu](./install-ubuntu.md)             | Ubuntu18.04, Ubuntu20.04 | 是           | 是                    |
| [Debian](./install-debian.md)             | Debian10, Debian11       | 是           | 是                    |
| [CentOS/RHEL](./install-centos.md)        | CentOS 7, CentOS 8       | 是           | 是                    |
| [Amazon Linux](./install-amazon-linux.md) | -                        | 是           | 是                    |
| [macOS](./install-macOS.md)               | macOS11, macOS12         | 是           | 是                    |
| [Windows](./install-windows.md)           | Windows Server 2019      | 是           | 否                    |

## 硬件规格

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

:::

## 文件和目录位置

EMQX 安装完成后会创建一些目录用来存放运行文件和配置文件，存储数据以及记录日志。

不同安装方式得到的文件和目录位置有所不同，具体如下:

| 目录       | 描述              | 压缩包解压安装 | 二进制包安装             |
| ---------- | ----------------- | -------------- | ------------------------ |
| `etc`      | 配置文件目录      | `./etc`        | `/etc/emqx/etc`          |
| `data`     | 数据文件          | `./data`       | `/var/lib/emqx/data`     |
| `log`      | 日志文件          | `./log`        | `/var/log/emqx`          |
| `releases` | 启动相关的脚本    | `./releases`   | `/usr/lib/emqx/releases` |
| `bin`      | 可执行文件目录    | `./bin`        | `/usr/lib/emqx/bin`      |
| `lib`      | Erlang 代码       | `./lib`        | `/usr/lib/emqx/lib`      |
| `erts-*`   | Erlang 虚拟机文件 | `./erts-*`     | `/usr/lib/emqx/erts-*`   |
| `plugins`  | 插件              | `./plugins`    | `/usr/lib/emqx/plugins`  |

::: tip

1. 压缩包解压安装时，目录相对于软件所在目录；
2. Docker 容器使用压缩包解压安装的方式，软件安装于 `/opt/emqx` 目录中；
3. `data`、`log`、`plugins` 目录可以通过配置文件设置，建议将 `data` 目录挂载至高性能磁盘以获得更好的性能。
   :::

接下来我们将详细介绍下其中的部分目录，其中包含的文件和子文件夹。

### bin 目录

存放可执行文件的目录，其中包括：

- `emqx`、`emqx.cmd`：EMQX 的可执行文件，具体使用可以查看 [基本命令](../admin/cli.md)。
- `emqx_ctl`、`emqx_ctl.cmd`：EMQX 管理命令的可执行文件，具体使用可以查看 [管理命令 CLI](../admin/cli.md)。

### etc: 配置文件目录

存放 EMQX 配置文件，其中包括:

- `emqx.conf`：EMQX 的主配置文件，默认包含常用的配置项。
- `emqx-example-en.conf`：EMQX 示例配置文件，包含所有可选的配置项。
- `acl.conf`：默认 ACL 规则。
- `vm.args`：Erlang 虚拟机的运行参数。
- `certs/`：X.509 的密钥和证书文件。这些文件被用于 EMQX 的 SSL/TLS 监听器；当要与和外部系统集成时，也可用于建立 SSL/TLS 连接。

### data: 数据库与文件目录

`data` 目录用于存放 EMQX 的运行数据，请确保 EMQX 具有该目录下所有文件的读写权限。`data` 目录中主要的目录和文件包括:

- `authz`: Dashboard 或 REST API 上传的 [基于文件进行授权](../access-control/authz/file.md) 规则内容。
- `certs`: Dashboard 或 REST API 上传的证书。
- `configs`: 启动时生成的配置文件，或者从 Dashboard/REST API/CLI 进行功能设置时覆盖的配置文件。
- `mnesia`: 内置数据库目录。在 `data` 目录中唯一一个以节点命名的子目录，如 `emqx@127.0.0.1`。如果节点被重新命名，应该将旧的目录删除或移动到其他地方。
- `patches`: 将 `.beam` 文件放在这里，使其作为 EMQX 的一个热补丁加载，用于补丁修复。
- `trace`: 在线日志追踪文件目录

生产环境中建议定期备份除 `trace` 之外的所有目录，以下是子目录和文件说明：

#### mnesia

Mnesia 数据库是 Erlang 内置的一个分布式数据库管理系统（DBMS），可以直接存储 Erlang 的各种数据结构，在文档中也被叫做内置数据库。

EMQX 使用 Mnesia 数据库存储自身运行数据，例如告警记录、客户端认证与权限数据、Dashboard 用户信息等数据，这些数据都存储在 `mnesia` 目录下，**一旦删除该目录，所有业务数据将丢失。**

可以通过 `emqx_ctl mnesia` 命令查询 EMQX 中 Mnesia 数据库的系统信息，具体请查看 [管理命令 CLI](../admin/cli.md)。

#### configs/app.*.config

EMQX 的配置项存储在 `etc/emqx.conf`、`data/configs/cluster-override.conf`和 `data/configs/local-override.conf` 中，EMQX 会读取其中的配置并将其合并转化为Erlang 原生配置文件格式，以便在运行时应用这些配置。

`data/configs` 与 `etc` 目录的主要区别是，`etc` 目录存储只读的配置文件，用户通过 Dashboard 和 REST API 提交的配置将被保存到 `data/configs` 目录下，并支持在运行时进行热更新。

#### trace

该文件夹用于保存 EMQX 的日志追踪结果，可用于调试和排查错误等场景，更多信息，可查看 [日志追踪](../observability/tracer.md)。

### log 目录

**emqx.log.\***

EMQX 运行时产生的日志文件，具体请查看 [日志与追踪](../observability/log.md)。

**erlang.log.\***

以 `emqx start` 方式后台启动 EMQX 时，控制台日志的副本文件。
