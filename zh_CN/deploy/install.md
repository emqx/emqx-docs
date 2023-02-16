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
EMQX 每个版本会发布各个操作系统与平台程序包以及 Docker 镜像，并在 EMQX 官网提供下载，您可点击以下链接下载：

官网下载：<https://www.emqx.com/zh/try?product=enterprise>

{% endemqxee %}

:::tip
除了私有部署外，我们也提供了全托管的 EMQX Cloud 服务，您只需几步注册即可轻松体验 EMQX 提供的 MQTT 消息服务，欢迎前往 [EMQX Cloud 门户](https://cloud.emqx.com/)页面免费试用。
:::

## 支持的操作系统与平台

EMQX 可以跨平台的在多种操作系统和硬件平台上运行，以下是支持情况：

{% emqxce %}
| 操作系统                                  | 支持版本                 | x86_64/amd64 | arm64 |
| :---------------------------------------- | :----------------------- | :----------- | :---- |
| [Ubuntu](./install-ubuntu.md)             | Ubuntu 18.04<br>Ubuntu 20.04<br/>Ubuntu 22.04 | 是           | 是    |
| [Debian](./install-debian.md)             | Debian 10<br/>Debian 11 | 是           | 是    |
| [CentOS/RHEL](./install-centos.md)        | CentOS 7<br/>CentOS 8 | 是           | 是    |
| [Amazon Linux](./install-amazon-linux.md) | -                        | 是           | 是    |
| [macOS](./install-macOS.md)               | macOS 11<br/>macOS 12 | 是           | 是    |
| [Windows](./install-windows.md)           | -                        | 是           | 是    |

{% endemqxce %}

{% emqxee %}

| 操作系统                                  | 支持版本                 | x86_64/amd64 | arm64 |
| :---------------------------------------- | :----------------------- | :----------- | :---- |
| [Ubuntu](./install-ubuntu.md)             | Ubuntu 18.04<br/>Ubuntu 20.04 | 是           | 是    |
| [Debian](./install-debian.md)             | Debian 10<br/>Debian 11 | 是           | 是    |
| [CentOS/RHEL](./install-centos.md)        | CentOS 7<br/>CentOS 8 | 是           | 是    |
| [Amazon Linux](./install-amazon-linux.md) | -                        | 是           | 是    |
| [macOS](./install-macOS.md)               | macOS 11<br/>macOS 12  | 是           | 是    |

{% endemqxee %}

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

## 文件和目录

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

| 目录   | 描述                 | 权限 | 目录文件                                                     |
| ------ | -------------------- | ---- | ------------------------------------------------------------ |
|bin | 存放可执行文件       | 读   | `emqx`、`emqx.cmd`：EMQX 的可执行文件，具体使用可以查看 [基本命令](../admin/cli.md)。<br><br/> `emqx_ctl`、`emqx_ctl.cmd`：EMQX 管理命令的可执行文件，具体使用可以查看 [管理命令 CLI](../admin/cli.md)。 |
|etc | 存放配置文件         | 读   |`emqx.conf`：EMQX 的主配置文件，默认包含常用的配置项。<br/><br/>`emqx-example-en.conf`：EMQX 示例配置文件，包含所有可选的配置项。<br/><br/>`acl.conf`：默认 ACL 规则。<br/><br/>`vm.args`：Erlang 虚拟机的运行参数。<br/><br/>`certs/`：X.509 的密钥和证书文件。这些文件被用于 EMQX 的 SSL/TLS 监听器；当要与和外部系统集成时，也可用于建立 SSL/TLS 连接。 |
|data | 存放 EMQX 的运行数据 | 写   |`authz`：Dashboard 或 REST API 上传的 [基于文件进行授权](../access-control/authz/file.md) 规则内容。<br/><br/>`certs`：Dashboard 或 REST API 上传的证书。<br/><br/>`configs`：启动时生成的配置文件，或者从 Dashboard/REST API/CLI 进行功能设置时覆盖的配置文件。<br/><br/>`mnesia`：内置数据库目录，用于存储自身运行数据，例如告警记录、客户端认证与权限数据、Dashboard 用户信息等数据，**一旦删除该目录，所有业务数据将丢失。**<br/><br/>  —  可包含以节点命名的子目录，如 `emqx@127.0.0.1`；如节点被重新命名，应手动将旧的目录删除或移走。<br/><br/>  —  可通过 `emqx_ctl mnesia` 命令查询 EMQX 中 Mnesia 数据库的系统信息，具体请查看 [管理命令 CLI](../admin/cli.md)。<br/><br/>`patches`：用于存储热补丁 `.beam` 文件，用于补丁修复。<br/><br/>`trace`: 在线日志追踪文件目录。<br /><br/><br/>在生产环境中，建议定期备份该文件夹下除 `trace` 之外的所有目录。 |
|log  | 日志文件             | 读   |`emqx.log.*`：EMQX 运行时产生的日志文件，具体请查看 [日志与追踪](../observability/log.md)。<br/><br/>`erlang.log.*`：当以 `emqx start` 方式后台启动 EMQX 时，控制台日志的副本文件。 |

​                                  |

:::tip

EMQX 的配置项存储在 `etc` 和 `data/configs` 目录下，二者的主要区别是 `etc` 目录存储**只读**的配置文件，用户通过 Dashboard 和 REST API 提交的配置将被保存到 `data/configs` 目录下，并支持在运行时进行热更新。

- `etc/emqx.conf`
- `data/configs/cluster-override.conf`
- `data/configs/local-override.conf` 

EMQX 读取这些配置并将其合并转化为 Erlang 原生配置文件格式，以便在运行时应用这些配置。

:::
