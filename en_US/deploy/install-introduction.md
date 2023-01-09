# Overview

## File and Directory Locations

EMQX 安装完成后会创建一些目录用来存放运行文件和配置文件，存储数据以及记录日志。

不同安装方式得到的文件和目录位置有所不同，具体如下:

| Description        | tar.gz       | RPM/DEB                  |
| ------------------ | ------------ | ------------------------ |
| Config files       | `./etc`      | `/etc/emqx/etc`          |
| Database and files | `./data`     | `/var/lib/emqx/data`     |
| Log files          | `./log`      | `/var/log/emqx`          |
| Boot instructions  | `./releases` | `/usr/lib/emqx/releases` |
| Executables        | `./bin`      | `/usr/lib/emqx/bin`      |
| Erlang code        | `./lib`      | `/usr/lib/emqx/lib`      |
| Erlang runtime     | `./erts-*`   | `/usr/lib/emqx/erts-*`   |
| Plugins            | `./plugins`  | `/usr/lib/emqx/plugins`  |

::: tip
The `data` directory is recommended to be mounted on a high-performance hard drive.
:::

### bin 目录

**emqx、emqx.cmd**

EMQX 的可执行文件，具体使用可以查看 [基本命令](../admin/cli.md)。

**emqx_ctl、emqx_ctl.cmd**

EMQX 管理命令的可执行文件，具体使用可以查看  [管理命令 CLI](../admin/cli.md)。

### etc: config files

EMQX only reads the files in this directory.

* `emqx.conf`: the bootstrapping config file for EMQX
* `vm.args`: The boot arguments for Erlang virtual machine
* `certs/`: X.509 keys are certificate files for EMQX SSL listeners or SSL clients when
  integrating with external systems. e.g., HTTPS client for webhooks

### data: database and files

This directory is for EMQX to persist its state. Please make sure EMQX has read/write permissions for all files in this directory.

A list of the subdirectories

* `authz`: File authorization rules uploaded from HTTP API or dashboard.
* `certs`: Certificate files uploaded from HTTP API or dashboard.
* `configs`: Generated config file at boot, or config overrides when changed from API or CLI.
* `mnesia`: The built-in database. Inside this directory, there should be one and only one subdirectory named
   after the node, e.g., `emqx@127.0.0.1`. The old directory should be deleted or moved elsewhere if the node is renamed.
* `patches`: Put `.beam` files here for EMQX to load as a hot patch. Handy to remedy urgent issues.
* `trace`: Online tracing log files.

生产环境中建议定期备份除 `trace` 之外的所有目录，以下是子目录和文件说明：

**mnesia**

Mnesia 数据库是 Erlang 内置的一个分布式 DBMS，可以直接存储 Erlang 的各种数据结构。 It also often referred to as the `built-in database` in EMQX documents.

EMQX 使用 Mnesia 数据库存储自身运行数据，例如告警记录、客户端认证与权限数据、Dashbaord 用户信息等数据，这些数据都存储在 `mnesia` 目录下，**一旦删除该目录，所有业务数据将丢失。**

可以通过 `emqx_ctl mnesia` 命令查询 EMQX 中 Mnesia 数据库的系统信息，具体请查看 [管理命令 CLI](../admin/cli.md)。

**configs/app.*.config**

EMQX 读取 `etc/emqx.conf` 和 `data/configs/cluster-override.conf` `data/configs/local-override.conf` 中的配置后，将其合并并转换为 Erlang 原生配置文件格式，以在运行时读取其中的配置。

不要与 `etc` 目录混淆，`etc` 目录存储只读的配置文件，通过 Dashboard 以及 REST API 提交的配置将被保存到 `data/configs` 目录下，以支持在运行时更改配置。

**trace**

EMQX trace 输出结果，trace 可用于调试和排查错误，具体请查看 [日志追踪](../observability/tracer.md)。

### log directory

**emqx.log.***

EMQX 运行时产生的日志文件，具体请查看 [日志与追踪](../observability/log.md)。

**erlang.log.***

以 `emqx start` 方式后台启动 EMQX 时，控制台日志的副本文件。
