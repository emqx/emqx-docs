# Overview

This chapter will walk you through the basic installtion steps for EMQX, the miminum hardware specifications, and the file and directory locations to facilitate future configuration and maintenance jobs. 

## Download

{% emqxce %}

EMQX will release the installtion packages for different operating systems or platforms in each release, you may click the line below to download. 

- EMQX website: <https://www.emqx.io/zh/downloads>
- GitHub Release: <https://github.com/emqx/emqx/releases>

You can also download the alpha, beta, or rc versions from our Github pages. 
{% endemqxce %}

{% emqxee %}

EMQX will release the corresponding docker image and the installtion packages for different operating systems or platforms in each release, you may click the line below to download. 

EMQX website: <https://www.emqx.com/zh/try?product=enterprise>
{% endemqxee %}

:::tip

Besides the above deployment methods, you are also welcome to try our [EMQX Cloud](https://www.emqx.com/en/cloud), a fully managed MQTT service for IoT. You will only need to [register for an account](https://www.emqx.com/en/signup?continue=https://www.emqx.com/en/cloud) before you can start your MQTT services and connect your IoT devices to any cloud with zero need for infrastructure maintenance.
:::

## Supported operating systems

The table below lists the operating system and versions that EMQX support. 

| Operating system                          | Versions supported       | x86_64/amd64 | arm64 (Apple Silicon) |
| :---------------------------------------- | :----------------------- | :----------- | :-------------------- |
| [Ubuntu](./install-ubuntu.md)             | Ubuntu18.04, Ubuntu20.04 | Yes          | Yes                   |
| [Debian](./install-debian.md)             | Debian10, Debian11       | Yes          | Yes                   |
| [CentOS/RHEL](./install-centos.md)        | CentOS 7, CentOS 8       | Yes          | Yes                   |
| [Amazon Linux](./install-amazon-linux.md) | -                        | Yes          | Yes                   |
| [macOS](./install-macOS.md)               | macOS11, macOS12         | Yes          | Yes                   |
| [Windows](./install-windows.md)           | Windows Server 2019      | Yes          | No                    |

## Hardware specification

Depends on the number of client connections, message rate, message size, and enabled features, the minimum hardware needed to run EMQX varies. 

Here we list the minimum hardware specification for runing a simple EMQX function verification, that is, with 100,000 client connections and100,000 message throughput per second.

| Item           | Mininum configuration | Recommended configuration |
| -------------- | --------------------- | ------------------------- |
| **Node**       | 1                     | 2                         |
| **CPU**        | 1 core                | 16 core                   |
| **Memory**     | 512 MB                | 32 GB                     |
| **Disk space** | 1 GB                  | 50 GB                     |

::: tip

Note: In production environment, you may use our [Server Estimate](https://www.emqx.com/en/server-estimate) calculator to calculate the recommended hardware specification under various maximum connections and message throughputs.

:::

## File and directories

After the installation is complete, EMQX will create some directories to store running and configuration files, data, and logs.

The table below lists the directories created and their file path under different installation methods:

| Directory  | Description        | Installed with tar.gz | Installed with RPM/DEB   |
| ---------- | ------------------ | --------------------- | ------------------------ |
| `etc`      | Config files       | `./etc`               | `/etc/emqx/etc`          |
| `data`     | Database and files | `./data`              | `/var/lib/emqx/data`     |
| `log`      | Log files          | `./log`               | `/var/log/emqx`          |
| `releases` | Boot instructions  | `./releases`          | `/usr/lib/emqx/releases` |
| `bin`      | Executables        | `./bin`               | `/usr/lib/emqx/bin`      |
| `lib`      | Erlang code        | `./lib`               | `/usr/lib/emqx/lib`      |
| `erts-*`   | Erlang runtime     | `./erts-*`            | `/usr/lib/emqx/erts-*`   |
| `plugins`  | Plugins            | `./plugins`           | `/usr/lib/emqx/plugins`  |

::: tip
It is recommended to mount the `data` director on a high-performance hard drive for better performance.
:::

Below will introduce the files and subfolders of some directories. 

### Direcotory `bin`

`bin` is the directory where all executables are stored, including:

- `emqx` and `emqx.cmd`: Executables of EMQX. For details, see [basic commands](../admin/cli.md).
- `emqx_ctl` and `emqx_ctl.cmd`: Executables of EMQX administration commands. For details, see [administration CLI commands](../admin/cli.md).

### Directory `etc`

This is the directory that holds all the configuration files, including:

* `emqx.conf`: Main configuration file for EMQX, contains all the commonly-used configuration items.
* `emqx-example-en.conf`: Demo configuration files of EMQX, contains all the configurable items;
* `acl.conf`: Default ACl rules.
* `vm.args`: Operating parameters of the Erlang virtual machine.
* `certs/`: X.509 keys and certificate files for EMQX SSL listeners, may also be used in the SSL/TLS connection when intergrating with external systems.

### Directory `data`: 

This directory is where EMQX stores its operating data, please ensure EMQX has read/write permissions for all files in this directory. This directory includes: 

* `authz`: Stores file authorization rules uploaded by HTTP API or Dashboard.
* `certs`: Stores certificate files uploaded by HTTP API or Dashboard.
* `configs`: Stores Generated config file at boot, or config overrides when changed from API or CLI.
* `mnesia`: The built-in database, one subdirectory will be generated for every node and will be named after this ndoe, e.g., `emqx@127.0.0.1`. Note: For nodes that are renamed, you should delete the subdirectory corresponding to this node or move it away from the directory. 
* `patches`: Stores the `.beam` files for EMQX to load as a hot patch. Can be used for a quick fix.
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

### Directory `log`

This directory stores the operating logs of EMQX, including

- ``emqx.log.*`: Operation logs of EMQX, for more information, see [logs日志与追踪](../observability/log.md)。
- `erlang.log.*`: Copy file of the console log when EMQX is started in the background with `emqx start`
