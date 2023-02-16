# Overview

This chapter will walk you through the basic installation steps for EMQX, the minimum hardware specification, and the file and directory locations to facilitate future configuration and maintenance jobs.

## Download

{% emqxce %}

EMQX will release the installation packages for different operating systems or platforms in each release. You may click the line below to download.

- EMQX website: <https://www.emqx.io/zh/downloads>
- GitHub Release: <https://github.com/emqx/emqx/releases>

You can also download the alpha, beta, or rc versions from our GitHub pages.
{% endemqxce %}

{% emqxee %}

EMQX will release the corresponding Docker image and the installation packages for different operating systems or platforms in each release. You may click the line below to download.

EMQX website: <https://www.emqx.com/zh/try?product=enterprise>

{% endemqxee %}

:::tip

Besides the above deployment methods, you are also welcome to try our [EMQX Cloud](https://www.emqx.com/en/cloud), a fully managed MQTT service for IoT. You will only need to [register for an account](https://www.emqx.com/en/signup?continue=https://www.emqx.com/en/cloud) before starting your MQTT services and connecting your IoT devices to any cloud with zero need for infrastructure maintenance.
:::

## Supported operating systems

The table below lists the operating systems and versions that EMQX supports.

{% emqxce %}
| Operating system                          | Versions supported       | x86_64/amd64 | arm64 |
| :---------------------------------------- | :----------------------- | :----------- | :---- |
| [Ubuntu](./install-ubuntu.md)             | Ubuntu 18.04<br>Ubuntu 20.04<br/>Ubuntu 22.04 | Yes          | Yes   |
| [Debian](./install-debian.md)             | Debian 10<br/>Debian 11 | Yes          | Yes   |
| [CentOS/RHEL](./install-centos.md)        | CentOS 7<br/>CentOS 8  | Yes          | Yes   |
| [Amazon Linux](./install-amazon-linux.md) | -                        | Yes          | Yes   |
| [macOS](./install-macOS.md)               | macOS 11<br/>macOS 12  | Yes          | Yes   |
| [Windows](./install-windows.md)           | -                        | Yes          | Yes   |

{% endemqxce %}

{% emqxee %}

| Operating system                          | Versions supported       | x86_64/amd64 | arm64 |
| :---------------------------------------- | :----------------------- | :----------- | :---- |
| [Ubuntu](./install-ubuntu.md)             | Ubuntu 18.04<br/>Ubuntu 20.04 | Yes          | Yes   |
| [Debian](./install-debian.md)             | Debian 10<br/>Debian 11 | Yes          | Yes   |
| [CentOS/RHEL](./install-centos.md)        | CentOS 7<br/>CentOS 8  | Yes          | Yes   |
| [Amazon Linux](./install-amazon-linux.md) | -                        | Yes          | Yes   |
| [macOS](./install-macOS.md)               | macOS 11<br/>macOS 12  | Yes          | Yes   |

{% endemqxee %}

## Hardware specification

Depending on the number of client connections, message rate, message size, and enabled features, the minimum hardware specification for EMQX varies.

Here is the minimum hardware specification for running a simple EMQX function verification, supporting 100,000 client connections and 100,000 message throughput per second.

| Item           | Minimum configuration | Recommended configuration |
| -------------- | --------------------- | ------------------------- |
| **Node**       | 1                     | 2                         |
| **CPU**        | 1 core                | 16 core                   |
| **Memory**     | 512 MB                | 32 GB                     |
| **Disk space** | 1 GB                  | 50 GB                     |

::: tip

Note: In production environments, you can use the [Server Estimate](https://www.emqx.com/en/server-estimate) calculator to calculate the recommended hardware specification under various maximum connections and message throughputs.

:::

## Files and directories

After installation, EMQX will create some directories to store running and configuration files, data, and logs. The table below lists the directories created and their file path under different installation methods:

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

1. When installed with the compressed package, the directory is relative to the directory where the software is installed;
2. When installed with Docker container, EMQX will be installed in the `/opt/emqx` directory;
3. The `data`, `log`, and `plugins` directories are configurable via the configuration files. Mounting the `data` directory to a high-performance disk is recommended for better performance.
   :::

Below will introduce the files and subfolders of some directories.

| Directory | Description         | Permissions | Files                                                        |
| --------- | ------------------- | ----------- | ------------------------------------------------------------ |
| bin       | Executables         | Read        | `emqx` and `emqx.cmd`: Executables of EMQX. For details, see [basic commands](../admin/cli.md).<br><br/>`emqx_ctl` and `emqx_ctl.cmd`: Executables of EMQX administration commands. For details, see [administration CLI commands](../admin/cli.md). |
| etc       | Configuration files | Read        | `emqx.conf`: Main configuration file for EMQX, contains all the commonly-used configuration items.<br/><br/>`emqx-example-en.conf`: Demo configuration files of EMQX, contains all the configurable items.<br/><br/>`acl.conf`: Default ACl rules.<br/><br/>`vm.args`: Operating parameters of the Erlang virtual machine.<br/><br/>`certs/`: X.509 keys and certificate files for EMQX SSL listeners, may also be used in the SSL/TLS connection when integrating with external systems. |
| data      | Operating data      | Write       | `authz`: Stores file authorization rules uploaded by REST API or Dashboard. For details, see [Authorization - File](../access-control/authz/file.md). <br/><br/>`certs`: Stores certificate files uploaded by REST API or Dashboard.<br/><br/>`configs`: Stores configuration files generated at boot, or configuration overrides by changes from API or CLI.<br/><br/>`mnesia`: Built-in database to store EMQX operating data, including alarm records, authentication and authorization data of the clients, Dashboard user information, etc. **If the directory is deleted, all these operating data will be lost.**<br/><br/>  —  May contain subdirectories named after different node, e.g., `emqx@127.0.0.1`. Note: In case of node renaming, please also delete or remove the corresponding subdirectory. <br/><br/>  —  Can use command `emqx_ctl mnesia` to  query the built-in database. For details, see [Management Command CLI](https://docs.emqx.com/en/enterprise/v4.4/advanced/cli.html).<br/><br/>`patches`: Stores the `.beam` files for EMQX to load as a hot patch. Can be used for a quick fix.<br/><br/>`trace`: Online tracing log files.<br /><br/>In production, it is recommended to periodically backup the `data` directory (excluding the `trace` folder )  for data safety. |
| log       | Operating logs      | Read        | `emqx.log.*`: Operation logs of EMQX, for more information, see [logs](../observability/log.md).<br/><br/>`erlang.log.`: Copy file of the console log when EMQX is started in the background with `emqx start` |

:::tip

EMQX stores the configuration information in the `data/configs` and the `etc` directory. The `etc` directory stores read-only configuration files, while configuration updates from the Dashboard or REST API are saved in the `data/configs` directory to support hot configuration reloads at runtime.

- `etc/emqx.conf`
- `data/configs/cluster-override.conf`
- `data/configs/local-override.conf` 

EMQX reads the configuration items from these files and converts them to the Erlang native configuration file format, so as to apply the configurations at runtime.
