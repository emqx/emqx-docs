# 备份与恢复

EMQX 集群能够确保系统的高可用性，并且采用分布式存储将数据复制到所有节点。

尽管如此，我们仍然应当做最坏的打算来确保数据万无一失，本章节指导您如何进行 EMQX 数据备份和恢复。

## 数据导入导出介绍

EMQX 5.1 引入了一个用户友好的命令行工具用于数据导入和导出。尽管与 EMQX 4.x 中提供的工具类似，但它具有一些显著的差异，并且与之不兼容。

在 EMQX 4.x 中，使用单个 JSON 文件携带了 EMQX 配置和内置数据库的所有必要数据。然而，在 EMQX 5.1 中，使用了一个不透明的压缩 tar 存档，这样可以更高效、更有结构地处理潜在的大量用户数据。

在 EMQX 5.1 中支持导入和导出的数据类似于 EMQX 4.x，包括：

* EMQX [配置重写](../configuration/configuration.md#配置重写)的内容：
  * 规则与桥接
  * 认证与授权配置
  * 监听器、网关配置
  * 其他 EMQX 配置
* 内置数据库 (Mnesia) 的数据
  * 仪表盘用户和 API 密钥
  * 用户身份凭证信息认证 （简单和增强）
  * PSK 认证数据
  * 授权规则
  * 禁用客户端
* 存储在 EMQX 数据目录（`node.data_dir`）中的 SSL/TLS 证书
* 存储在 EMQX 数据目录中的授权 acl.conf 文件

::: 特别提示

任何存储在 EMQX 数据目录之外的 SSL/TLS 证书或 acl.conf 文件不会包含在导出的存档中。因此，在导出和导入数据时，需要注意在导入数据之前在所有目标节点上预置这些文件。这样确保必要的 SSL/TLS 证书和 acl.conf 文件可以正常使用。

:::

## 导出

数据可以从任何运行的集群节点导出。

## 导入

要导入数据，EMQX 节点必须处于运行状态，并且需要满足一些条件才能成功进行导入操作：

* 如果启用了[核心节点 + 复制节点](../deploy/cluster/mria-introduction.md)模式，数据导入只能在核心节点上进行。这不会影响实际的导入行为，因为数据将被复制到所有集群节点，包括核心节点和复制节点。在核心节点上进行操作可以确保正确导入数据。
* 从 EMQX 企业版集群导出的数据无法导入到 EMQX 开源版集群。
* 数据文件不能被重命名。

如果上述任何条件不满足，导入过程将被中止，并显示对应的错误消息。

在数据导入操作期间，数据将被插入（如果在目标 EMQX 集群中不存在）或更新（如果存在冲突）到 EMQX 中。导入过程不会从 EMQX 集群中删除任何现有数据。

::: tip 特别提示

在极少数情况下，现有数据可能与导入的数据不兼容。例如，如果 EMQX 集群使用内置数据库的身份认证机制，并具有“后缀”盐位置，而导入的数据定义了具有“前缀”盐位置的相同身份认证源，那么在导入之前创建的旧用户身份凭证将不再起作用。因此，将数据导入到未清除数据的 EMQX 集群可能需要额外的注意。

:::

## 示例

本节介绍如何使用命令行界面导入和导出数据。

1. 导出数据。导出的文件名格式为`emqx-export-YYYY-MM-DD-HH-mm-ss.sss.tar.gz`，导出目录为`<EMQX data directory>/backup`：

   ```bash
   $ ./emqx_ctl data export
   Exporting data to "data/backup/emqx-export-2023-06-19-15-14-19.947.tar.gz"...
   Exporting cluster configuration...
   Exporting additional files from EMQX data_dir: "data"...
   Exporting built-in database...
   Exporting emqx_admin database table...
   Exporting emqx_authn_mnesia database table...
   Exporting emqx_enhanced_authn_scram_mnesia database table...
   Exporting emqx_app database table...
   Exporting emqx_acl database table...
   Exporting emqx_psk database table...
   Exporting emqx_banned database table...
   Data has been successfully exported to data/backup/emqx-export-2023-06-19-15-14-19.947.tar.gz.
   ```

2. 导入数据。导入的文件名应指定为绝对路径，例如：

   ```bash
   $ ./emqx_ctl data import /tmp/emqx-export-2023-06-19-15-14-19.947.tar.gz
   Importing data from "/tmp/emqx-export-2023-06-19-15-14-19.947.tar.gz"...
   Importing cluster configuration...
   Importing built-in database...
   Importing emqx_banned database table...
   Importing emqx_psk database table...
   Importing emqx_acl database table...
   Importing emqx_app database table...
   Importing emqx_enhanced_authn_scram_mnesia database table...
   Importing emqx_authn_mnesia database table...
   Importing emqx_admin database table...
   Data has been imported successfully.
   ```


