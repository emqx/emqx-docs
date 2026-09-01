# EMQX 备份同步（Backup Sync）

该插件通过现有的数据备份（Data Backup）API，定期将选定的备份数据从主 EMQX 集群同步到备用 EMQX 集群。

备用集群调用主集群导出备份文件，然后下载该文件、在本地上传并导入。选定的配置根（configuration roots）按 EMQX 现有的配置导入语义导入。选定的 Mnesia 表集合以快照方式导入，因此这些表集合中仅存在于备用集群的记录会被删除。未被选定的配置根和表集合保持不变。

## 配置

在每个备用集群上安装并启动该插件。主集群无需安装此插件，只需保证其 Dashboard 数据备份 API 可从备用集群访问。

```hocon
primary {
  base_url = "https://primary.example.com:18083/api/v5"
  api_key = "sync-key"
  api_secret = "sync-secret"
  ssl {
    enable = true
    server_name_indication = "primary.example.com"
    verify = "verify_peer"
    cacertfile = "/etc/emqx/certs/primary-ca.pem"
    certfile = ""
    keyfile = ""
  }
}

sync {
  interval = "5m"
  root_keys = [
    "connectors",
    "actions",
    "sources",
    "rule_engine",
    "listeners",
    "schema_registry"
  ]
  table_sets = [
    "banned",
    "builtin_authn",
    "builtin_authz"
  ]
  timeout = "30s"
  retain_remote_backup = false
  retain_backup_after_import = true
}
```

所配置的 API 密钥必须有权访问主集群上的数据备份接口。`primary.api_key` 和 `primary.api_secret` 可以直接设置，也可以设为 `file://` 路径，例如 `file:///etc/emqx/backup-sync-api-key`。

`sync.root_keys` 支持的取值为 `connectors`、`actions`、`sources`、`rule_engine`、`listeners`、`schema_registry`、`authentication` 和 `authorization`。

规则通常依赖连接器（connectors）、动作（actions）、数据源（sources）和 schema registry 对象。如果同步 `rule_engine` 而不同步其依赖项，导入可能会失败或造成运行时行为不完整。除非这些依赖根已存在于备用集群，否则请将它们一并包含在 `sync.root_keys` 中。

默认情况下，同步还会包含 `banned`、`builtin_authn` 和 `builtin_authz` 表集合。这些选定的表集合会在备用集群上被替换。当只需同步配置时，请设置 `sync.table_sets = []`。`sync.table_sets` 支持的取值为 `banned`、`builtin_authn`、`builtin_authz`、`builtin_retainer`、`psk` 和 `mt`。当使用 API 密钥调用时，主集群的数据备份 API 不包含 `dashboard_users` 或 `api_keys`。

## CLI

在备用节点上使用以下命令查看本地同步 worker：

```bash
emqx ctl backup_sync status
```

该命令会打印本地节点、健康状态、worker 状态、选定的 core 节点、下一次计划同步时间以及非敏感的同步配置。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## 下载

各 EMQX 版本对应的插件安装包：

| EMQX 版本 | 插件版本 | 安装包 |
|---|---|---|
| 6.3.0 | 0.1.3 | [emqx_backup_sync-0.1.3.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.3.0/emqx_backup_sync-0.1.3.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.3.0/emqx_backup_sync-0.1.3.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
