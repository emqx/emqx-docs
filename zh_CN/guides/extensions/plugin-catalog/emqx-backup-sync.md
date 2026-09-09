# EMQX 备份同步（Backup Sync）

EMQX 备份同步插件用于定期将选定的配置和内置数据库数据从主 EMQX 集群同步到备用集群。该插件适用于 EMQX 企业版 5.10.5 及之后的 5.10 版本。

只需在备用集群上安装并启动插件。主集群无需安装插件，但必须确保备用集群可以访问其 Dashboard 数据备份 API。

## 同步机制

插件以有效配置启动后，会立即异步执行首次同步，之后按配置的时间间隔重复同步。在备用集群中，同一时间只有一个运行中的核心节点执行同步。如果该节点不可用，其他核心节点可在后续同步周期接管任务。

每次同步执行以下操作：

1. 请求主集群导出选定的配置根和 Mnesia 表集合。
2. 从主集群下载备份，并将其上传到备用集群。
3. 在备用集群上导入备份。
4. 根据清理配置删除或保留备份文件。

配置根和 Mnesia 表集合采用不同的导入行为：

- **配置根**：采用 EMQX 的标准配置导入行为。来自主集群的值会被插入或更新，但不会删除仅存在于备用集群的配置。
- **Mnesia 表集合**：采用快照恢复行为。备用集群中的对应表会被主集群的快照替换。

::: warning 重要提示

快照恢复会删除选定表集合中仅存在于备用集群的记录。启动插件前，请仔细检查 `sync.table_sets`。

:::

只有插件已启动且配置有效时，才会执行同步。如果当前没有同步任务，应用有效配置会立即触发同步；如果任务正在运行，插件会请求取消该任务，并在后续同步中使用新配置。由于插件只在各同步阶段之间检查取消请求，已经开始的导入仍可能完成。

## 配置插件

1. 在主集群上[创建 API 密钥](../../api.md#创建-api-密钥)，并将角色设置为 `administrator`。如果配置了 API Scope，请包含 `system` Scope，该 Scope 允许访问插件所需的 `/data/*` 接口。

2. 确保备用集群可以访问主集群的 Dashboard API。两个集群均应运行 EMQX 5.10，因为备用集群无法导入由更高主版本或次版本生成的备份。

3. 参照[插件管理](../plugin-management.md)，在备用集群上安装 `emqx_backup_sync`。

4. 配置插件。以下示例启用了 TLS 证书验证：

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

   `primary.api_key` 和 `primary.api_secret` 可直接填写凭据，也可使用 `file://` 路径，例如 `file:///etc/emqx/backup-sync-api-key`。如果凭据文件末尾包含换行符，插件会在使用前删除末尾的换行符。

   由于任何运行中的核心节点都可能成为选定的同步节点，凭据文件以及 `primary.ssl.cacertfile`、`primary.ssl.certfile` 和 `primary.ssl.keyfile` 配置的文件必须以相同路径存在于备用集群的每个核心节点上，并且 EMQX 进程必须具有读取权限。

5. 在备用集群上启动插件。

### 配置项

| 配置项 | 默认值 | 说明 |
| --- | --- | --- |
| `primary.base_url` | 无 | 主集群的 Dashboard API 基础 URL，需包含 `/api/v5`。 |
| `primary.api_key` | 无 | 用于访问主集群的 API 密钥。支持直接填写或使用 `file://` 路径。 |
| `primary.api_secret` | 无 | 用于访问主集群的 API Secret。支持直接填写或使用 `file://` 路径。 |
| `primary.ssl.enable` | `false` | 是否为发往主集群的 HTTPS 请求启用 TLS 配置。 |
| `primary.ssl.server_name_indication` | `disable` | TLS 握手时发送的服务器名称指示（SNI）。 |
| `primary.ssl.verify` | `verify_none` | TLS 证书验证模式。支持 `verify_none` 和 `verify_peer`。生产环境中建议使用 `verify_peer` 并配置 `primary.ssl.cacertfile`。 |
| `primary.ssl.cacertfile` | 无 | 用于验证主服务器的 CA 证书文件路径。 |
| `primary.ssl.certfile` | 无 | 双向 TLS 使用的客户端证书文件路径。 |
| `primary.ssl.keyfile` | 无 | 双向 TLS 使用的客户端私钥文件路径。 |
| `sync.interval` | `5m` | 两次同步尝试之间的时间间隔。 |
| `sync.root_keys` | 见[同步范围](#同步范围) | 从主集群导出的配置根。 |
| `sync.table_sets` | 见[同步范围](#同步范围) | 从主集群导出并在备用集群上以快照方式恢复的 Mnesia 表集合。 |
| `sync.timeout` | `30s` | 每个发往主集群的 HTTP 请求的超时时间。 |
| `sync.retain_remote_backup` | `false` | 是否在主集群上保留每次导出的备份。默认情况下，插件会在清理阶段将其删除。 |
| `sync.retain_backup_after_import` | `true` | 尝试导入后是否在备用集群上保留每次上传的备份。 |

HTTP 客户端不会自动跟随重定向。请将 `primary.base_url` 配置为最终的 Dashboard API 地址。

每次成功导出备份后，插件都会执行清理，即使后续阶段失败或同步被取消。默认情况下，插件会删除主集群上的导出备份；如需保留该文件用于故障排查，请设置 `sync.retain_remote_backup = true`。

## 同步范围

`sync.root_keys` 的默认值如下：

- `connectors`
- `actions`
- `sources`
- `rule_engine`
- `listeners`
- `schema_registry`

还可以添加 `authentication` 和 `authorization`。建议将这 8 个配置根用于备份同步。

`sync.root_keys` 也接受主集群 `/data/export` API 支持的其他配置根。未知配置根会返回 `400 Invalid root keys`。但是，API 接受并不代表一定会同步：部分配置根（如 `node` 和 `rpc`）会在导入时被跳过。添加其他配置根前，请确认其可以导入且适用于备用集群。

规则通常依赖连接器、动作、数据源和 Schema Registry 对象。如果同步 `rule_engine`，请同时包含其依赖的配置根，除非备用集群中已存在等效对象。否则，导入可能失败，或导入的规则可能无法按预期运行。

默认情况下，`sync.table_sets` 包含 `banned`、`builtin_authn` 和 `builtin_authz`。还可选择 `builtin_retainer`、`psk` 和 `mt`。如果只需同步配置，请设置 `sync.table_sets = []`。

该插件使用 API 密钥调用数据备份 API，因此无法同步 `dashboard_users` 或 `api_keys` 表集合。对于使用 API 密钥发起的导出请求，数据备份 API 会忽略这些敏感表集合。

## 查看同步状态

在备用集群的任意节点上运行以下命令：

```bash
emqx ctl backup_sync status
```

该命令会查询选定的核心节点，并显示整体状态、健康状态、是否已启用同步、同步任务状态、选定的核心节点、距离下次同步的时间、主集群 API 基础 URL、同步间隔、配置根和表集合。命令不会显示 API 凭据。

如果导入成功但备份清理失败，该次同步仍会报告为失败。可通过健康状态输出和 EMQX 日志判断失败发生在导出、下载、上传、导入、清理还是 worker 取消阶段。
