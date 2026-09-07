# 映射表（Mapping Tables）

EMQX Mapping Tables 插件为 Rule SQL 提供命名 mapping table。当规则需要将稳定的标识符、编码或二进制字段 ID 映射为结构化值时，可以使用该插件，避免在 SQL 语句中维护较长的 `CASE WHEN` 表达式。

该插件从 EMQX 6.2.3 开始可用。请先安装并启动该插件，再在 Rule SQL 中使用 `maptab_lookup`。

Mapping table 通过 `emqx ctl maptabs` CLI 从 JSON 文件加载，并存储在 EMQX 的复制数据库中。规则可使用 `maptab_lookup` SQL 函数查询这些表。函数签名和 SQL 示例参见[内置 SQL 函数](../../../data-integration/rule-sql-builtin-functions.md)。

## 表文件

一个 mapping table 对应一个 JSON 文件。文件名去掉 `.json` 扩展名后作为表名。表名只能包含字母、数字、下划线和连字符。

JSON 文件必须包含一个由行对象组成的数组。每一行必须包含 `key` 字段。该行中除 `key` 以外的所有字段组成该行的值映射。

示例：

```json
[
  {
    "key": 1,
    "signal_name": "temperature_c",
    "start_bit": 17,
    "length": 8,
    "type": "integer",
    "signedness": "signed",
    "endian": "big"
  },
  {
    "key": 2,
    "signal_name": "pressure_kpa",
    "start_bit": 17,
    "length": 32,
    "type": "float",
    "signedness": "unsigned",
    "endian": "big"
  }
]
```

`key` 必须是 JSON integer 或 string。原生 JSON 类型会被保留，因此整数 `50` 和字符串 `"50"` 是不同的 Key。

加载采用 fail-closed 行为。文件存在以下任一问题时，EMQX 会拒绝整个文件并保留此前的表版本：

- JSON 格式无效
- 顶层值不是数组
- 行不是对象
- 行缺少 `key`
- 存在重复 Key
- Key 类型为 float、boolean、null、array 或 object

## CLI 命令

使用 `emqx ctl maptabs` CLI 管理 mapping table。

| 命令 | 说明 |
| --- | --- |
| `emqx ctl maptabs list` | 列出本节点缓存的表，包括行数和版本。 |
| `emqx ctl maptabs status` | 列出所有运行中节点上的表，可用于检测缓存漂移。 |
| `emqx ctl maptabs load <file>` | 校验表 JSON 文件，并将其复制到所有节点。 |
| `emqx ctl maptabs reload` | 在所有运行中节点上从存储重建缓存，可用于在需要时重新同步缓存。 |
| `emqx ctl maptabs get <name>` | 输出指定表中存储的 JSON 内容。 |
| `emqx ctl maptabs delete <name>` | 从所有节点删除指定表。 |

除 `emqx ctl maptabs get <name>` 在表存在时会直接输出存储的表 JSON 内容外，其他命令均输出 JSON。

## 配置

可通过标准插件配置 API `PUT /api/v5/plugins/<name-vsn>/config` 或插件配置文件管理该插件配置。

| 配置项 | 默认值 | 说明 |
| --- | --- | --- |
| `max_tables` | `100` | mapping table 最大数量。超过限制时，加载新表会被拒绝；替换已有表始终允许。 |
| `max_rows_per_table` | `10000` | 单个表的最大行数。行数超过限制的文件会被拒绝。 |
| `max_table_file_bytes` | `10000000` | 表 JSON 文件的最大字节数。超过限制的文件会在读入内存和复制前被拒绝。 |

这些限制在加载表时检查。修改限制不会删除或截断已经加载的表。

## 集群行为

该插件将表内容存储在 EMQX 的内置复制数据库中。加载或删除表会复制到集群中的每个节点，各节点会根据存储的表内容重建内存缓存。

请在集群所有节点上安装并启动该插件。如果某个节点在加载或删除表时处于离线状态，该节点重启后会自动同步这些更新并重建缓存。

缓存更新对读取方是原子的。规则查询会看到旧版本或新版本的表，不会看到部分更新。

## 访问和共享

Mapping table 只能由管理员通过 CLI 管理。这些表在租户命名空间之间共享。无论客户端是否属于多租户命名空间，查询都会返回相同的行。

如果不同租户需要使用不同的行，请将租户信息编码到表数据中。例如，在查询 Key 中包含租户：

```sql
maptab_lookup('signals', concat(client_attrs.tns, ':', item_id))
```

也可以为每个租户使用一张表，并在规则中组合表名。请对表中的每个 Key 和每个查询位置使用一致的约定。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## 下载

各 EMQX 版本对应的插件安装包：

| EMQX 版本 | 插件版本 | 安装包 |
|---|---|---|
| 6.2.3 | 0.1.2 | [emqx_maptabs-0.1.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.3/emqx_maptabs-0.1.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.3/emqx_maptabs-0.1.2.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
