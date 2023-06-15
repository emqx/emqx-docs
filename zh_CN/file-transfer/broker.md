# 在 EMQX 中配置文件传输

本页面提供了在 EMQX 中配置文件传输功能设置的说明。主题将涵盖用于文件传输操作的 MQTT 设置，以及用于管理文件元数据、片段和导出文件的存储选项。本页还将具体说明本地存储后端和用于文件导出的 S3 导出器。

EMQX 允许不配置大多数文件传输设置并使用默认值。

## 配置文件传输的 MQTT 设置

为了优化文件传输操作并防止客户端等待时间过长，我们可以为不同的文件传输操作设置特定的超时时间。可以配置以下 MQTT 传输设置：

```
file_transfer {
    enable = true
    init_timeout = "10s"
    store_segment_timeout = "10s"
    assemble_timeout = "60s"
}
```

- `init_timeout`：初始化操作的超时时间。
- `store_segment_timeout`：存储文件片段的超时时间。
- `assemble_timeout`：文件组装的超时时间。

如果这些操作中的任何一个超过了指定的超时时间，MQTT 客户端将收到带有 `RC_UNSPECIFIED_ERROR` 代码的PUBACK数据包。

## 配置文件传输存储设置

EMQX 提供了用于管理文件元数据、片段和导出文件的存储选项。目前，EMQX 支持单个存储后端：本地文件存储。

要启用本地文件存储，请使用以下配置：

```
{
    file_transfer {
        enable = true
        storage {
            local {
                enable = true
            }
        }
    }
}
```

使用本地文件存储，EMQX 将文件元数据和片段存储在接收节点的本地文件系统中。此外，本地文件存储使用配置的导出器导出已上传的文件。EMQX 支持两种导出器：本地导出器和 S3 导出器，分别将文件导出到本地文件系统和兼容 S3 的对象存储系统。

可以同时设置多个导出器的设置，但只能启用一个。

### 文件片段设置

文件片段设置允许您配置如何管理文件片段。可以指定以下参数：

- `root`：存储片段的根目录。
- 片段垃圾回收设置。


```
{
    file_transfer {
        enable = true
        storage {
            local {
                enable = true
                segments {
                    root = "/var/lib/emqx/file_transfer/segments"
                    gc {
                        interval = "1h"
                        maximum_segments_ttl = "1d"
                        minimum_segments_ttl = "1h"
                    }
                }
            }
        }
    }
}
```

您需要根据预期的文件传输负载、文件大小、并发传输和可用磁盘空间这些因素为这些参数选择适当的值。

### 本地导出器设置

本地导出器允许将文件导出到本地文件系统。您只需设置导出文件的根文件夹。

```
{
    file_transfer {
        enable = true
        storage {
            local {
                enable = true
                exporter {
                    local {
                        enable = true
                        root = "/var/lib/emqx/file_transfer/exported"
                    }
                }
            }
        }
    }
}
```

随着时间的推移，导出的文件可能会累积，可能导致导出文件夹中存在大量文件。为了解决这个问题，EMQX 使用存储导出文件的分桶存储方案。该方案涉及以下 6 级文件夹层次结构：

- 它计算文件 ID 和客户端 ID 的 sha256 哈希值（`ABCDEFG012345...`）。
- 它将文件存储在 6 级文件夹层次结构中，使用以下级别：
  1. 哈希的前两个字节作为第一级文件夹名称；
  2. 接下来的两个字节作为第二级文件夹名称；
  3. 剩余的哈希作为第三级文件夹名称；
  4. 客户端 ID；
  5. 文件 ID；
  6. 元数据中的转义文件名作为最后一级。

例如，导出的文件可能存储在如下的文件夹结构中：`AB/CD/EFGH.../clientid/file_id/escaped_file_name_from_the_metadata`。

EMQX 提供了用于列出和下载导出文件的 API。

### S3 导出器设置

S3 导出器允许将文件导出到兼容 S3 的对象存储系统。它具有以下配置设置：

- `min_part_size` 和 `max_part_size`：S3 导出器使用分块上传将文件上传到 S3。在上传之前，片段将被收集到大于 `min_part_size` 的块中，而大于 `max_part_size` 的片段将导致错误。
- `transport_settings`：用于与S3的底层 HTTP(S) 连接的设置，允许安全文件上传和连接池管理。

与本地导出器使用的分桶存储方案不同，使用 S3 导出器导出的文件使用更简单的 3 级层次结构存储：

1. 客户端 ID。
2. 文件 ID。
3. 转义后的文件名。
