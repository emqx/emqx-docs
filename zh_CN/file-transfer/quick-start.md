# 快速体验 MQTT 文件传输

本页提供了在 EMQX 中快速开始使用 MQTT 文件传输功能的指南。它涵盖了两种场景：使用本地磁盘和使用 S3 存储桶进行文件存储。

您可以按照以下说明在 EMQX 启用文件传输功能、使用客户端上传文件并通过 EMQX 提供的 API 访问上传的文件。

## 上传文件并使用本地磁盘存储文件

1. 在 EMQX 配置文件 `etc/emqx.conf` 中添加以下配置启用文件传输功能：

   ```bash
   file_transfer {
      enable = true
   }
   ```

   该配置使用本地磁盘存储上传的分片文件，传输完成后并不会对分片文件进行合并。

2. 运行以下命令启动 EMQX：

   ```bash
   ./bin/emqx start
   ```

3. 运行以下命令下载文件传输示例程序 `emqx-ft`，并设置测试客户端环境：

   ```bash
   git clone https://github.com/emqx/emqx-ft.git
   cd emqx-ft
   python3 -m venv .venv
   source .venv/bin/activate
   pip install .
   ```

4. 使用 `emqx-ft` 命令行工具运行以下命令以上传文件：

   ```bash
   emqx-ft --file test-file.txt \
      --file-id file-id-1 --segment-size 10 \
      --client-id client-1 \
      --file-name uploaded-test-file.txt
   ```

   命令参数说明：

   | 参数             | 含义                                                                   |
   | ---------------- | ---------------------------------------------------------------------- |
   | `--file`         | 要上传的文件的路径。                                                   |
   | `--file-id`      | 上传文件的唯一标识符。                                                 |
   | `--segment-size` | 文件的分片大小，以字节为单位，该参数用于将大文件分成较小的段进行上传。 |
   | `--client-id`    | 客户端 ID，用于标识进行文件上传操作的客户端。                          |
   | `--file-name`    | 上传后的文件名。                                                       |

5. 运行以下命令列出已上传的分片文件：

   ```bash
   $ tree /var/lib/emqx/file_transfer/exports
   /var/lib/emqx/file_transfer/exports
   _./data/file_transfer/exports
   ├── 8E
   │   └── B5
   │       └── 7023DA998C12F0B2A6CA586027E48BEC6271
   │           └── client-1
   │               └── file-id-1
   │                   ├── uploaded-test-file.txt
   │                   └── uploaded-test-file.txt.MANIFEST.json
   └── tmp
   ```

6. 使用 REST API 列出已上传文件列表，列表包含已上传文件的详细信息，包括名称、大小和时间戳：

   ```bash
   $ curl -u '...' -s 'http://127.0.0.1:18083/api/v5/file_transfer/files' | jq
   {
     "files": [
       {
         "clientid": "client-1",
         "fileid": "file-id-1",
         "metadata": {
           "checksum": "8CFE0BE8A1A5C9BF0F019ABAA8AEEA5D1E26251A0B000883C8875C99A5CFF2F8",
           "name": "uploaded-test-file.txt",
           "size": 168
         },
         "name": "uploaded-test-file.txt",
         "size": 168,
         "timestamp": "2023-06-13T00:43:25+02:00",
         "uri": "/api/v5/file_transfer/file?node=emqx%40127.0.0.1&fileref=8E%2FB5%2F7023DA998C12F0B2A6CA586027E48BEC6271%2Fclient-1%2Ffile-id-1%2Fuploaded-test-file.txt"
       }
     ]
   }
   ```

7. 运行以下命令以使用提供的 API 下载文件：

   ```bash
   $ curl -u '...' -s 'http://127.0.0.1:18083/api/v5/file_transfer/file?node=emqx%40127.0.0.1&fileref=8E%2FB5%2F7023DA998C12F0B2A6CA586027E48BEC6271%2Fclient-1%2Ffile-id-1%2Fuploaded-test-file.txt'
   ```

## 上传文件并使用 S3 存储桶存储

文件传输允许将已上传的文件导出到兼容 S3 的对象存储系统，如 Amazon S3。

::: tip 前置条件

开始之前，请确保已安装并正确配置了`s3cmd`。您可以参考 [Official s3cmd repo](https://github.com/s3tools/s3cmd) 获取更多信息。

:::

1. 在 EMQX 配置文件 `etc/emqx.conf` 中启用文件传输功能并配置 S3 存储桶：

   ```bash
   file_transfer {
       enable = true
       storage {
           local {
               enable = true
               exporter {
                   s3 {
                       enable = true
                       host = "s3.eu-north-1.amazonaws.com" # or any other S3-compatible storage
                       port = "443"

                       access_key_id = "..."
                       secret_access_key = "..."

                       bucket = "YOURBUCKET"
                       acl = private

                       transport_options {
                           ssl {
                               enable = true
                               # Use verify = true and other SSL options ensuring
                               # security for production
                           }
                       }
                   }
               }
           }
       }
   }
   ```

2. 运行以下命令启动 EMQX ：

   ```bash
   ./bin/emqx start
   ```

3. 运行以下命令下载文件传输示例程序 `emqx-ft`，并设置测试客户端环境：

   ```bash
   git clone https://github.com/emqx/emqx-ft.git
   cd emqx-ft
   python3 -m venv .venv
   source .venv/bin/activate
   pip install .
   ```

4. 使用 `emqx-ft` 命令行工具运行以下命令以上传文件：

   ```bash
   emqx-ft --file test-file.txt \
      --file-id file-id-1 --segment-size 10 \
      --client-id client-1 \
      --file-name uploaded-test-file.txt
   ```

   命令参数说明：

   | 参数             | 含义                                                                   |
   | ---------------- | ---------------------------------------------------------------------- |
   | `--file`         | 要上传的文件的路径。                                                   |
   | `--file-id`      | 上传文件的唯一标识符。                                                 |
   | `--segment-size` | 文件的分片大小，以字节为单位，该参数用于将大文件分成较小的段进行上传。 |
   | `--client-id`    | 客户端 ID，用于标识进行文件上传操作的客户端。                          |
   | `--file-name`    | 上传后的文件名。    |

5. 使用 S3 命令行工具运行以下命令，手动列出已上传的文件：

   ```bash
   $ s3cmd ls -r s3://YOURBUCKET/
   2023-06-12 22:58          168  s3://YOURBUCKET/client-1/file-id-1/uploaded-test-file.txt
   ```

   输出将显示指定 S3 存储桶中的文件。

6. 运行以下命令，使用 HTTP API 检索已上传文件的列表。

   ```
   $ curl -u '...' -s 'http://127.0.0.1:18083/api/v5/file_transfer/files' | jq
   {
     "files": [
       {
         "clientid": "client-1",
         "fileid": "file-id-1",
         "name": "uploaded-test-file.txt",
         "size": 168,
         "timestamp": "2023-06-13T00:58:53+02:00",
         "uri": "https://s3.eu-north-1.amazonaws.com/YOURBUCKET/client-1/file-id-1/uploaded-test-file.txt?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=...&X-Amz-SignedHeaders=host&X-Amz-Signature=..."
       }
     ]
   }
   ```

7. 运行以下命令，使用提供的 URI 直接从 S3 存储下载文件。

   ::: tip

   在使用 S3 存储桶的情况下，提供的下载链接不会指向 EMQX，而是直接指向 S3 存储，因此文件不会存储在 EMQX 本地。

   :::

   ```bash
   $ curl "https://s3.eu-north-1.amazonaws.com/YOURBUCKET/client-1/file-id-1/uploaded-test-file.txt?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=...&X-Amz-SignedHeaders=host&X-Amz-Signature=..."
   ```
