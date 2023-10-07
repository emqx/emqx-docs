# 快速体验 MQTT 文件传输

本页提供了在 EMQX 中快速开始使用 MQTT 文件传输功能的指南。它涵盖了两种场景：使用本地导出器和使用 S3 导出器进行文件存储。按照以下说明设置必要的配置、上传文件并使用提供的 API 访问上传的文件。

## 上传文件并用本地导出器存储

1. 在 EMQX 配置文件 `etc/emqx.conf` 中设置以下配置启用文件传输功能：

   ```bash
   file_transfer {
       enable = true
   }
   ```

2. 运行以下命令启动 EMQX：

   ```bash
   $ ./bin/emqx start
   ```

3. 运行以下命令从 GitHub 克隆 `emqx-ft` 仓库，设置测试客户端环境：

   ```bash
   $ git clone https://github.com/emqx/emqx-ft.git
   $ cd emqx-ft
   $ python3 -m venv .venv
   $ source .venv/bin/activate
   $ pip install .
   ```

4. 使用 `emqx-ft` 命令行工具运行以下命令以上传文件：

   ```
   $ emqx-ft --file test-file.txt --file-id file-id-1 --segment-size 10 --client-id client-1 --file-name uploaded-test-file.txt
   ```

5. 运行以下命令导航到文件存储目录，手动列出已上传的文件。

   显示的目录结构将包含已上传的文件。

   ```
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

6. 运行以下命令以使用 HTTP API 检索已上传文件的列表。

   返回内容将包含有关已上传文件的详细信息，包括名称、大小和时间戳。

   ```
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

7. 运行以下命令以使用提供的 API 端点下载文件。下载的文件将从 EMQX 检索。

   ```
   $ curl -u '...' -s 'http://127.0.0.1:18083/api/v5/file_transfer/file?node=emqx%40127.0.0.1&fileref=8E%2FB5%2F7023DA998C12F0B2A6CA586027E48BEC6271%2Fclient-1%2Ffile-id-1%2Fuploaded-test-file.txt'
   ```

## 上传文件并用 S3 导出器存储

文件传输允许将已上传的文件导出到兼容 S3 的对象存储系统，如 Amazon S3。

::: tip 前置条件

开始之前，请确保已安装并正确配置了`s3cmd`。您可以参考 [Official s3cmd repo](https://github.com/s3tools/s3cmd) 获取更多信息。

:::

1. 在 EMQX 配置文件 `etc/emqx.conf` 中启用文件传输功能并配置 S3 导出器：

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
   $ ./bin/emqx start
   ```

3. 运行以下命令从 GitHub 克隆 `emqx-ft` 仓库，设置测试客户端环境：

   ```bash
   $ git clone https://github.com/emqx/emqx-ft.git
   $ cd emqx-ft
   $ python3 -m venv .venv
   $ source .venv/bin/activate
   $ pip install .
   ```

4. 使用 `emqx-ft` 命令行工具运行以下命令以上传文件：

   ```bash
   $ emqx-ft --file test-file.txt --file-id file-id-1 --segment-size 10 --client-id client-1 --file-name uploaded-test-file.txt
   ```

5. 使用 S3 命令行工具运行以下命令，手动列出已上传的文件：

   ```bash
   $ s3cmd ls -r s3://YOURBUCKET/
   2023-06-12 22:58          168  s3://YOURBUCKET/client-1/file-id-1/uploaded-test-file.txt
   ```

   输出将显示指定的 S3 存储。

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

   在使用 S3 导出器的情况下，提供的下载链接不会指向 EMQX，而是直接指向 S3 存储，因此文件不会存储在EMQX 本地。

   :::

   ```bash
   $ curl "https://s3.eu-north-1.amazonaws.com/YOURBUCKET/client-1/file-id-1/uploaded-test-file.txt?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=...&X-Amz-SignedHeaders=host&X-Amz-Signature=..."
   ```

