---
# 编写日期
date: 2020-05-09 17:15:26
# 作者 Github 名称
author: tigercl
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref:
---

# 数据导入导出

EMQ X Broker 为用户提供了数据导入导出功能，以满足服务器升级、迁移以及数据备份等需要。数据导入导出功能支持将当前运行的 EMQ X Broker 中的黑名单、规则引擎配置等存储在 EMQ X Broker 默认数据库 Mnesia 中的数据以 Json 格式导出至本地文件。当然用户无需关心导出文件中的数据内容。导出文件可以导入至其他 EMQ X Broker 的运行实例，EMQ X Broker 可以是相同版本，也可以是不同版本，但目前仅支持 `4.1.0` 及之后的版本。

EMQ X Broker 为数据导入导出功能提供了[命令行接口](./cli.md#endpoint-data-import-and-export)、[HTTP API](./http-api.md#endpoint-data-import-and-export)以及 Dashboard 的可视化界面（企业版）。目前支持导入导出的数据如下:

- 规则引擎配置数据（资源、规则）
- 规则引擎编解码配置数据（企业版）
- 黑名单数据
- Application 数据
- Dashboard 用户数据
- 通过 emqx-auth-mnesia 插件添加的 MQTT 用户数据和 ACL 数据
- 通过 emqx-auth-clientid 插件添加的 MQTT 用户数据和 ACL 数据
- 通过 emqx-auth-username 插件添加的 MQTT 用户数据和 ACL 数据
- 编解码 Schema

## 示例

### 命令行接口

1. 导出数据，导出文件的文件名格式为 `emqx-export-YYYY-MM-DD-HH-mm-SS.json`，默认导出路径为 data 目录（请参见 [目录结构](../getting-started/directory.md)）

    ```bash
    $ ./emqx_ctl data export
    The emqx data has been successfully exported to /var/lib/emqx/data/emqx-export-2020-5-15-17-39-0.json.
    ```
2. 保存导出文件，这里将导出文件保存到 tmp 目录

   ```bash
   $ cp /var/lib/emqx/data/emqx-export-2020-5-15-17-39-0.json /tmp
   ```

3. 重新安装 EMQ X Broker 并启动

   ```bash
   $ ./emqx start
   EMQ X Broker v4.1-rc.1 is started successfully!
   ```

4. 导入数据，导入的文件名必须以绝对路径形式指定

   ```bash
   $ ./emqx_ctl data import /tmp/emqx-export-2020-5-15-17-39-0.json
   The emqx data has been imported successfully.
   ```

### HTTP API

1. 导出数据

   ```bash
   $ curl -i --basic -u admin:public -X POST "http://localhost:8081/api/v4/data/export"

   {"data":{"size":388,"filename":"emqx-export-2020-9-4-10-24-16.json","created_at":"2020-9-4 10:24:16"},"code":0}
   ```

   > 导出的数据文件位于 `.../emqx/data` 或 `/var/lib/emqx/data` 目录

2. 下载数据文件

   ```bash
   $ curl --basic -u admin:public -X GET http://localhost:8081/api/v4/data/file/emqx-export-2020-9-4-10-24-16.json -o /tmp/emqx-export-2020-9-4-10-24-16.json   
   ```

3. 上传数据文件

   ```bash
   $ curl -i --basic -u admin:public -X POST "http://localhost:8081/api/v4/data/import" -d @/tmp/emqx-export-2020-9-4-10-24-16.json

   {"code":0}
   ```

   > 第 2、3 步适用于在不同机器上迁移 emqx

4. 导入数据

   ```bash
   $ curl -i --basic -u admin:public -X POST "http://localhost:8081/api/v4/data/import" -d '{"filename":"emqx-export-2020-9-4-10-24-16.json"}'

   {"code",0}
   ```