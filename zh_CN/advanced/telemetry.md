# 遥测

::: tip
遥测功能在 4.2-rc.1 版本中加入到 EMQ X。
:::

EMQ 通过遥测收集有关 EMQ X 使用情况的信息，这种功能旨在为我们提供有关用户、社区的综合信息，以及对 EMQ X 使用方式的了解。与我们共享这些指标可以帮助我们更好地了解您如何使用我们的产品，并可以持续地帮助我们改进产品。

这些统计信息不包含个体数据，如服务器型号、硬件编号、IP 地址，统计数据数据永远不会与其他任何人共享。

## 遥测数据示例

遥测通过加密 HTTP 请求将数据上报至 [https://telemetry.emqx.io/api/telemetry](https://telemetry.emqx.io/api/telemetry)，传输的数据如下，如果您有任何疑问请通过 [Issues](http://github.com/emqx/emqx/issues) 联系我们：

```json
{
  "license": {
    "edition": "community"
  },
  "nodes_uuid": [],
  "active_plugins": [
    "emqx_telemetry",
    "emqx_retainer",
    "emqx_recon",
    "emqx_management",
    "emqx_dashboard"
  ],
  "active_modules": [
    "emqx_mod_presence",
    "emqx_mod_acl_internal"
  ],
  "num_clients": 0,
  "messages_received": 0,
  "messages_sent": 0,
  "emqx_version": "4.2-rc.1",
  "os_name": "Ubuntu",
  "os_version": "16.04.6 LTS (Xenial Xerus)",
  "otp_version": "22",
  "up_time": 7081,
  "uuid": "D6138FCE-E455-11EA-A854-FD8F5F98125F"
}
```

遥测**默认是启用的**，有些人可能对收集这样的数据感到不舒服，你在启动前、运行中都可以通过最简单的方式禁用它。

## 启动前禁用

编辑 data/loaded_plugins 文件，删除 emqx_telemetry.  这一行数据（注意后面有一个 "." 字符），这表示遥测插件永远不会启动。

## 运行中禁用

在命令行中停止 emqx-telemetry 插件：./bin/emqx_ctl plugins unload emqx_telemetry 

在 Dashboard 中停止 emqx-telemetry 插件：打开 http://localhost:18083 进入 Dashboard，进入 插件 页面，找到 emqx-telemetry 点击 停止 即可。
