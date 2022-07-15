# Dashboard

## 简介

EMQX 提供了 Dashboard，以方便用户通过 Web 页面管理、监控 EMQX 并配置所需的功能。

主要功能:

* 监控运行中的 EMQX 集群。
* 查看、搜索和管理连接的客户端，甚至踢出客户端。
* 查看、搜索订阅与主题。
* 管理客户端的用户名/密码，或者配置一个外部资源来满足客户端的认证。例如数据库或认证服务器。
* 创建或更新数据集成规则（规则引擎），在设备和外部数据平台间提取、过滤或转换并传输数据，如另一个 MQTT 服务器，数据库或 Web 服务。
* 管理扩展，如网关，gRPC 以及插件。
* 诊断工具，如内置的 Websocket MQTT 客户端，在线日志追踪等。

## 启动

EMQX Dashboard 是一个 Web 应用程序，默认监听 18083 端口。EMQX 成功启动之后可以通过浏览器打开 [http://localhost:18083/](http://localhost:18083/)（将 localhost 替换为实际 IP 地址）以访问 Dashboard。

在不启用 Dashboard 情况下仍然可以使用 EMQX，但这可能大大影响服务的可操作性：Dashboard 可以以表单方式配置很多功能，相比手动编辑配置文件更容易上手使用。

## 配置 Dashboard

Dashboard 默认监听 HTTP 端口，你可以为 Dashboard 启用 HTTPS 或更改监听器端口，更多配置可以参考[配置文档](../admin/cfg.md#dashboard)。

<!--TODO: maybe add some screenshots-->
