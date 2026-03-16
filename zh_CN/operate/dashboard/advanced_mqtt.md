# MQTT 高级特性

::: tip 注意

MQTT 高级特性仅适用于 EMQX 企业版。

:::

## 主题重写

主题重写功能可以帮助实现修改设备业务主题。通过给 EMQX 设置一套规则，它可以在订阅、发布时改变将原有主题重写为新的目标主题。该页面方便用户通过 Dashboard 添加主题重写规则而无需修改配置文件。详细的主题重写规则，参考[主题重写](../messaging/mqtt-topic-rewrite.md)。

## 自动订阅

自动订阅是 EMQX 支持的 MQTT 扩展功能。自动订阅能够给 EMQX 设置多个规则，在设备成功连接后按照规则为其订阅指定主题，不需要额外发起订阅。该页面方便用户通过 Dashboard 配置自动订阅功能。具体的配置指导，参考[自动订阅](../messaging/mqtt-auto-subscription.md)。

## 延迟发布

延迟发布是 EMQX 支持的 MQTT 扩展功能。当客户端使用特殊主题前缀 `$delayed/{DelayInteval}` 发布消息时，将触发延迟发布功能，可以实现按照用户配置的时间间隔延迟发布消息。该页面方便用户通过 Dashboard 配置延迟发布功能。具体的配置指导，参考[通过 Dashboard 配置延迟发布](../messaging/mqtt-delayed-publish.md#通过-dashboard-配置延迟发布)。

## 文件传输

基于 MQTT 的文件传输是 EMQX 企业版的高级功能。EMQX 扩展了 MQTT 协议，使客户端设备除了能够传输传感器数据和控制指令这样的实时结构化数据，还能够传输、管理和存储离线类型的文件数据，如音视频、图片和诊断日志等。您可以在文件传输页面配置该功能。具体的配置指导，参考[通过 Dashboard 启用和配置文件传输](../file-transfer/broker.md#通过-dashboard-启用和配置文件传输)。

## 