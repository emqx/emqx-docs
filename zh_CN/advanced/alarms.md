---
# 编写日期
date: 2020-08-21 17:41:22
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

# Alarm

EMQ X Broker 内置监控与告警功能，目前支持监控 CPU 占用率、（系统、进程）内存占用率、进程数量、规则引擎资源状态、集群脑裂与愈合并进行告警。激活和取消告警都将产生一条警告日志并由 Broker 发布一个主题为 `$SYS/brokers/<Node>/alarms/activate` 或 `$SYS/brokers/<Node>/alarms/deactivate` 的 MQTT 消息，用户可以通过订阅 `$SYS/brokers/+/alarms/avtivate` 和 `$SYS/brokers/+/alarms/deactivate` 主题来获取告警通知。

告警通知消息的 Payload 为 Json 格式，包含以下字段：

| 字段          | 类型             | 说明                                                         |
| ------------- | ---------------- | ------------------------------------------------------------ |
| name          | string           | 告警名称                                                     |
| details       | object           | 告警详情                                                     |
| message       | string           | 人类易读的告警说明                                           |
| activate_at   | integer          | 激活告警的时间，以微秒为单位的 UNIX 时间戳                   |
| deactivate_at | integer / string | 取消激活告警的时间，以微秒为单位的 UNIX 时间戳，激活中的告警此字段值为 infinity |
| activated     | boolean          | 告警是否处于激活状态                                         |

以系统内存占用率过高告警为例，您将收到以下格式的消息：

![alarms_avtivate_msg](./assets/alarms_avtivate_msg.png)

告警不会重复产生，即如果 CPU 占用率过高告警已经激活，则在其激活期间，不会出现第二个 CPU 占用率过高告警。告警会在被监控项恢复正常后自动取消激活，但同样支持用户手动取消激活（如果用户明确自己不关心该告警）。用户除了可以在 Dashboard 查看当前告警（激活中的告警）与历史告警（已取消激活的告警）以外，还可以通过 EMQ X Broker 提供的 [HTTP API](./http-api.md#endpoint-alarms) 查询和管理告警。

EMQ X Broker 允许用户对告警功能进行一定程度的调整以适应实际需要，目前开放了以下配置项：

| 配置项                        | 类型     | 默认值      | 说明                                                         |
| ----------------------------- | -------- | ----------- | ------------------------------------------------------------ |
| os_mon.cpu_check_interval     | duration | 60s         | CPU 占用率的检查间隔                                         |
| os_mon.cpu_high_watermark     | percent  | 80%         | CPU 占用率高水位，即 CPU 占用率达到多少时激活告警            |
| os_mon.cpu_low_watermark      | percent  | 60%         | CPU 占用率低水位，即 CPU 占用率降低到多少时取消告警          |
| os_mon.mem_check_interval     | duration | 60%         | 内存占用率的检查间隔                                         |
| os_mon.sysmem_high_watermark  | percent  | 70%         | 系统内存占用率高水位，即申请的总内存占比达到多少时激活告警   |
| os_mon.procmem_high_watermark | percent  | 5%          | 进程内存占用率高水位，即单个进程申请的内存占比达到多少时激活告警 |
| vm_mon.check_interval         | duration | 30s         | 进程数量的检查间隔                                           |
| vm_mon.process_high_watermark | percent  | 80%         | 进程占用率高水位，即创建的进程数量与最大数量限制的占比达到多少时激活告警 |
| vm_mon.process_low_watermark  | percent  | 60%         | 进程占用率低水位，即创建的进程数量与最大数量限制的占比降低到多少时取消告警 |
| alarm.actions                 | string   | log,publish | 告警激活时触发的动作，目前仅支持 log 与 publish，即输出日志与发布系统消息 |
| alarm.size_limit              | integer  | 1000        | 已取消激活告警的最大保存数量，达到限制后将以 FIFO 原则清理这些告警 |
| alarm.validity_period         | duration | 24h         | 已取消激活告警的最大保存时间，过期的告警将被清理             |


