---
# 标题
title: 告警
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
ref: undefined
---

# Alarm

EMQ X Broker has built-in monitoring and alarm functions. Currently, it supports monitoring of CPU occupancy, (system, process) memory occupancy, number of processes, rule engine resource status, cluster partition and healing and it can alarm. Both activation and deactivation of alarms will generate a alarm log and the Broker will publish an MQTT message with the topic of `$SYS/brokers/<Node>/alarms/activate` or `$SYS/brokers/<Node>/alarms/deactivate`, Users can subscribe to the topics of `$SYS/brokers/+/alarms/avtivate` and `$SYS/brokers/+/alarms/deactivate` to get alarm notifications.

The Payload of the alarm notification message is in Json format and contains the following fields:

| Field         | Type             | Description                                                  |
| ------------- | ---------------- | ------------------------------------------------------------ |
| name          | string           | Alarm name                                                   |
| details       | object           | Alarm details                                                |
| message       | string           | Human-readable alarm instructions                            |
| activate_at   | integer          | The time to activate the alarm, UNIX timestamp in microseconds |
| deactivate_at | integer / string | The time to deactivate the alarm, the UNIX timestamp in microseconds. The value of this field for the activated alarm is infinity |
| activated     | boolean          | Whether the alarm is activated                               |

Taking the alarm of high system memory usage  as an example, you will receive a message in the following format:

<!-- ![alarms_avtivate_msg](./assets/alarms_avtivate_msg.png) -->

The alarm will not be generated repeatedly. That is to say, if the high CPU usage alarm has been activated, the same alarm will not appear during its activation. The alarm will be automatically deactivated when the monitored item returns to normal. However, it also supports manual deactivation by the user (if the user clearly does not care about the alarm). Users can view current alarms (activated alarms) and historical alarms (deactivated alarms) on the Dashboard, and they can also use the [HTTP API](./http-api.md#endpoint-alarms) provided by EMQ X Broker to Query and manage alarms.

EMQ X Broker allows users to adjust the alarm function to a certain extent to meet actual needs. The following configuration items are currently opened:

| Configuration item            | Type     | Default value | Description                                                  |
| ----------------------------- | -------- | ------------- | ------------------------------------------------------------ |
| os_mon.cpu_check_interval     | duration | 60s           | Check interval for CPU usage                                 |
| os_mon.cpu_high_watermark     | percent  | 80%           | The high watermark of the CPU usage, which is the threshold to activate the alarm |
| os_mon.cpu_low_watermark      | percent  | 60%           | The low watermark of the CPU usage, which is the threshold to deactivate the alarm |
| os_mon.mem_check_interval     | duration | 60%           | Check interval for memory usage                              |
| os_mon.sysmem_high_watermark  | percent  | 70%           | The high water mark of the system memory usage. The alarm is activated when the total memory occupied by the application reaches this value |
| os_mon.procmem_high_watermark | percent  | 5%            | The high water mark of the process memory usage. The alarm will be activated when the memory occupied by a single process reaches this value. |
| vm_mon.check_interval         | duration | 30s           | Check interval for the number of processes                   |
| vm_mon.process_high_watermark | percent  | 80%           | The high watermark of the process occupancy rate, that is, the alarm is activated when the ratio of the number of created processes to the maximum number limit reaches this value |
| vm_mon.process_low_watermark  | percent  | 60%           | The low water mark of the process occupancy rate, that is, the alarm is deactivated when the ratio of the number of created processes to the maximum number limit drops to this value |
| alarm.actions                 | string   | log,publish   | The action triggered when the alarm is activated, and it currently only supports log and publish, which is to output log and publish system messages |
| alarm.size_limit              | integer  | 1000          | The maximum number of saved alarms that has been deactivated. After the limit is reached, these alarms will be cleared according to the FIFO principle |
| alarm.validity_period         | duration | 24h           | The maximum storage time of deactivated alarms, and expired alarms will be cleared |


