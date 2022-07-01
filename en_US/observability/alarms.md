# Alarm

EMQX Broker has built-in monitoring and alarm functionality. Currently it supports monitoring of CPU occupancy, system and process memory occupancy, number of processes, rule engine resource status, cluster partition and healing, and it can alarm on these metrics. Both activation and deactivation of alarms will generate an alarm log and the Broker will publish an MQTT message with the topic of `$SYS/brokers/<Node>/alarms/activate` or `$SYS/brokers/<Node>/alarms/deactivate`. Users can subscribe to the topics of `$SYS/brokers/+/alarms/avtivate` and `$SYS/brokers/+/alarms/deactivate` to get alarm notifications.

The Payload of an alarm notification message is in Json format and contains the following fields:

| Field         | Type             | Description                                                  |
| ------------- | ---------------- | ------------------------------------------------------------ |
| name          | string           | Alarm name                                                   |
| details       | object           | Alarm details                                                |
| message       | string           | Human-readable alarm instructions                            |
| activate_at   | integer          | A UNIX timestamp in microseconds representing when the alarm was activated |
| deactivate_at | integer / string | A UNIX timestamp in microseconds representing when the alarm was deactivated. The value of this field for the activated alarm is infinity. |
| activated     | boolean          | Whether the alarm is activated                               |

Taking the alarm of high system memory usage as an example, you will receive a message in the following format:

<!-- ![alarms_activate_msg](./assets/alarms_activate_msg.png) -->

<!-- Update links to include a link to {{ your-emqx-dashboard-endpoint }}  -->
An alarm will not be generated repeatedly. That means if the high CPU usage alarm has been activated the same alarm will not appear again while high CPU is maintained.
The alarm will be automatically deactivated when the monitored metric returns to normal.
However, it also supports manual deactivation by the user (if the user clearly does not care about the alarm).
Users can view current alarms (activated alarms) and historical alarms (deactivated alarms) on the Dashboard, 
and they can also use the HTTP API provided by EMQX to Query and manage alarms.

EMQX Broker allows users to adjust the alarm function to a certain extent to meet actual needs. The following configuration items are currently available:

| Configuration item            | Type     | Default value | Description                                                  |
| ----------------------------- | -------- | ------------- | ------------------------------------------------------------ |
| os_mon.cpu_check_interval     | duration | 60s           | Check interval for CPU usage                                 |
| os_mon.cpu_high_watermark     | percent  | 80%           | The high watermark of the CPU usage, which is the threshold to activate the alarm |
| os_mon.cpu_low_watermark      | percent  | 60%           | The low watermark of the CPU usage, which is the threshold to deactivate the alarm |
| os_mon.mem_check_interval     | duration | 60s           | Check interval for memory usage                              |
| os_mon.sysmem_high_watermark  | percent  | 70%           | The high water mark of the system memory usage. The alarm is activated when the total memory occupied by the application reaches this value |
| os_mon.procmem_high_watermark | percent  | 5%            | The high water mark of the process memory usage. The alarm will be activated when the memory occupied by a single process reaches this value. |
| vm_mon.check_interval         | duration | 30s           | Check interval for the number of processes                   |
| vm_mon.process_high_watermark | percent  | 80%           | The high watermark of the process occupancy rate, that is, the alarm is activated when the ratio of the number of created processes to the maximum number limit reaches this value |
| vm_mon.process_low_watermark  | percent  | 60%           | The low water mark of the process occupancy rate, that is, the alarm is deactivated when the ratio of the number of created processes to the maximum number limit drops to this value |
| alarm.actions                 | string   | log,publish   | The action triggered when the alarm is activated. It currently only supports log and publish, which is to output log and publish system messages |
| alarm.size_limit              | integer  | 1000          | The maximum number of saved alarms that has been deactivated. After the limit is reached, these alarms will be cleared according to the FIFO principle |
| alarm.validity_period         | duration | 24h           | The maximum storage time of deactivated alarms, and expired alarms will be cleared |


{% emqxee %}
EMQX Enterprise raises an alarm when the license expires in less than 30 days, or if the number of connections exceeds the high watermark. User can adjust the high/low watermark for the number of connections according to the actual situation.

| Configuration item            | Type     | Default value | Description                                                  |
| ----------------------------- | -------- | ----------- | ------------------------------------------------------------ |
| license.connection_high_watermark_alarm  | percent  | 80%         | The high water mark of license‘s max connections. The alarm is activated when this threshold is reached. Measured as a percentage of alive connections/max connections.  |
| license.connection_low_watermark_alarm    | percent  | 75%         | The low water mark of license‘s max connections. The alarm is deactivated when it goes below this threshold. Measured as a percentage of alive connections/max connections.  |
{% endemqxee %}
