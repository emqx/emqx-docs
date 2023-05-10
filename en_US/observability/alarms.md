# Alarm

EMQX offers a built-in monitoring and alarm functionality for monitoring the internal state changes, such as CPU occupancy, system and process memory occupancy, number of processes, rule engine resource status, cluster partition and healing. EMQX triggers and records these changes when they exceed a threshold or deviate from expectations, and removes them from the list once they are restored. 

This page introduces which alarm information EMQX provides, how you can obtain and check the detailed alarm information, and how to configure the alarm settings and thresholds in EMQX. The monitoring and alarm function keeps you notified of potential problems during operation. By configuring alarms and setting appropriate thresholds, you can make sure that EMQX remains secure, stable, and reliable.

## Alarm List

The following table lists the alarms that can be triggered to indicate potential problems during system monitoring.

::: tip

Depending on the severance and impacts on the system, alarms can have 3 levels:

- **Error**: Errors caused by user presets. The client can perceive the error and retry.

- **Warning**: Occasional errors, and need to be taken seriously if they occur frequently.

- **Critical**: Irreversible data loss between the client and server, causing communication and business interruption.

The levels are defined from development perspectives and are only for a recommendation. You can define your own alarm levels according to the business needs.

:::

{% emqxce %}

| **Alarm**                 | Level    | Description                                                  | **Details**                              | **Threshold**                                                |
| :------------------------ | -------- | :----------------------------------------------------------- | :--------------------------------------- | :----------------------------------------------------------- |
| high_system_memory_usage  | Warning  | System memory usage is too high                              | "System memory usage is higher than ~p%" | `os_mon.sysmem_high_watermark = 70%`                         |
| high_process_memory_usage | Warning  | Single Erlang process memory usage is too high (percentage of system memory usage) | Process memory usage is higher than ~p%  | `os_mon.procmem_high_watermark = 5%`                         |
| high_cpu_usage            | Warning  | CPU usage is too high                                        | ~p% cpu usage                            | `os_mon.cpu_high_watermark = 80%` `os_mon.cpu_low_watermark = 60%` |
| too_many_processes        | Warning  | Too many processes                                           | ~p% process usage                        | `vm_mon.process_high_watermark = 80%` `vm_mon.process_low_watermark = 60%` |
| partition                 | Critical | Partition occurs at node                                     | Partition occurs at node ~s              | -                                                            |
| resource                  | Critical | Resource is disconnected                                     | Resource ~s(~s) is down                  | -                                                            |
| conn_congestion           | Critical | Connection process congestion                                | connection congested                     | -                                                            |

{% endemqxce %}

{% emqxee %}

| **Alarm**                 | Level    | Description                                                  | **Details**                                  | **Threshold**                                                |
| :------------------------ | -------- | :----------------------------------------------------------- | :------------------------------------------- | :----------------------------------------------------------- |
| high_system_memory_usage  | Warning  | System memory usage is too high                              | "System memory usage is higher than ~p%"     | `os_mon.sysmem_high_watermark = 70%`                         |
| high_process_memory_usage | Warning  | Single Erlang process memory usage is too high (percentage of system memory usage) | Process memory usage is higher than ~p%      | `os_mon.procmem_high_watermark = 5%`                         |
| high_cpu_usage            | Warning  | CPU usage is too high                                        | ~p% cpu usage                                | `os_mon.cpu_high_watermark = 80%` `os_mon.cpu_low_watermark = 60%` |
| too_many_processes        | Warning  | Too many processes                                           | ~p% process usage                            | `vm_mon.process_high_watermark = 80%` `vm_mon.process_low_watermark = 60%` |
| license_quota             | Warning  | License exceeds quota                                        | License: the number of connections exceeds % | `license.connection_high_watermark_alarm = 80%` `license.connection_low_watermark_alarm = 75%` |
| license_expiry            | Critical | License expired                                              | License will be expired at %                 | -                                                            |
| partition                 | Critical | Partition occurs at node                                     | Partition occurs at node ~s                  | -                                                            |
| resource                  | Critical | Resource is disconnected                                     | Resource ~s(~s) is down                      | -                                                            |
| conn_congestion           | Critical | Connection process congestion                                | Connection congested                         | -                                                            |

{% endemqxee %}

## Get Alarms

EMQX provides you with various ways to get alarms and check detailed alarm information. One way is to view the alarms on EMQX Dashboard, where you can view a list of active or historical alarms. However, it is only a central place for easy access to an overview of alarms that have been triggered. Another way is to subscribe to system topics through MQTT to receive real-time notifications of alarms with detailed alarm information. Alarms can also be accessed from the log or via REST API. 
### View Alarms in Dashboard

On EMQX Dashboard, click **Monitoring** -> **Alarms**. Select the **Active** or **History** tab, and you can see the list of currently active alarms and historical alarms.

<img src="./assets/view-alarms.png" alt="view-alarms" style="zoom:50%;" />

### Get Alarms via System Topic

EMQX will publish an MQTT message to system topics `$SYS/brokers/<Node>/alarms/activate` or `$SYS/brokers/<Node>/alarms/deactivate` when an alarm is triggered or cleared. Users can subscribe to the topic to receive alarm notifications.

The payload in the alarm notification message is in JSON format and contains the following fields:

| Field           | Type             | Description                                                  |
| --------------- | ---------------- | ------------------------------------------------------------ |
| `name`          | string           | Alarm name                                                   |
| `details`       | object           | Alarm details                                                |
| `message`       | string           | Human-readable alarm instructions                            |
| `activate_at`   | integer          | A UNIX timestamp in microseconds representing the activation time of the alarm |
| `deactivate_at` | integer / string | A UNIX timestamp in microseconds representing the deactivation time of the alarm. The value of this field for the activated alarm is `infinity`. |
| `activated`     | boolean          | Whether the alarm is activated                               |

Taking the alarm of high system memory usage as an example, you will receive an alarm message like below:

<img src="./assets/alarm_activate_msg.png" alt="alarm massage" style="zoom:67%;" />

One system multifunction will be repeatedly reported. That is, if one alarm on high CPU usage alarm is activated, the system will not generate another alarm of the same type. The generated alarm will be automatically deactivated when the monitored metric returns to normal, or you can manually deactivate the alarm.

Users can view the current alarms and historical alarms on the Dashboard, or use EMQX HTTP API to query and manage alarms.

Users can use the configuration items to customize the alarm feature to meet their business needs. The following configuration items are currently available:

| Configuration item              | Type              | Default Value                    | Description                                      |
|-----------------------------------|-------------------|---------------------------|----------------------------------------------------------------------------|
| sysmon.os.cpu_check_interval      | duration          | 60s                       | Check interval for CPU usage; |
| sysmon.os.cpu_high_watermark      | percent           | 80%                       | The high watermark of the CPU usage, the threshold to activate the alarm; |
| sysmon.os.cpu_low_watermark       | percent           | 60%                       | The low watermark of the CPU usage, the threshold to deactivate the alarm; |
| sysmon.os.mem_check_interval      | duration          | 60s                       | Check interval for memory usage; |
| sysmon.os.sysmem_high_watermark   | percent           | 70%                       | The high watermark of the system memory usage. The alarm will be activated when the total memory occupied reaches this value. |
| sysmon.os.procmem_high_watermark  | percent           | 5%                        | The high watermark of the process memory usage. The alarm will be activated when the memory occupied by a single process reaches this value. |
| sysmonn.vm.process_check_interval | duration          | 30s                       | Check interval for the number of processes; |
| sysmon.vm.process_high_watermark  | percent           | 80%                       | The high watermark of the process occupancy rate; The alarm will be activated when this threshold is reached; Measured as a ratio of the number of created processes/maximum number limit. |
| sysmon.vm.process_low_watermark   | percent           | 60%                       | The low water mark of the process occupancy rate; The alarm will be deactivated when it goes below this threshold; Measured as a ratio of the number of created processes/maximum number limit. |
| sysmonn.vm.long_gc                | disabled/duration | disabled                  | Whether to enable Long GC monitoring |
| sysmon.vm.long_schedule           | disabled/duration | disabled                  | Whether to enable Long Schedule monitoring |
| sysmon.vm.large_heap              | disabled/bytesize | disabled                  | Whether to enable Large Heap monitoring |
| sysmon.vm.busy_port               | boolean           | true                      | Whether to enable Busy Distribution Port monitoring |
| sysmonn.top.num_items             | integer           | 10                        | Number of top processes per monitoring group |
| sysmon.top.sample_interlval       | duration          | 2s                        | Check interval for top processes |
| sysmon.top.max_procs              | integer           | 1000000                   | Stop collecting data when the number of processes in the VM exceeds this value  |
| sysmonn.top.db_hostname           | string            | ""                        | Hostname of the PostgreSQL database |
| sysmonn.top.db_port               | integer           | 5432                      | Port of the PostgreSQL database |
| sysmon.top.db_username            | string            | "system_monitor"          | Username of the PostgreSQL database |
| sysmon.top.db_password            | string            | "system_monitor_password" | Password in the PostgreSQL database |
| sysmon.top.db_name                | string            | "postgres"                | Name of the PostgreSQL database |

{% emqxee %}
EMQX Enterprise will raise an alarm when the license expires in less than 30 days, or if the number of connections exceeds the high watermark. You can adjust the high/low watermark for the number of connections by modifying the following configuration items in `emqx.conf` file. For more information on how to configure settings for the license, see [License](../configuration/license.md).

| Configuration item            | Description                                                  | Default value                                     |
| ----------------------------- | ------------------------------------------------------------ | ------------------------------------------------------------ |
| license.connection_high_watermark_alarm  | The high watermark of the max connections the license supports. The alarm is activated when this threshold is reached. Measured as a ratio of active connections/max connections. | `80%` |
| license.connection_low_watermark_alarm    | The low watermark of the max connections the license supports. The alarm is deactivated when it goes below this threshold. Measured as a ratio of active connections/max connections. | `75%` |

{% endemqxee %}
