# Alarm

EMQX offers a built-in monitoring and alarm functionality for monitoring the internal state changes, such as CPU occupancy, system and process memory occupancy, number of processes, rule engine resource status, cluster partition and healing. EMQX triggers and records these changes when they exceed a threshold or deviate from expectations, and removes them from the list once they are restored. These state changes can be obtained through log and alarm APIs.

When failures occur during message transmission or event processing, detailed information can be logged, and the logging system can also be used to capture alerts through log analysis. 

## Alarm Type

Alarms can be captured from log analysis and system monitoring. Depending on the severance and impacts on the system, alarms can have 3 levels:

- **Error**: Errors caused by user presets. The client can perceive the error and retry.

- **Warning**: Occasional errors, and need to be taken seriously if they occur frequently.

- **Critical**: Irreversible data loss between the client and server, causing communication and business interruption.

The levels are defined from development perspectives and are only for a recommendation. You can define your own alarm levels according to the business needs.

### Alarm from Log 

| **Alarm Log Message**                                        | **Details**                                                  | Level    |
| :----------------------------------------------------------- | :----------------------------------------------------------- | :------- |
| Client login failure                                         | Client ${clientid} (Username: ${username}) login failed for ${Reason} | Error    |
| Client fails to subscribe                                    | Cannot subscribe ${topic} due to ${Reason}                   | Error    |
| When publishing messages, drop the current message if waiting_rel is full | Dropped the qos2 packet ${packetid} due to awaiting_rel is full | Critical |
| Client fails to publish                                      | Cannot publish message to ${topic} due to ${Reason}          | Error    |
| When handling a PUBCOMP message, the message with the Packet ID of "xxx" cannot be found. | The PUBCOMP PacketId ${packetid} is not found                | Critical |
| When handling a PUBREL message, the message with the Packet ID of "xxx" cannot be found. | The PUBREL PacketId ${packetid} is not found                 | Critical |
| When handling a PUBREC message, the message with the Packet ID of "xxx" cannot be found. | The PUBREC PacketId ${packetid} is not found                 | Critical |
| When handling a PUBACK message, the message with the Packet ID of "xxx" cannot be found. | The PUBACK PacketId ${packetid} is not found                 | Critical |
| When handling a PUBREC message, the packet identifier (Packet ID) is found in use. | The PUBREC PacketId ~w is in use.                            | Critical |
| When handling a PUBACK message, the packet identifier (Packet ID) is found in use. | The PUBACK PacketId ~w is in use.                            | Critical |
| Terminate an unknown session.                                | kicked_an_unknown_session ${clientid}                        | Warning  |
| Socket experiences an abnormal exit.                         | socket_error: ${Reason}                                      | Error    |
| Paused due to rat limit                                      | Pause ${time} ms due to rate limit                           | Error    |
| The message is discarded when it exceeds the expiration threshold. | ${Packet} is discarded due to the frame is too large!        | Warning  |
| When a client goes offline and the session is maintained, QoS 0 messages sent to the client while it was offline are discarded. | Dropped qos0 msg: ${msg}                                     | Critical |
| When a client goes offline and the session is maintained, messages are discarded due to queue length exceeds the threshold. | Dropped msg due to mqueue is full: ${msg}                    | Critical |

### System Monitoring Alarm

| **Reason**                                                   | **Alarm**                 | **Threshold**                                                | **Details**                                  | **Level** |
| :----------------------------------------------------------- | :------------------------ | :----------------------------------------------------------- | :------------------------------------------- | :-------- |
| System memory usage is too high                              | high_system_memory_usage  | `os_mon.sysmem_high_watermark = 70%`                         | "System memory usage is higher than ~p%"     | Warning   |
| Single Erlang process memory usage is too high (percentage of system memory usage) | high_process_memory_usage | `os_mon.procmem_high_watermark = 5%`                         | Process memory usage is higher than ~p%      | Warning   |
| CPU usage is too high                                        | high_cpu_usage            | `os_mon.cpu_high_watermark = 80%``os_mon.cpu_low_watermark = 60%` | ~p% cpu usage                                | Warning   |
| Too many processes                                           | too_many_processes        | `vm_mon.process_high_watermark = 80%``vm_mon.process_low_watermark = 60%` | ~p% process usage                            | Warning   |
| License exceeds quota                                        | license_quota             | `license.connection_high_watermark_alarm = 80%``license.connection_low_watermark_alarm = 75%` | License: the number of connections exceeds % | Warning   |
| License expired                                              | license_expiry            | -                                                            | License will be expired at %                 | Critical  |
| Partition occurs at node                                     | partition                 | -                                                            | Partition occurs at node ~s                  | Critical  |
| Resource is disconnected                                     | resource                  | -                                                            | Resource ~s(~s) is down                      | Critical  |
| Connection process congestion                                | conn_congestion           | -                                                            | connection congested                         | Critical  |
| -                                                            | -                         | -                                                            | Unknown alarm                                | -         |

## Alarm Notification

The activation and deactivation of alarms will generate an alarm log and EMQX will publish an MQTT message with the topic of `$SYS/brokers/<Node>/alarms/activate` or `$SYS/brokers/<Node>/alarms/deactivate`. Users can subscribe to the topic to receive alarm notifications.

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

<img src="./assets/alarm_activate_msg.png" alt="alarm massage" style="zoom:50%;" />

One system multifunction will be repeatedly reported. That is, if one alarm on high CPU usage is activated, the system will not generate another alarm of the same type. The generated alarm will be automatically deactivated when the monitored metric returns to normal, or you can manually deactivate the alarm.

## View Alarms in Dashboard

Users can view the current alarms and historical alarms on the Dashboard, or use EMQX HTTP API to query and manage alarms.

On EMQX Dashboard, click **Monitoring** -> **Alarms**. Select the **Active** or **History** tab to view current active alarms and history alarms.

<img src="./assets/view-alarms.png" alt="view-alarms" style="zoom:50%;" />

## Configure Alarm Settings

You can configure the alarm settings through Dashboard or modify the configuration items in the configuration file.

### Configure Settings via Dashboard

On the **Alarms** page, click the **Setting** button and you will be led to the **Monitoring** page. On the **Erlang** tab, you can configure the following items for the system performance of Erlang virtual machine:

<img src="./assets/monitoring-system-ee.png" alt="monitoring-system" style="zoom:35%;" />

**Process limit check interval**: Specify the time interval for checking the periodic process limit. The default value is `30` seconds.

**Process high watermark**: Specify the threshold value of processes that can simultaneously exist at the local node. When the percentage exceeds the specified number, an alarm is raised. The default value is `80` percent.

**Process low watermark**: Specify the threshold value of processes that can simultaneously exist at the local node. When the percentage is lowered to the specified number, an alarm is cleared. The default value is `60` percent.

**Enable Long GC monitoring**: Disabled by default. When enabled, a warning level log `long_gc` is emitted and an MQTT message is published to the system topic `$SYS/sysmon/long_gc`. 

**Enable Long Schedule monitoring**: Enabled by default, which means when the Erlang VM detects a task scheduled for too long, a warning level log `long_schedule` is emitted. You can set the proper time scheduled for a task in the text box. The default value is `240` milliseconds. 

**Enable Large Heap monitoring**: Enabled by default, which means when an Erlang process consumed a large amount of memory for its heap space, a warning level log `large_heap` is emitted, and an MQTT message is published to the system topic `$SYS/sysmon/large_heap`. You can set the limit of space bytesize in the text box. The default value is `32` MB.

**Enable Busy Distribution Port monitoring**: Enabled by default, which means when the Remote Procedure Call (RPC) connection used to communicate with other nodes in the cluster is overloaded, a warning level log `busy_dis_port` log is emitted, and an MQTT message is published to system topic `$SYS/sysmon/busy_dist_port`.

**Enable Busy Port monitory**: Enabled by default, which means when a port is overloaded, a warning level log `busy_port` is emitted, and an MQTT message is published to the system topic `$SYS/sysmon/busy_port`.

After you complete the configurations, click **Save Changes**.

On the **Operating System** tab, you can configure the following items for the system performance:

<img src="./assets/monitoring-operating-system-ee.png" alt="monitoring-operating-system-ee" style="zoom:35%;" />

**The time interval of the periodic CPU check**: Specify the time interval for checking the CPU usage. The default value is `60` seconds.

**CPU high watermark**: Specify the threshold value of how much system CPU can be used. When the percentage exceeds the specified value, a corresponding alarm is raised. The default value is `80` percent.

**CPU low watermark**: Specify the threshold value of how much system CPU can be used. When the percentage is lowered to the specified value, a corresponding alarm is released. The default value is `60` percent.

**Mem check interval**: Enabled by default. You can specify the time interval for periodic memory checks. The default value is `60` seconds.

**SysMem high watermark**: Specify the threshold for how much system memory can be allocated. When the percentage exceeds the specified value, a corresponding alarm is raised. The default value is `70`%.

**ProcMem high watermark**: Specify the threshold for how much system memory can be allocated by one Erlang process. When the percentage exceeds the specified value, a corresponding alarm is raised. The default value is `5`%.

### Configure Settings via Configuration Items

You can use the configuration items to customize the alarm feature to meet your business needs. The following configuration items are currently available:

| Configuration item              | Type              | Default Value                    | Description                                      |
|-----------------------------------|-------------------|---------------------------|----------------------------------------------------------------------------|
| sysmon.os.cpu_check_interval      | duration          | 60s                       | Check interval for CPU usage. |
| sysmon.os.cpu_high_watermark      | percent           | 80%                       | The high watermark of the CPU usage, the threshold to activate the alarm. |
| sysmon.os.cpu_low_watermark       | percent           | 60%                       | The low watermark of the CPU usage, the threshold to deactivate the alarm. |
| sysmon.os.mem_check_interval      | duration          | 60s                       | Check interval for memory usage. |
| sysmon.os.sysmem_high_watermark   | percent           | 70%                       | The high watermark of the system memory usage. The alarm will be activated when the total memory occupied reaches this value. |
| sysmon.os.procmem_high_watermark  | percent           | 5%                        | The high watermark of the process memory usage. The alarm will be activated when the memory occupied by a single process reaches this value. |
| sysmonn.vm.process_check_interval | duration          | 30s                       | Check interval for the number of processes. |
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
EMQX Enterprise will raise an alarm when the license expires in less than 30 days, or if the number of connections exceeds the high watermark. You can adjust the high/low watermark for the number of connections.

| Configuration item            | Type     | Default value | Description                                                  |
| ----------------------------- | -------- | ----------- | ------------------------------------------------------------ |
| license.connection_high_watermark_alarm  | percent  | 80%         | The high watermark of the max connections the license supports. The alarm is activated when this threshold is reached. Measured as a ratio of active connections/max connections. |
| license.connection_low_watermark_alarm    | percent  | 75%         | The low watermark of the max connections the license supports. The alarm is deactivated when it goes below this threshold. Measured as a ratio of active connections/max connections. |

{% endemqxee %}
