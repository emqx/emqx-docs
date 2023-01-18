# Log

Logs provide a reliable source of information for troubleshooting and system performance optimization. You can find the record about the access, operating or network issues from EMQX logs. 

To minimize the impact of logs on system operation, for example, when the log data is too much or the log writing is too slow, EMQX activates the overload protection mechanism by default to better serve our users.

## Log level

EMQX log has 8 levels ([RFC 5424](https://www.ietf.org/rfc/rfc5424.txt)), with warning as the default level, from low to high these 8 levels are:

```bash
debug < info < notice < warning < error < critical < alert < emergency
```

You can change the log configuration with either of the following methods:

1. **[Recommended]** Configure with EMQX Dashboard, click **Configuration** -> **Log** on the left navigation tree to configure. Save the changes and they will take effect immediately without needing to restart the node.
1. Modify the configuration items under `log` in `emqx.conf`, which will take effect after the node restarts.

EMQX supports outputting logs to the console or log file. These are 2 independent features, you can choose the output method as needed or keep both.

## Output logs to the console

After the EMQX is started with command line `./bin/emqx console` or `/bin/emqx/foreground`, EMQX will output the logs (`warning` level) to the console and disable the log file output option. You can use the console logs for development debugging. Its configuration path is `log.console_handler`, and all configuration items are described as follows:

| Configuration item  | Type | Description                                                  |
| ------------------- | ---- | ------------------------------------------------------------ |
| log.console_handler |      | Console log handler                                          |
| ⌞_ enable           |      | Whether to enable this log handler.                          |
| ⌞_ level            |      | Log level, default:`warning`                                 |
| ⌞_ time_offset      |      | Time offset to be used when formatting the timestamp：<br/><br />Options: <br />   ##   - system: the time offset used by the local system<br/>    ##   - utc: the UTC time offset<br/>    ##   - +-[hh]:[mm]: user specified time offset, such as "-02:00" or "+00:00" |
| ⌞_ chars_limit      |      | Maximum length of a single log message. If message length exceeds this value, the log message will be truncated; Default: 100 |
| ⌞_ formatter        |      | Log format; `text` for free text, and `json` for structured logging. |
| ⌞_ single_line      |      | Print logs in a single line if set to `true`. Otherwise, log messages may span multiple lines. |
| ⌞_ sync_mode_qlen   |      | Threshold for overload protection; Default: 100<br />ThrIf the number of buffered log events is lower than this value, all log events are handled asynchronously. This means that after the client process calls the Logger API to send a log function, it need not wait for a response from the handler but can continue with other operations. Therefore the working procedure will not be affected. <br/> <br/>If the message queue grows larger than this value, the handler starts handling log events synchronously instead, meaning that after the client process sends the event, it must wait for a response. When the handler reduces the message queue to a level below the `sync_mode_qlen` threshold, it will switch back to the asynchronous mode. |
| ⌞_ drop_mode_qlen   |      | When the number of buffered log events is larger than this value, the new log events are dropped. When drop mode is activated or deactivated, a message is printed in the logs. |
| ⌞_ flush_qlen       |      | If the number of buffered log events grows larger than this threshold, a flush (delete) operation occurs. The handler discards the buffered log messages without logging. |
| ⌞_ overload_kill    |      | Overload kill feature                                        |
| ⌞_ _ enable         |      | Whether to enable log handler overload kill feature.         |
| ⌞_ _ mem_size       |      | Maximum memory size that the log handler process can use.    |
| ⌞_ _ qlen           |      | Maximum allowed queue length.                                |
| ⌞_ _ restart_after  |      | If the handler is terminated, it restarts automatically after a delay specified in milliseconds. The value `infinity` prevents restarts. |
| ⌞_  burst_limit     |      | Log burst control feature                                    |
| ⌞_ _ enable         |      | Whether to enable log burst control feature.                 |
| ⌞_ _ max_count      |      | Maximum number of log events to handle within a `window_time` interval. After the limit is reached, successive events are dropped until the end of the `window_time`. |
| ⌞_ _ window_time    |      | Window time                                                  |
| ⌞_ max_depth        |      | Maximum depth for Erlang term log formatting and Erlang process message queue inspection. |

Code example:

```
log.console_handler {
    enable = false
    level = warning
    time_offset = system
    chars_limit = unlimited
    formatter = text
    single_line = true
    sync_mode_qlen = 100
    drop_mode_qlen = 3000
    flush_qlen = 8000
    overload_kill {
      enable = true
      mem_size = 30MB
      qlen = 20000
      restart_after = 5s
     }
    burst_limit {
      enable = true
      max_count = 10000
      window_time  =  1s
     }   
    max_depth  =  100
   }
```

## File log handler

The default log file directory of EMQX is `./log` (for zip package installation) or `/var/log/emqx` (for RPM or DEB package installation). The configuration of the file  log handler is mostly the same as that of the console handler, except for the additional file write control configuration, which is listed below.

| Configuration item  | Type | Description                                                  |
| ------------------- | ---- | ------------------------------------------------------------ |
| log.console_handler |      | File log output handler                                      |
| ⌞_ enable           |      | Whether to enable this file logging handler                  |
| ⌞_ level            |      | Log level, default:`warning`                                 |
| ⌞_ file             |      | File path                                                    |
| ⌞_ rotation         |      | Log rotation                                                 |
| ⌞_ _ enable         |      | Whether to enable log rotation feature                       |
| ⌞_ _ count          |      | Maximum number of log files to rotate                        |
| ⌞_ _ max_size       |      | This parameter controls log file rotation. `infinity` means the log file will grow indefinitely, otherwise the log file will be rotated when it reaches `max_size` (in bytes); used together with `rotation.count` |

Code example:

```
 log.file_handlers.default {  
      enable = true
      level = warning
      file = "log/emqx.log"     
      rotation {            
        enable = true
        count = 10
        }
      max_size = 50MB 
 }
```

When file logging is enabled (log.to = file or both), the following files will appear in the log directory:

- **emqx.log.N:** L og file prefixed with emqx.log, that contains all the log messages of EMQX Broker, such as `emqx.log.1`,` emqx.log.2` ...
- **emqx.log.siz and emqx.log.idx:** System files used to record log rotation information. **Do not change manually**. 
- **run_erl.log:** System file used to record startup information when starting EMQX Broker in the background with `emqx start`.
- **erlang.log.N:** Log file prefixed with erlang.log, which is a copy file of the console log when EMQX Broker is started in the background with `emqx start`, such as `erlang.log.1`,` erlang.log.2` ...

## Output log file for log level

If you want to write logs greater than or equal to a certain level to a separate file, you can configure `emqx.conf`  as :

```
log.file_handlers.my_info_log {  
      enable = true
      level = info
      file = "log/info.log"     
      rotation {        
        enable = true
        count = 10
        }
      max_size = 50MB      
 }
```

## Log format

The format of the log message (with different fields separated by spaces) is as follows:

```
**date time level client_info module_info msg**
```

where,

- **date:** Local data. The format is: YYYY-MM-DD
- **time:** Local time, accurate to milliseconds. The format is: hh:mm:ss.ms
- **level:** Log level, wrapped in brackets. The format is:[Level]
- **client_info (optional):** Only exists if this log message is related to a client. The format is: ClientId@Peername or ClientId or Peername
- **msg:** Log message content. The format is arbitrary and can contain spaces.

### Log message example 1:

```bash
2022-06-30T16:07:47.689512+08:00 [debug] clientid: test, line: 792, mfa: emqx_connection:handle_incoming/2, msg: mqtt_packet_received, packet: PINGREQ(Q0, R0, D0), payload: [], peername: 127.0.0.1:64391, tag: MQTT
```

The fields in this log message are:

- **datetime:** `2022-06-30T15:59:19.438914+08:00`
- **level:** `[debug]`
- **flat log-content:** `clientid: test, line: 792, mfa: emqx_connection:handle_incoming/2, msg: mqtt_packet_received, packet: PINGREQ(Q0, R0, D0), payload: [], peername: 127.0.0.1:64391, tag: MQTT`

This log indicates that EMQX received a `PINGREQ(Q0,R0,D0)` packet at `2022-06-30T16:07:47.689512+08:00` with clientid `test`. The IP of the client is `127.0.0.1:64391`.

### Log message example 2:

```bash
2022-06-30T16:25:32.446873+08:00 [debug] line: 150, mfa: emqx_retainer_mnesia:store_retained/2, msg: message_retained, topic: $SYS/brokers/emqx@127.0.0.1/sysdescr
```

The fields in this log message are:

- **date-time:** `2022-06-30T16:25:32.446873+08:00`
- **level:** `[debug]`
- **flat log-content:** `line: 150, mfa: emqx_retainer_mnesia:store_retained/2, msg: message_retained, topic: $SYS/brokers/emqx@127.0.0.1/sysdescr`

